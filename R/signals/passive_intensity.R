# R/signals/passive_intensity.R
# Component B — Passive Ownership Intensity Score.
#
# This signal has two implementations, selected automatically based on what
# data is available in the loaded ownership panel:
#
#   DIRECT path (preferred):
#     Requires PASSIVELY_HELD_PCT_OUT (premium Bloomberg ownership field).
#     Score = z_in_sector(passive_pct_level) + z(passive_pct_YoY_change)
#
#   PROXY path (used when the direct field is missing):
#     Score = z_in_sector(log(market_cap)) - z_in_sector(90d_turnover)
#
#     - Size proxy: within-sector, large names attract more passive flows
#       both mechanically (cap-weighted index inclusion) and via more
#       index products being benchmarked to them.
#     - Inverse-turnover proxy: low average volume / shares-outstanding
#       indicates a buy-and-hold holder base — the marginal-price-setter
#       channel the strategy is designed to exploit, regardless of whether
#       the holders are technically "passive" or just slow-moving active.
#
# The proxy is consistent with the academic literature on passive ownership
# proxies (Ben-David/Franzoni/Moussawi 2018, Coles/Heath/Ringgenberg 2022)
# when direct holdings data is not available.

suppressPackageStartupMessages({
  library(data.table)
})

compute_passive_intensity <- function(ownership_dt,
                                      universe_dt,
                                      rebalance_dates,
                                      prices_dt = NULL,
                                      lag_days = 45) {

  setDT(ownership_dt); setDT(universe_dt)

  has_direct <- "passively_held_pct_out" %in% names(ownership_dt) &&
                sum(!is.na(ownership_dt$passively_held_pct_out)) > 100

  if (has_direct) {
    message("[passive_intensity] using DIRECT path: PASSIVELY_HELD_PCT_OUT")
    return(.compute_passive_direct(ownership_dt, universe_dt,
                                   rebalance_dates, lag_days))
  }

  if (is.null(prices_dt)) {
    stop("[passive_intensity] PASSIVELY_HELD_PCT_OUT not available AND ",
         "prices_dt not provided — cannot compute proxy. Pass the prices ",
         "panel to enable the proxy path.", call. = FALSE)
  }
  message("[passive_intensity] using PROXY path: ",
          "size + inverse-turnover (within sector). ",
          "PASSIVELY_HELD_PCT_OUT not available on this Bloomberg session.")
  .compute_passive_proxy(universe_dt, prices_dt, rebalance_dates)
}


# ---- DIRECT path ----------------------------------------------------------
.compute_passive_direct <- function(ownership_dt, universe_dt,
                                    rebalance_dates, lag_days) {
  ownership_dt <- copy(ownership_dt)
  ownership_dt[, date := as.Date(date)]
  ownership_dt[, available_date := date + lag_days]
  setkey(ownership_dt, ticker, date)
  ownership_dt[, passive_yoy := passively_held_pct_out -
                                shift(passively_held_pct_out, 4L),
               by = ticker]

  sectors <- unique(universe_dt[, .(ticker, gics_sector_name)])
  sectors <- sectors[!duplicated(ticker)]

  out_rows <- vector("list", length(rebalance_dates))
  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    snap <- ownership_dt[available_date <= rd,
                         .SD[which.max(available_date)],
                         by = ticker]
    if (nrow(snap) == 0) next
    snap <- merge(snap, sectors, by = "ticker", all.x = TRUE)
    snap[, passive_z_in_sector := xs_zscore(passively_held_pct_out),
         by = gics_sector_name]
    snap[, passive_yoy_z := xs_zscore(passive_yoy)]
    snap[, passive_intensity := rowMeans(
      cbind(passive_z_in_sector, passive_yoy_z), na.rm = TRUE)]
    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, passive_intensity,
                              passively_held_pct_out, passive_yoy,
                              gics_sector_name)]
  }
  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}


# ---- PROXY path -----------------------------------------------------------
.compute_passive_proxy <- function(universe_dt, prices_dt, rebalance_dates) {
  prices_dt <- copy(prices_dt)
  prices_dt[, date := as.Date(date)]
  setkey(prices_dt, ticker, date)

  sectors <- unique(universe_dt[, .(ticker, gics_sector_name)])
  sectors <- sectors[!duplicated(ticker)]

  out_rows <- vector("list", length(rebalance_dates))
  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    window_start <- rd - 90

    # 90-day average market cap and turnover per ticker.
    win <- prices_dt[date >= window_start & date <= rd]
    if (nrow(win) == 0) next

    metrics <- win[, .(
      avg_mkt_cap  = mean(cur_mkt_cap, na.rm = TRUE),
      avg_turnover = mean(px_volume /
                          pmax(eqy_sh_out * 1e6, 1),
                          na.rm = TRUE)
    ), by = ticker]
    metrics[!is.finite(avg_mkt_cap),  avg_mkt_cap  := NA_real_]
    metrics[!is.finite(avg_turnover), avg_turnover := NA_real_]

    snap <- merge(metrics, sectors, by = "ticker", all.x = TRUE)
    snap[, log_mkt_cap := log(pmax(avg_mkt_cap, 1))]

    # Within-sector z-scores.
    snap[, size_z          :=  xs_zscore(log_mkt_cap),
         by = gics_sector_name]
    snap[, inv_turnover_z  := -xs_zscore(avg_turnover),
         by = gics_sector_name]

    snap[, passive_intensity := rowMeans(
      cbind(size_z, inv_turnover_z), na.rm = TRUE)]

    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, passive_intensity,
                              avg_mkt_cap, avg_turnover, gics_sector_name)]
  }
  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
