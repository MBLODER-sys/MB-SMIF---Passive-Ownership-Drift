# R/signals/passive_intensity.R
# Component B — Passive Ownership Intensity Score.
#
# This signal has two implementations, selected automatically based on
# what data is available in the loaded ownership panel:
#
#   DIRECT path (preferred):
#     Requires PASSIVELY_HELD_PCT_OUT (premium Bloomberg ownership field).
#     Score = z_in_sector(passive_pct_level) + z(passive_pct_YoY_change)
#
#   PROXY path (used when the direct field is missing):
#     Score = z_in_sector(log(market_cap))
#           + z_in_sector(- 90d avg [PX_VOLUME / EQY_FLOAT])
#
#     - SIZE proxy: within-sector, large names attract more passive flows
#       because cap-weighted index inclusion is mechanical and large
#       names are eligible for more index products. The within-sector
#       z-score removes the strategy becoming a sector bet.
#       (Methodological precedent for using size as the running variable
#       in identifying passive ownership: Appel, Gormley & Keim (2016,
#       JFE) "Passive Investors, Not Passive Owners" — their Russell 1000
#       / 2000 IV uses end-of-May market cap as the assignment variable.)
#
#     - FLOAT-ADJUSTED INVERSE TURNOVER proxy: low PX_VOLUME / EQY_FLOAT
#       indicates a buy-and-hold holder base on the freely-tradeable
#       share base. Float-adjusting is important because raw turnover
#       (volume / shares outstanding) misclassifies controlled-ownership
#       stocks (founder stakes, post-IPO lockups, Berkshire-type
#       structures) as "passive-heavy" when they're actually just
#       illiquid in raw terms. Float-adjusted turnover measures trading
#       activity on the shares that can actually trade — which is what
#       the slow-price-setter channel is really about.
#       Falls back to EQY_SH_OUT only if EQY_FLOAT is unavailable.

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
          "size + float-adjusted-inverse-turnover (within sector). ",
          "PASSIVELY_HELD_PCT_OUT not available on this Bloomberg session.")
  .compute_passive_proxy(universe_dt, prices_dt, ownership_dt, rebalance_dates)
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
.compute_passive_proxy <- function(universe_dt, prices_dt, ownership_dt,
                                   rebalance_dates) {

  prices_dt <- copy(prices_dt)
  prices_dt[, date := as.Date(date)]
  setkey(prices_dt, ticker, date)

  ownership_dt <- copy(ownership_dt)
  if ("date" %in% names(ownership_dt)) {
    ownership_dt[, date := as.Date(date)]
  }

  has_float <- "eqy_float" %in% names(ownership_dt) &&
               sum(!is.na(ownership_dt$eqy_float)) > 100
  if (!has_float) {
    warning("[passive_intensity] EQY_FLOAT unavailable; falling back to ",
            "EQY_SH_OUT for the turnover denominator. Float-adjusted ",
            "turnover is preferred (removes founder/lockup misclassifications).",
            call. = FALSE)
  }

  sectors <- unique(universe_dt[, .(ticker, gics_sector_name)])
  sectors <- sectors[!duplicated(ticker)]

  out_rows <- vector("list", length(rebalance_dates))
  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    window_start <- rd - 90

    win <- prices_dt[date >= window_start & date <= rd]
    if (nrow(win) == 0) next

    if (has_float) {
      # Most recent EQY_FLOAT per ticker as of rd. Float changes slowly so a
      # single snapshot for the 90-day window is fine.
      float_snap <- ownership_dt[date <= rd & !is.na(eqy_float),
                                  .SD[which.max(date)], by = ticker]
      float_snap <- float_snap[, .(ticker, eqy_float)]
      win <- merge(win, float_snap, by = "ticker", all.x = TRUE)
      # If a ticker has no float observation (delisted GUIDs etc.), fall
      # back to shares outstanding for that ticker only.
      win[is.na(eqy_float), eqy_float := eqy_sh_out]
      win[, daily_turnover := px_volume / pmax(eqy_float * 1e6, 1)]
    } else {
      win[, daily_turnover := px_volume / pmax(eqy_sh_out * 1e6, 1)]
    }
    win[!is.finite(daily_turnover), daily_turnover := NA_real_]

    metrics <- win[, .(
      avg_mkt_cap  = mean(cur_mkt_cap,    na.rm = TRUE),
      avg_turnover = mean(daily_turnover, na.rm = TRUE)
    ), by = ticker]
    metrics[!is.finite(avg_mkt_cap),  avg_mkt_cap  := NA_real_]
    metrics[!is.finite(avg_turnover), avg_turnover := NA_real_]

    snap <- merge(metrics, sectors, by = "ticker", all.x = TRUE)
    snap[, log_mkt_cap := log(pmax(avg_mkt_cap, 1))]

    snap[, size_z         :=  xs_zscore(log_mkt_cap), by = gics_sector_name]
    snap[, inv_turnover_z := -xs_zscore(avg_turnover), by = gics_sector_name]

    snap[, passive_intensity := rowMeans(
      cbind(size_z, inv_turnover_z), na.rm = TRUE)]

    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, passive_intensity,
                              avg_mkt_cap, avg_turnover, gics_sector_name)]
  }
  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}

