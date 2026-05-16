# R/signals/passive_intensity.R
# Component B — Passive Ownership Intensity Score.
# Level + sector-relative z-score + 12-month growth.

suppressPackageStartupMessages({
  library(data.table)
})

compute_passive_intensity <- function(ownership_dt,
                                      universe_dt,
                                      rebalance_dates,
                                      lag_days = 45) {

  setDT(ownership_dt); setDT(universe_dt)
  ownership_dt[, date := as.Date(date)]
  ownership_dt[, available_date := date + lag_days]
  setkey(ownership_dt, ticker, available_date)

  # Sector map (most recent observation per ticker is good enough for
  # sector-relative z-scoring at any rebalance date).
  sectors <- unique(universe_dt[, .(ticker, gics_sector_name)])
  sectors <- sectors[!duplicated(ticker)]

  # Compute 12-month change in passive ownership per ticker.
  setkey(ownership_dt, ticker, date)
  ownership_dt[, passive_yoy := passively_held_pct_out -
                                shift(passively_held_pct_out, 4L),
               by = ticker]

  # xs_zscore() comes from R/data_loader.R; sourced by main.R.
  out_rows <- vector("list", length(rebalance_dates))

  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    snap <- ownership_dt[available_date <= rd,
                         .SD[which.max(available_date)],
                         by = ticker]
    if (nrow(snap) == 0) next
    snap <- merge(snap, sectors, by = "ticker", all.x = TRUE)

    # Sector-relative z-score on level.
    snap[, passive_z_in_sector :=
            xs_zscore(passively_held_pct_out),
         by = gics_sector_name]
    # Cross-sectional z-score on YoY change.
    snap[, passive_yoy_z := xs_zscore(passive_yoy)]

    # Composite passive intensity: equal-weight of level (sector-relative)
    # and YoY z-score.
    snap[, passive_intensity := rowMeans(
      cbind(passive_z_in_sector, passive_yoy_z), na.rm = TRUE)]

    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, passive_intensity,
                              passively_held_pct_out, passive_yoy,
                              gics_sector_name)]
  }

  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
