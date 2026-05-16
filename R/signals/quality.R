# R/signals/quality.R
# Quality screen — risk control, NOT alpha source.
# Combines ROE, gross profitability, leverage (inverted), earnings stability.

suppressPackageStartupMessages({
  library(data.table)
})

compute_quality <- function(fundamentals_dt, rebalance_dates) {

  setDT(fundamentals_dt)
  fundamentals_dt[, date := as.Date(date)]

  # xs_zscore() comes from R/data_loader.R; sourced by main.R.

  # Compute gross profitability = gross_profit / total_assets.
  if (all(c("gross_profit", "bs_tot_asset") %in% names(fundamentals_dt))) {
    fundamentals_dt[, gross_profitability :=
                     gross_profit / pmax(bs_tot_asset, 1e-6)]
  } else {
    fundamentals_dt[, gross_profitability := NA_real_]
  }

  out_rows <- vector("list", length(rebalance_dates))

  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    snap <- fundamentals_dt[date <= rd - 45, .SD[which.max(date)], by = ticker]
    if (nrow(snap) == 0) next

    snap[, roe_z   := xs_zscore(return_com_eqy)]
    snap[, prof_z  := xs_zscore(gross_profitability)]
    snap[, lev_z   := -xs_zscore(tot_debt_to_tot_eqy)]   # inverted: lower leverage = higher z
    snap[, stab_z  := -xs_zscore(earn_growth_ttm_stdev)] # inverted: lower stdev = higher z
    snap[, quality_score := rowMeans(
      cbind(roe_z, prof_z, lev_z, stab_z), na.rm = TRUE)]
    # Decile assignment via rank — robust to duplicate quantile breaks.
    snap[, quality_decile := NA_integer_]
    valid <- !is.na(snap$quality_score)
    if (sum(valid) >= 10) {
      snap[valid,
           quality_decile := as.integer(ceiling(
             frank(quality_score, ties.method = "average") /
             (.N / 10)))]
      snap[, quality_decile := pmin(pmax(quality_decile, 1L), 10L)]
    }
    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, quality_score, quality_decile)]
  }

  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
