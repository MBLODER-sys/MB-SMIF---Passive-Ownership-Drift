# R/signals/value.R
# Value screen — risk control, NOT alpha source.
# Combines earnings yield, book-to-market, free-cash-flow yield.

suppressPackageStartupMessages({
  library(data.table)
})

compute_value <- function(fundamentals_dt, rebalance_dates) {

  setDT(fundamentals_dt)
  fundamentals_dt[, date := as.Date(date)]

  # Ensure every column the screen references exists.
  needed_cols <- c("earn_yld", "px_to_book_ratio", "fcf_yield")
  for (col in needed_cols) {
    if (!col %in% names(fundamentals_dt)) {
      fundamentals_dt[, (col) := NA_real_]
    }
  }

  fundamentals_dt[, book_to_market := 1 / pmax(px_to_book_ratio, 1e-6)]

  out_rows <- vector("list", length(rebalance_dates))

  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    snap <- fundamentals_dt[date <= rd - 45, .SD[which.max(date)], by = ticker]
    if (nrow(snap) == 0) next

    snap[, earn_yld_z := xs_zscore(earn_yld)]
    snap[, btm_z      := xs_zscore(book_to_market)]
    snap[, fcf_yld_z  := xs_zscore(fcf_yield)]
    snap[, value_score := rowMeans(
      cbind(earn_yld_z, btm_z, fcf_yld_z), na.rm = TRUE)]
    snap[, value_decile := NA_integer_]
    valid <- !is.na(snap$value_score)
    if (sum(valid) >= 10) {
      snap[valid,
           value_decile := as.integer(ceiling(
             frank(value_score, ties.method = "average") /
             (.N / 10)))]
      snap[, value_decile := pmin(pmax(value_decile, 1L), 10L)]
    }
    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, value_score, value_decile)]
  }

  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
