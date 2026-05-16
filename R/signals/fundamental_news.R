# R/signals/fundamental_news.R
# Component A — Confirmed Fundamental News Score.
# Four sub-signals; cross-sectional z-scores averaged. Multi-signal
# confirmation: require 3-of-4 positive components for eligibility.

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
})

compute_fundamental_news <- function(news_dt,
                                     prices_dt,    # kept for API symmetry; unused in v1
                                     rebalance_dates,
                                     market_ret_dt = NULL) {

  setDT(news_dt)
  news_dt[, date := as.Date(date)]
  setkey(news_dt, ticker, date)

  # 1. Standardized Unexpected Earnings (SUE).
  if (all(c("is_eps", "best_eps") %in% names(news_dt))) {
    news_dt[, eps_surprise := is_eps - best_eps]
    news_dt[, sue_sigma := zoo::rollapplyr(
      eps_surprise, width = 8,
      FUN = function(x) stats::sd(x, na.rm = TRUE),
      partial = TRUE, fill = NA_real_), by = ticker]
    news_dt[, sue := eps_surprise / sue_sigma]
    news_dt[!is.finite(sue), sue := NA_real_]
  } else {
    news_dt[, sue := NA_real_]
  }

  # 2. Revenue surprise.
  if (all(c("sales_rev_turn", "best_sales") %in% names(news_dt))) {
    news_dt[, rev_surprise := (sales_rev_turn - best_sales) /
                              pmax(abs(best_sales), 1e-6)]
    news_dt[!is.finite(rev_surprise), rev_surprise := NA_real_]
  } else {
    news_dt[, rev_surprise := NA_real_]
  }

  # 3. Margin expansion vs trailing 3Y (12-quarter) trend.
  if ("oper_margin" %in% names(news_dt)) {
    news_dt[, oper_margin_trend := zoo::rollapplyr(
      oper_margin, width = 12, FUN = mean,
      partial = TRUE, fill = NA_real_, na.rm = TRUE),
      by = ticker]
    news_dt[, margin_expansion := oper_margin - oper_margin_trend]
  } else {
    news_dt[, margin_expansion := NA_real_]
  }

  # 4. Guidance / revision signal.
  if ("best_eps_revision_ratio_1m" %in% names(news_dt)) {
    news_dt[, guidance := best_eps_revision_ratio_1m]
  } else {
    news_dt[, guidance := NA_real_]
  }

  components <- c("sue", "rev_surprise", "margin_expansion", "guidance")

  # Cross-section z-scores at each rebalance date. xs_zscore() lives in
  # R/data_loader.R and main.R is responsible for sourcing it first.
  out_rows <- vector("list", length(rebalance_dates))
  for (i in seq_along(rebalance_dates)) {
    rd <- rebalance_dates[i]
    snap <- news_dt[date <= rd, .SD[which.max(date)], by = ticker]
    if (nrow(snap) == 0) next
    for (c in components) {
      snap[, paste0(c, "_z") := xs_zscore(get(c))]
    }
    snap[, news_score := rowMeans(.SD, na.rm = TRUE),
         .SDcols = paste0(components, "_z")]
    snap[, n_positive := rowSums(.SD > 0, na.rm = TRUE),
         .SDcols = components]
    snap[, news_eligible := n_positive >= 3]   # 3-of-4 majority confirmation
    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, news_score,
                              n_positive, news_eligible)]
  }

  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
