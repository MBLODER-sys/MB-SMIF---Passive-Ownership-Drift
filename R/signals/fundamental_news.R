# R/signals/fundamental_news.R
# Component A — Confirmed Fundamental News Score.
# Five sub-signals; cross-sectional z-scores averaged; requires 3-of-5
# positive components for eligibility (multi-signal confirmation).

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
})

compute_fundamental_news <- function(news_dt,
                                     prices_dt,
                                     rebalance_dates,
                                     market_ret_dt = NULL) {

  setDT(news_dt); setDT(prices_dt)
  news_dt[, date := as.Date(date)]

  # ---- 1. SUE ------------------------------------------------------------
  # actual EPS - consensus EPS, normalized by stock-specific stdev of past
  # surprises (rolling 8 quarters).
  if (all(c("is_eps", "best_eps") %in% names(news_dt))) {
    news_dt[, eps_surprise := is_eps - best_eps]
    setkey(news_dt, ticker, date)
    news_dt[, sue_sigma := zoo::rollapplyr(eps_surprise,
                                           width = 8,
                                           FUN = function(x) stats::sd(x, na.rm = TRUE),
                                           partial = TRUE,
                                           fill = NA_real_),
            by = ticker]
    # Guard against sigma == 0 (single-observation tickers etc.) producing Inf.
    news_dt[, sue := eps_surprise / sue_sigma]
    news_dt[!is.finite(sue), sue := NA_real_]
  } else {
    news_dt[, sue := NA_real_]
  }

  # ---- 2. Revenue surprise ----------------------------------------------
  if (all(c("sales_rev_turn", "best_sales") %in% names(news_dt))) {
    news_dt[, rev_surprise := (sales_rev_turn - best_sales) /
                              pmax(abs(best_sales), 1e-6)]
  } else {
    news_dt[, rev_surprise := NA_real_]
  }

  # ---- 3. Margin expansion vs trailing 3Y trend -------------------------
  if ("oper_margin" %in% names(news_dt)) {
    setkey(news_dt, ticker, date)
    news_dt[, oper_margin_trend := zoo::rollapplyr(oper_margin,
                                                   width = 12,
                                                   FUN = mean,
                                                   partial = TRUE,
                                                   fill = NA_real_,
                                                   na.rm = TRUE),
            by = ticker]
    news_dt[, margin_expansion := oper_margin - oper_margin_trend]
  } else {
    news_dt[, margin_expansion := NA_real_]
  }

  # ---- 4. Guidance signal -----------------------------------------------
  if ("best_eps_revision_ratio_1m" %in% names(news_dt)) {
    news_dt[, guidance := best_eps_revision_ratio_1m]
  } else {
    news_dt[, guidance := NA_real_]
  }

  # ---- 5. Price confirmation: 1-3 day CAR around announcement -----------
  # Use the announcement date in news_dt; subtract market return over the
  # same window. market_ret_dt should be a data.table with columns
  # (date, spx_ret) — typically derived from the SPX series in the macro
  # extraction.
  if ("latest_announcement_dt" %in% names(news_dt) && !is.null(market_ret_dt)) {
    spx_ret <- copy(market_ret_dt)
    if (!"spx_ret" %in% names(spx_ret)) {
      setnames(spx_ret, intersect(c("ret", "px_last"), names(spx_ret))[1],
               "spx_ret")
    }
    spx_ret[, date := as.Date(date)]
    pr <- prices_dt[, .(ticker, date = as.Date(date), ret)]
    setkey(pr, ticker, date)

    car_rows <- vector("list", nrow(news_dt))
    for (i in seq_len(nrow(news_dt))) {
      tk <- news_dt$ticker[i]
      ad <- news_dt$latest_announcement_dt[i]
      if (is.na(ad)) next
      win <- pr[ticker == tk & date >= ad & date <= ad + 5]
      if (nrow(win) < 2) next
      stock_cum <- prod(1 + win$ret[1:min(3, nrow(win))]) - 1
      spx_win <- spx_ret[date >= ad & date <= ad + 5]
      spx_cum <- if (nrow(spx_win) >= 2)
        prod(1 + spx_win$spx_ret[1:min(3, nrow(spx_win))]) - 1 else 0
      car_rows[[i]] <- data.table(rn = i, car = stock_cum - spx_cum)
    }
    car_dt <- rbindlist(car_rows)
    news_dt[, price_confirmation := NA_real_]
    if (nrow(car_dt) > 0) {
      news_dt[car_dt$rn, price_confirmation := car_dt$car]
    }
  } else {
    # No market series provided — degrade gracefully to NA so this component
    # is dropped from the news-score average.
    news_dt[, price_confirmation := NA_real_]
  }

  # ---- Cross-section z-scores per rebalance date ------------------------
  # xs_zscore() comes from R/data_loader.R; main.R is responsible for
  # sourcing data_loader.R before calling this function.
  components <- c("sue", "rev_surprise", "margin_expansion",
                  "guidance", "price_confirmation")
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
    snap[, news_eligible := n_positive >= 3]
    snap[, rebalance_date := rd]
    out_rows[[i]] <- snap[, .(rebalance_date, ticker, news_score,
                              n_positive, news_eligible)]
  }

  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
