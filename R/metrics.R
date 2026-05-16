# R/metrics.R
# Performance and risk metrics. Returns one row per portfolio.
# Self-contained for the simple metrics; only the genuinely non-trivial
# ones (max drawdown, CVaR, Ulcer) borrow from PerformanceAnalytics.

suppressPackageStartupMessages({
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
})

compute_metrics <- function(returns_dt,
                            label,
                            benchmark_dt = NULL,
                            risk_free_dt = NULL,
                            periods_per_year = 252) {

  setDT(returns_dt)
  returns_dt <- copy(returns_dt)
  returns_dt[, date := as.Date(date)]
  returns_dt[is.na(ret) | !is.finite(ret), ret := 0]

  if (nrow(returns_dt) == 0) return(data.table(strategy = label))

  rets <- xts(returns_dt$ret, order.by = returns_dt$date)
  colnames(rets) <- label
  r <- as.numeric(rets)

  # Risk-free aligned to returns dates; default 0 if missing or NA.
  rf <- rep(0, length(r))
  if (!is.null(risk_free_dt) && nrow(risk_free_dt) > 0) {
    setDT(risk_free_dt)
    rf_lookup <- setNames(risk_free_dt$daily_rate,
                          as.character(risk_free_dt$date))
    rf_aligned <- rf_lookup[as.character(returns_dt$date)]
    rf_aligned[is.na(rf_aligned) | !is.finite(rf_aligned)] <- 0
    rf <- unname(rf_aligned)
  }

  # Benchmark aligned to returns dates; default 0 if missing.
  has_bench <- !is.null(benchmark_dt) && nrow(benchmark_dt) > 0
  bench <- rep(0, length(r))
  if (has_bench) {
    setDT(benchmark_dt)
    benchmark_dt <- copy(benchmark_dt)
    benchmark_dt[, date := as.Date(date)]
    b_lookup <- setNames(benchmark_dt$ret, as.character(benchmark_dt$date))
    b_aligned <- b_lookup[as.character(returns_dt$date)]
    b_aligned[is.na(b_aligned) | !is.finite(b_aligned)] <- 0
    bench <- unname(b_aligned)
  }

  # Absolute and risk metrics (hand-rolled; robust to NA / 0-variance).
  total_return <- prod(1 + r) - 1
  years        <- as.numeric(diff(range(returns_dt$date))) / 365.25
  cagr         <- (1 + total_return)^(1 / max(years, 1e-9)) - 1

  ann_vol  <- stats::sd(r) * sqrt(periods_per_year)
  ann_mean <- mean(r) * periods_per_year
  ann_rf   <- mean(rf) * periods_per_year

  sharpe   <- if (ann_vol > 0) (ann_mean - ann_rf) / ann_vol else NA_real_
  downside <- stats::sd(pmin(r - rf, 0)) * sqrt(periods_per_year)
  sortino  <- if (downside > 0) (ann_mean - ann_rf) / downside else NA_real_

  max_dd <- as.numeric(maxDrawdown(rets))
  calmar <- if (!is.na(max_dd) && max_dd > 0) cagr / max_dd else NA_real_
  cvar95 <- tryCatch(as.numeric(CVaR(rets, p = 0.95, method = "historical")),
                     error = function(e) NA_real_)
  cvar99 <- tryCatch(as.numeric(CVaR(rets, p = 0.99, method = "historical")),
                     error = function(e) NA_real_)
  ulcer  <- tryCatch(as.numeric(UlcerIndex(rets)),
                     error = function(e) NA_real_)

  # Monthly hit rate vs benchmark.
  m_dt <- copy(returns_dt)
  m_dt[, ym := format(date, "%Y-%m")]
  monthly_rets <- m_dt[, .(mret = prod(1 + ret) - 1), by = ym]$mret
  hit_rate <- NA_real_
  if (has_bench) {
    bench_dt <- data.table(date = returns_dt$date, ret = bench)
    bench_dt[, ym := format(date, "%Y-%m")]
    monthly_bench <- bench_dt[, .(mret = prod(1 + ret) - 1), by = ym]$mret
    n <- min(length(monthly_rets), length(monthly_bench))
    if (n > 0) hit_rate <- mean(monthly_rets[seq_len(n)] >
                                monthly_bench[seq_len(n)])
  }

  best_month  <- if (length(monthly_rets) > 0) max(monthly_rets) else NA_real_
  worst_month <- if (length(monthly_rets) > 0) min(monthly_rets) else NA_real_

  # CAPM alpha/beta + Information Ratio.
  alpha <- NA_real_; beta <- NA_real_; ir <- NA_real_
  if (has_bench && stats::sd(bench) > 0) {
    fit  <- stats::lm(r ~ bench)
    alpha <- as.numeric(coef(fit)[1]) * periods_per_year
    beta  <- as.numeric(coef(fit)[2])
    excess <- r - bench
    te     <- stats::sd(excess) * sqrt(periods_per_year)
    if (te > 0) ir <- (mean(excess) * periods_per_year) / te
  }

  data.table(
    strategy          = label,
    n_obs             = length(r),
    years             = round(years, 2),
    total_return      = total_return,
    cagr              = cagr,
    ann_volatility    = ann_vol,
    sharpe_ratio      = sharpe,
    sortino_ratio     = sortino,
    max_drawdown      = max_dd,
    calmar_ratio      = calmar,
    cvar_95           = cvar95,
    cvar_99           = cvar99,
    ulcer_index       = ulcer,
    best_month        = best_month,
    worst_month       = worst_month,
    hit_rate_vs_bench = hit_rate,
    alpha_capm_ann    = alpha,
    beta_capm         = beta,
    information_ratio = ir
  )
}


bootstrap_sharpe_ci <- function(rets,
                                n_bootstrap = 1000,
                                periods_per_year = 252,
                                conf_level = 0.95) {
  r <- na.omit(as.numeric(rets))
  if (length(r) < 30) return(c(lower = NA, upper = NA))
  sharpes <- replicate(n_bootstrap, {
    s <- sample(r, length(r), replace = TRUE)
    (mean(s) / stats::sd(s)) * sqrt(periods_per_year)
  })
  q <- stats::quantile(sharpes, c((1 - conf_level) / 2,
                                  1 - (1 - conf_level) / 2))
  c(lower = q[[1]], upper = q[[2]])
}


# Year-by-year compounded returns. NAs treated as 0 within a year so a
# single missing day doesn't blank the whole year.
annual_return_table <- function(returns_dt, label = "portfolio") {
  setDT(returns_dt)
  returns_dt <- copy(returns_dt)
  returns_dt[, date := as.Date(date)]
  returns_dt[is.na(ret) | !is.finite(ret), ret := 0]
  returns_dt[, year := as.integer(format(date, "%Y"))]
  yr <- returns_dt[, .(annual_return = prod(1 + ret) - 1), by = year]
  setnames(yr, "annual_return", label)
  yr
}
