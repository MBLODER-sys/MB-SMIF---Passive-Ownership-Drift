# R/metrics.R
# Performance and risk metrics. Returns a single-row data.table per portfolio.
# Outputs are written as CSV by main.R for the headline performance table.

suppressPackageStartupMessages({
  library(data.table)
  library(PerformanceAnalytics)
  library(xts)
})

compute_metrics <- function(returns_dt,
                            label,
                            benchmark_dt = NULL,
                            risk_free_dt = NULL,
                            periods_per_year = 252) {

  setDT(returns_dt)
  returns_dt[, date := as.Date(date)]

  if (nrow(returns_dt) == 0) {
    return(data.table(strategy = label))
  }

  rets <- xts(returns_dt$ret, order.by = returns_dt$date)
  colnames(rets) <- label

  rf_xts <- NULL
  if (!is.null(risk_free_dt)) {
    rf_xts <- xts(risk_free_dt$daily_rate, order.by = risk_free_dt$date)
    rf_xts <- rf_xts[index(rets)]
  }

  bench_xts <- NULL
  if (!is.null(benchmark_dt)) {
    bench_xts <- xts(benchmark_dt$ret, order.by = benchmark_dt$date)
    bench_xts <- bench_xts[index(rets)]
  }

  total_return <- as.numeric(prod(1 + coredata(rets), na.rm = TRUE) - 1)
  years <- as.numeric(diff(range(returns_dt$date))) / 365.25
  cagr <- (1 + total_return)^(1 / max(years, 1e-9)) - 1
  ann_vol <- as.numeric(sd(rets, na.rm = TRUE) * sqrt(periods_per_year))
  sharpe <- if (!is.null(rf_xts)) {
              as.numeric(SharpeRatio.annualized(rets, Rf = rf_xts,
                                                scale = periods_per_year))
            } else {
              as.numeric(SharpeRatio.annualized(rets, scale = periods_per_year))
            }
  sortino <- as.numeric(SortinoRatio(rets) * sqrt(periods_per_year))
  max_dd  <- as.numeric(maxDrawdown(rets))
  calmar  <- if (max_dd > 0) cagr / max_dd else NA_real_
  cvar95  <- as.numeric(CVaR(rets, p = 0.95, method = "historical"))
  cvar99  <- as.numeric(CVaR(rets, p = 0.99, method = "historical"))
  ulcer   <- as.numeric(UlcerIndex(rets))

  monthly <- apply.monthly(rets, function(x) prod(1 + x) - 1)
  monthly <- monthly[!is.na(monthly)]
  best_month  <- as.numeric(max(monthly, na.rm = TRUE))
  worst_month <- as.numeric(min(monthly, na.rm = TRUE))

  hit_rate <- if (!is.null(bench_xts)) {
                bench_monthly <- apply.monthly(bench_xts,
                                               function(x) prod(1 + x) - 1)
                common <- index(monthly)[index(monthly) %in% index(bench_monthly)]
                if (length(common) > 0) {
                  as.numeric(mean(monthly[common] > bench_monthly[common],
                                  na.rm = TRUE))
                } else NA_real_
              } else NA_real_

  ir <- NA_real_; alpha <- NA_real_; beta <- NA_real_
  if (!is.null(bench_xts)) {
    excess <- rets - bench_xts
    te <- as.numeric(sd(excess, na.rm = TRUE) * sqrt(periods_per_year))
    ir <- as.numeric(mean(excess, na.rm = TRUE) * periods_per_year) / max(te, 1e-9)
    capm <- CAPM.alpha(rets, bench_xts, Rf = 0)
    alpha <- as.numeric(capm) * periods_per_year
    beta <- as.numeric(CAPM.beta(rets, bench_xts, Rf = 0))
  }

  data.table(
    strategy            = label,
    n_obs               = length(rets),
    years               = round(years, 2),
    total_return        = total_return,
    cagr                = cagr,
    ann_volatility      = ann_vol,
    sharpe_ratio        = sharpe,
    sortino_ratio       = sortino,
    max_drawdown        = max_dd,
    calmar_ratio        = calmar,
    cvar_95             = cvar95,
    cvar_99             = cvar99,
    ulcer_index         = ulcer,
    best_month          = best_month,
    worst_month         = worst_month,
    hit_rate_vs_bench   = hit_rate,
    alpha_capm_ann      = alpha,
    beta_capm           = beta,
    information_ratio   = ir
  )
}


bootstrap_sharpe_ci <- function(rets,
                                n_bootstrap = 1000,
                                periods_per_year = 252,
                                conf_level = 0.95) {
  r <- na.omit(as.numeric(rets))
  if (length(r) < 30) return(c(lower = NA, upper = NA))
  sharpes <- replicate(n_bootstrap, {
    samp <- sample(r, length(r), replace = TRUE)
    (mean(samp) / stats::sd(samp)) * sqrt(periods_per_year)
  })
  q <- stats::quantile(sharpes, c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2))
  c(lower = q[1], upper = q[2])
}


# Year-by-year compounded returns. NAs are treated as 0 within a year so a
# single missing day doesn't blank the whole year.
annual_return_table <- function(returns_dt, label = "portfolio") {
  setDT(returns_dt)
  returns_dt <- copy(returns_dt)
  returns_dt[, date := as.Date(date)]
  returns_dt[is.na(ret), ret := 0]
  returns_dt[, year := as.integer(format(date, "%Y"))]
  yr <- returns_dt[, .(annual_return = prod(1 + ret) - 1), by = year]
  setnames(yr, "annual_return", label)
  yr
}
