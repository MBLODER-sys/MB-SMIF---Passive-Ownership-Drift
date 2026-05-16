# main.R
# Local pipeline entry point for the Passive Ownership Drift strategy.
#
# Assumes Bloomberg extraction has been run via `source("extract.R")`
# producing parquet files in data/raw/ and data/universe/.
#
# Outputs per horizon (10yr and 20yr):
#   output/tables/performance_summary_<horizon>.csv
#   output/tables/annual_returns_<horizon>.csv
#   output/tables/holdings_latest_<horizon>_<rebal>.csv
#   output/figures/equity_curves_<horizon>.png
#   output/figures/drawdowns_<horizon>.png
#   output/figures/rolling_sharpe_<horizon>.png

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
  library(zoo)
})

source("config/settings.R")
source("config/bbg_fields.R")

source("R/data_loader.R")
source("R/signals/fundamental_news.R")
source("R/signals/passive_intensity.R")
source("R/signals/quality.R")
source("R/signals/value.R")
source("R/portfolio.R")
source("R/backtest.R")
source("R/metrics.R")
source("R/visualization.R")

message("[main] loading cached Bloomberg data...")
data <- load_all_data()

# Rebalance calendar: 1st of Jan/Apr/Jul/Oct within the full window.
rebalance_dates <- as.Date(unlist(lapply(
  seq.int(year(settings$full_start_date), year(settings$full_end_date)),
  function(yr) sprintf("%04d-%02d-01", yr, settings$quarterly_months)
)))
rebalance_dates <- sort(unique(rebalance_dates[
  rebalance_dates >= settings$full_start_date &
  rebalance_dates <= settings$full_end_date
]))

message("[main] building daily return panel...")
prices_with_ret <- build_daily_returns(data$prices)

message("[main] computing fundamental news signal...")
news_scores <- compute_fundamental_news(
  data$news, prices_with_ret, rebalance_dates
)

message("[main] computing passive intensity signal...")
passive_scores <- compute_passive_intensity(
  data$ownership, data$universe, rebalance_dates,
  lag_days = settings$passive_ownership_lag_days
)

message("[main] computing quality screen...")
quality_scores <- compute_quality(data$fundamentals, rebalance_dates)

message("[main] computing value screen...")
value_scores <- compute_value(data$fundamentals, rebalance_dates)

# Merge per-rebalance scores. passive_scores carries gics_sector_name;
# fall back to the universe sector map for any tickers missing it.
scores <- merge(news_scores,    passive_scores, by = c("rebalance_date", "ticker"), all = TRUE)
scores <- merge(scores,         quality_scores, by = c("rebalance_date", "ticker"), all = TRUE)
scores <- merge(scores,         value_scores,   by = c("rebalance_date", "ticker"), all = TRUE)

sectors_lookup <- unique(data$universe[, .(ticker, gics_sector_name)])
sectors_lookup <- sectors_lookup[!duplicated(ticker)]
scores[is.na(gics_sector_name),
       gics_sector_name := sectors_lookup[match(ticker, sectors_lookup$ticker),
                                          gics_sector_name]]

spx_sector_weights <- build_spx_sector_weights(data$universe, rebalance_dates)

# Cash rate as a daily decimal.
cash_dt <- data$macro[series_name == "cash_rate",
                      .(date = as.Date(date),
                        daily_rate = px_last / 100 / 252)]

# --------------------------------------------------------------------------
# Run all comparator backtests for a single horizon.
# --------------------------------------------------------------------------
run_horizon <- function(start_date, label) {

  message(sprintf("\n===== horizon: %s (%s -> %s) =====",
                  label, start_date, settings$backtest_end))

  in_window <- function(dt, col = "date")
    dt[get(col) >= start_date & get(col) <= settings$backtest_end]

  rebals     <- rebalance_dates[rebalance_dates >= start_date &
                                rebalance_dates <= settings$backtest_end]
  pr_win     <- in_window(prices_with_ret)
  cash_win   <- in_window(cash_dt)
  scores_win <- scores[rebalance_date >= start_date &
                       rebalance_date <= settings$backtest_end]
  spx_w_win  <- spx_sector_weights[rebalance_date >= start_date &
                                   rebalance_date <= settings$backtest_end]

  modes <- c("full", "news_only", "passive_only")
  mode_labels <- c(full = "Strategy",
                   news_only = "News-only",
                   passive_only = "Passive-only")

  bt_all <- rbindlist(lapply(modes, function(m) {
    message(sprintf("[main] backtest mode = %s", m))
    bt <- run_backtest(
      rebalance_dates       = rebals,
      scores_dt             = scores_win,
      prices_dt             = pr_win,
      spx_sector_weights_dt = spx_w_win,
      cash_dt               = cash_win,
      settings              = settings,
      mode                  = m
    )
    bt[, strategy := mode_labels[[m]]]
    bt
  }), use.names = TRUE, fill = TRUE)

  # Benchmarks from the macro extraction.
  bench <- function(series, label) {
    d <- in_window(data$macro)[series_name == series][order(date),
       .(date = as.Date(date),
         ret  = px_last / shift(px_last, 1L) - 1)]
    d[is.na(ret), ret := 0]
    d[, equity := cumprod(1 + ret)]
    d[, strategy := label]
    d
  }
  spx_tr <- bench("spx_tr", "S&P 500 TR (SPXT)")
  spxew  <- bench("spxew",  "S&P 500 EW (SPXEW)")

  combined <- rbindlist(list(
    bt_all[, .(date, ret, equity, strategy)],
    spx_tr[, .(date, ret, equity, strategy)],
    spxew[, .(date, ret, equity, strategy)]
  ), use.names = TRUE)

  # Per-portfolio performance metrics.
  spx_for_bench <- spx_tr[, .(date, ret)]
  perf <- rbindlist(lapply(unique(combined$strategy), function(s) {
    compute_metrics(combined[strategy == s, .(date, ret)],
                    label = s,
                    benchmark_dt = spx_for_bench,
                    risk_free_dt = cash_win)
  }), use.names = TRUE, fill = TRUE)

  # Annual returns matrix.
  annual <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE),
                   lapply(unique(combined$strategy), function(s) {
                     annual_return_table(
                       combined[strategy == s, .(date, ret)], label = s)
                   }))

  # Persist artifacts.
  fwrite(perf,   file.path(paths$tables,
                           sprintf("performance_summary_%s.csv", label)))
  fwrite(annual, file.path(paths$tables,
                           sprintf("annual_returns_%s.csv", label)))

  latest_rd <- max(rebals)
  latest_full <- build_portfolio(
    rebalance_date        = latest_rd,
    scores_dt             = scores_win,
    prices_dt             = pr_win,
    spx_sector_weights_dt = spx_w_win,
    settings              = settings,
    prev_holdings         = NULL,
    mode                  = "full"
  )
  fwrite(latest_full,
         file.path(paths$tables,
                   sprintf("holdings_latest_%s_%s.csv",
                           label, format(latest_rd, "%Y%m%d"))))

  plot_equity_curves(combined,
    file.path(paths$figures, sprintf("equity_curves_%s.png", label)),
    title = sprintf("Equity curves (%s)", label))
  plot_drawdown_curves(combined,
    file.path(paths$figures, sprintf("drawdowns_%s.png", label)),
    title = sprintf("Drawdowns (%s)", label))
  plot_rolling_sharpe(combined,
    window = 252,
    out_path = file.path(paths$figures, sprintf("rolling_sharpe_%s.png", label)),
    title = sprintf("Rolling 12-month Sharpe (%s)", label))

  message(sprintf("[main] horizon %s complete. Tables: %s  Figures: %s",
                  label, paths$tables, paths$figures))

  list(performance = perf, annual = annual, combined = combined)
}

ten_yr    <- run_horizon(settings$ten_year_start,    "10yr")
twenty_yr <- run_horizon(settings$twenty_year_start, "20yr")

message("\n[main] ============== DONE ==============")
print(ten_yr$performance)
print(twenty_yr$performance)
