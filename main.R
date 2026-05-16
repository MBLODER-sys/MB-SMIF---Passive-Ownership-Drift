# main.R
# Local pipeline entry point for the Passive Ownership Drift strategy.
#
# Assumes Bloomberg extraction has already been run via:
#     Rscript bloomberg/run_all_extractions.R
# producing parquet files in data/raw/.
#
# Outputs:
#     output/tables/performance_summary_<horizon>.csv
#     output/tables/annual_returns_<horizon>.csv
#     output/tables/holdings_<rebalance_date>.csv (latest only)
#     output/figures/equity_curves_<horizon>.png
#     output/figures/drawdowns_<horizon>.png
#     output/figures/rolling_sharpe_<horizon>.png

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
  library(zoo)
})

source("config/settings.R")
source("config/bbg_fields.R")
source("config/regime_thresholds.R")

source("R/data_loader.R")
source("R/signals/fundamental_news.R")
source("R/signals/passive_intensity.R")
source("R/signals/quality.R")
source("R/signals/value.R")
source("R/regime.R")
source("R/portfolio.R")
source("R/backtest.R")
source("R/metrics.R")
source("R/visualization.R")

message("[main] loading cached Bloomberg data...")
data <- load_all_data()

# ---- Rebalance calendar -----------------------------------------------------
rebalance_dates <- as.Date(unlist(lapply(
  seq.int(year(settings$full_start_date), year(settings$full_end_date)),
  function(yr) sapply(settings$quarterly_months, function(m) {
    sprintf("%04d-%02d-01", yr, m)
  })
)))
rebalance_dates <- rebalance_dates[
  rebalance_dates >= settings$full_start_date &
  rebalance_dates <= settings$full_end_date
]

# Daily return panel.
message("[main] building daily return panel...")
prices_with_ret <- build_daily_returns(data$prices)

# ---- Signals ----------------------------------------------------------------
message("[main] computing fundamental news signal...")
spx_daily_ret <- data$macro[series_name == "spx"][order(date),
                            .(date = as.Date(date),
                              spx_ret = px_last / shift(px_last, 1L) - 1)]
spx_daily_ret[is.na(spx_ret), spx_ret := 0]
news_scores <- compute_fundamental_news(data$news, prices_with_ret,
                                        rebalance_dates,
                                        market_ret_dt = spx_daily_ret)

message("[main] computing passive intensity signal...")
passive_scores <- compute_passive_intensity(
  data$ownership, data$universe, rebalance_dates,
  lag_days = settings$passive_ownership_lag_days
)

message("[main] computing quality screen...")
quality_scores <- compute_quality(data$fundamentals, rebalance_dates)

message("[main] computing value screen...")
value_scores <- compute_value(data$fundamentals, rebalance_dates)

# ---- Merge all per-rebalance scores ----------------------------------------
# passive_scores already carries gics_sector_name; quality/value/news do not.
scores <- merge(news_scores,    passive_scores, by = c("rebalance_date", "ticker"), all = TRUE)
scores <- merge(scores,         quality_scores, by = c("rebalance_date", "ticker"), all = TRUE)
scores <- merge(scores,         value_scores,   by = c("rebalance_date", "ticker"), all = TRUE)

# Fallback sector for tickers only present in news/quality/value but not in
# passive (no ownership data yet).
sectors_lookup <- unique(data$universe[, .(ticker, gics_sector_name)])
sectors_lookup <- sectors_lookup[!duplicated(ticker)]
scores[is.na(gics_sector_name),
       gics_sector_name := sectors_lookup[match(ticker, sectors_lookup$ticker),
                                          gics_sector_name]]

# ---- SPX sector weights per rebalance --------------------------------------
spx_sector_weights <- build_spx_sector_weights(data$universe, rebalance_dates)

# ---- Regime classification --------------------------------------------------
message("[main] classifying regimes...")
regime_dt <- classify_regime(
  macro_dt = data$macro,
  prices_dt = data$prices[ticker %in% data$universe$ticker],
  thresholds = regime_thresholds
)

# ---- Cash rate to daily returns --------------------------------------------
cash_dt <- data$macro[series_name == "cash_rate",
                      .(date = as.Date(date),
                        daily_rate = px_last / 100 / 252)]

# ---- Macro shaped for VIX override ------------------------------------------
macro_long <- copy(data$macro)
macro_long[, date := as.Date(date)]

# ---- Run all five comparator backtests for each horizon ---------------------
run_horizon <- function(start_date, label) {

  message(sprintf("\n===== horizon: %s (%s -> %s) =====",
                  label, start_date, settings$backtest_end))

  in_window <- function(dt, col = "date") {
    dt[get(col) >= start_date & get(col) <= settings$backtest_end]
  }
  rebals  <- rebalance_dates[rebalance_dates >= start_date &
                             rebalance_dates <= settings$backtest_end]
  pr_win  <- in_window(prices_with_ret)
  reg_win <- in_window(regime_dt)
  mac_win <- in_window(macro_long)
  cash_win <- in_window(cash_dt)
  scores_win <- scores[rebalance_date >= start_date &
                       rebalance_date <= settings$backtest_end]
  pi_win <- passive_scores[rebalance_date >= start_date &
                           rebalance_date <= settings$backtest_end]
  spx_w_win <- spx_sector_weights[rebalance_date >= start_date &
                                  rebalance_date <= settings$backtest_end]

  modes <- c("full", "news_only", "passive_only", "static")
  results <- lapply(modes, function(m) {
    message(sprintf("[main] backtest mode = %s", m))
    bt <- run_backtest(
      rebalance_dates       = rebals,
      scores_dt             = scores_win,
      prices_dt             = pr_win,
      spx_sector_weights_dt = spx_w_win,
      passive_intensity_dt  = pi_win,
      regime_dt             = reg_win,
      macro_dt              = mac_win,
      cash_dt               = cash_win,
      settings              = settings,
      thresholds            = regime_thresholds,
      mode                  = m
    )
    bt[, strategy := switch(m,
                            full = "Strategy (full)",
                            news_only = "News-only",
                            passive_only = "Passive-only",
                            static = "Static (no regime)")]
    bt
  })
  bt_all <- rbindlist(results, use.names = TRUE, fill = TRUE)

  # ---- Benchmarks -----------------------------------------------------------
  spx_tr <- mac_win[series_name == "spx_tr"][order(date),
                    .(date, ret = px_last / shift(px_last, 1L) - 1)]
  spx_tr[is.na(ret), ret := 0]
  spx_tr[, equity := cumprod(1 + ret)]
  spx_tr[, strategy := "S&P 500 TR (SPX)"]

  spxew <- mac_win[series_name == "spxew"][order(date),
                   .(date, ret = px_last / shift(px_last, 1L) - 1)]
  spxew[is.na(ret), ret := 0]
  spxew[, equity := cumprod(1 + ret)]
  spxew[, strategy := "S&P 500 EW (SPXEW)"]

  combined <- rbindlist(list(
    bt_all[, .(date, ret, equity, strategy)],
    spx_tr[, .(date, ret, equity, strategy)],
    spxew[, .(date, ret, equity, strategy)]
  ), use.names = TRUE)

  # ---- Metrics --------------------------------------------------------------
  spx_for_bench <- spx_tr[, .(date, ret)]
  perf <- rbindlist(lapply(unique(combined$strategy), function(s) {
    compute_metrics(combined[strategy == s, .(date, ret)],
                    label = s,
                    benchmark_dt = spx_for_bench,
                    risk_free_dt = cash_win)
  }), use.names = TRUE, fill = TRUE)

  # ---- Annual returns table -------------------------------------------------
  annual <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE),
                   lapply(unique(combined$strategy), function(s) {
                     annual_return_table(
                       combined[strategy == s, .(date, ret)],
                       label = s
                     )
                   }))

  # ---- Persist artifacts ----------------------------------------------------
  fwrite(perf, file.path(paths$tables,
                         sprintf("performance_summary_%s.csv", label)))
  fwrite(annual, file.path(paths$tables,
                           sprintf("annual_returns_%s.csv", label)))

  # Save the latest holdings for inspection.
  latest_rd <- max(rebals)
  latest_full <- build_portfolio(
    rebalance_date = latest_rd,
    scores_dt = scores_win,
    prices_dt = pr_win,
    spx_sector_weights_dt = spx_w_win,
    settings = settings,
    prev_holdings = NULL,
    mode = "full"
  )
  fwrite(latest_full,
         file.path(paths$tables,
                   sprintf("holdings_latest_%s_%s.csv",
                           label, format(latest_rd, "%Y%m%d"))))

  # Figures.
  plot_equity_curves(combined,
                     file.path(paths$figures,
                               sprintf("equity_curves_%s.png", label)),
                     title = sprintf("Equity curves (%s)", label))
  plot_drawdown_curves(combined,
                       file.path(paths$figures,
                                 sprintf("drawdowns_%s.png", label)),
                       title = sprintf("Drawdowns (%s)", label))
  plot_rolling_sharpe(combined,
                      window = 252,
                      out_path = file.path(paths$figures,
                                           sprintf("rolling_sharpe_%s.png", label)),
                      title = sprintf("Rolling 12-month Sharpe (%s)", label))

  message(sprintf("[main] horizon %s complete. Tables in %s, figures in %s.",
                  label, paths$tables, paths$figures))

  list(performance = perf, annual = annual, combined = combined)
}

ten_yr     <- run_horizon(settings$ten_year_start,    "10yr")
twenty_yr  <- run_horizon(settings$twenty_year_start, "20yr")

message("\n[main] ============== DONE ==============")
message(sprintf("[main] Outputs: %s", paths$tables))
print(ten_yr$performance)
print(twenty_yr$performance)
