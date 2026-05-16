# R/data_loader.R
# Load all cached Bloomberg extractions into a single named list and apply
# point-in-time alignment + the passive-ownership filing lag.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

# Load all panels.
load_all_data <- function() {

  source(file.path(paths$root, "config", "settings.R"), local = TRUE)
  source(file.path(paths$root, "config", "bbg_fields.R"), local = TRUE)

  read_pq <- function(fname) {
    path <- file.path(paths$raw, fname)
    if (!file.exists(path)) {
      stop(sprintf("Missing extraction file: %s. Run bloomberg/run_all_extractions.R first.",
                   path), call. = FALSE)
    }
    as.data.table(read_parquet(path))
  }

  universe   <- as.data.table(read_parquet(
                   file.path(paths$universe, "spx_constituents_quarterly.parquet")))
  prices     <- read_pq("prices_daily.parquet")
  fundamentals <- read_pq("fundamentals_quarterly.parquet")
  news       <- read_pq("news_signals_quarterly.parquet")
  ownership  <- read_pq("ownership_quarterly.parquet")
  macro      <- read_pq("macro_daily.parquet")

  list(
    universe     = universe,
    prices       = prices,
    fundamentals = fundamentals,
    news         = news,
    ownership    = ownership,
    macro        = macro
  )
}

# PIT-align quarterly fundamentals/news to a daily-frequency state-at-date table
# by forward-filling each panel field starting from the announcement date.
# Conservative: if LATEST_ANNOUNCEMENT_DT is missing, lag the period date by
# `default_lag_days` (typical reporting lag for US large-caps).
pit_align <- function(panel,
                      ticker_col       = "ticker",
                      date_col         = "date",
                      announce_col     = "latest_announcement_dt",
                      default_lag_days = 45) {

  dt <- copy(panel)
  if (!announce_col %in% names(dt)) {
    dt[, available_date := as.Date(get(date_col)) + default_lag_days]
  } else {
    dt[, available_date := as.Date(get(announce_col))]
    dt[is.na(available_date),
       available_date := as.Date(get(date_col)) + default_lag_days]
  }
  setkeyv(dt, c(ticker_col, "available_date"))
  dt
}

# Apply institutional-filing lag to ownership.
lag_ownership <- function(ownership_dt,
                          lag_days = 45) {
  dt <- copy(ownership_dt)
  dt[, available_date := as.Date(date) + lag_days]
  setkey(dt, ticker, available_date)
  dt
}

# Build a long-format daily return panel from prices, using the total-return
# index where available, falling back to PX_LAST.
build_daily_returns <- function(prices_dt) {
  dt <- copy(prices_dt)
  dt[, date := as.Date(date)]
  setkey(dt, ticker, date)

  ret_col <- if ("tot_return_index_gross_dvds" %in% names(dt))
                "tot_return_index_gross_dvds" else "px_last"

  dt[, ret_basis := get(ret_col)]
  dt[, ret := ret_basis / shift(ret_basis, 1L) - 1, by = ticker]
  # Treat first-observation and missing prior-day returns as NA (not 0).
  # The backtest engine drops days where the held name has no observation.
  dt[!is.finite(ret), ret := NA_real_]
  dt
}

# Cross-sectional z-score with winsorization at 1st / 99th percentile.
# Handles Inf/NaN by coercing to NA before computing quantiles.
xs_zscore <- function(x, winsor_lo = 0.01, winsor_hi = 0.99) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  if (sum(!is.na(x)) < 3) return(rep(NA_real_, length(x)))
  q <- stats::quantile(x, c(winsor_lo, winsor_hi), na.rm = TRUE)
  x_w <- pmin(pmax(x, q[1]), q[2])
  mu  <- mean(x_w, na.rm = TRUE)
  sdv <- stats::sd(x_w, na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(0, length(x)))
  (x_w - mu) / sdv
}
