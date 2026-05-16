# R/data_loader.R
# Load cached Bloomberg extractions and the helpers shared by signal
# generators (cross-sectional z-score, daily return panel construction).

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

# Load all panels written by bloomberg/ extraction phases.
load_all_data <- function() {
  read_pq <- function(fname) {
    path <- file.path(paths$raw, fname)
    if (!file.exists(path)) {
      stop(sprintf(
        "Missing extraction file: %s. Run bloomberg/run_all_extractions.R first.",
        path), call. = FALSE)
    }
    # mmap = FALSE so re-running extraction in the same R session can
    # overwrite the parquet (Windows locks mmap'd files).
    as.data.table(read_parquet(path, mmap = FALSE))
  }

  universe <- as.data.table(read_parquet(
    file.path(paths$universe, "spx_constituents_quarterly.parquet"),
    mmap = FALSE))
  # The universe panel mixes lowercased BDS columns with uppercase BDP
  # columns (GICS_SECTOR_NAME etc.) — normalise here so downstream code
  # can rely on consistent lowercase names.
  setnames(universe, names(universe), tolower(names(universe)))

  list(
    universe     = universe,
    prices       = read_pq("prices_daily.parquet"),
    fundamentals = read_pq("fundamentals_quarterly.parquet"),
    news         = read_pq("news_signals_quarterly.parquet"),
    ownership    = read_pq("ownership_quarterly.parquet"),
    macro        = read_pq("macro_daily.parquet")
  )
}

# Build a long-format daily return panel from the prices extraction.
# Uses TOT_RETURN_INDEX_GROSS_DVDS if present (it isn't in v1), otherwise
# falls back to PX_LAST.
build_daily_returns <- function(prices_dt) {
  dt <- copy(prices_dt)
  dt[, date := as.Date(date)]
  setkey(dt, ticker, date)

  ret_col <- if ("tot_return_index_gross_dvds" %in% names(dt))
               "tot_return_index_gross_dvds" else "px_last"
  dt[, ret_basis := get(ret_col)]
  dt[, ret := ret_basis / shift(ret_basis, 1L) - 1, by = ticker]
  dt[!is.finite(ret), ret := NA_real_]
  dt
}

# Cross-sectional z-score with winsorization at 1st / 99th percentile.
# Coerces Inf/NaN to NA before computing quantiles.
xs_zscore <- function(x, winsor_lo = 0.01, winsor_hi = 0.99) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  if (sum(!is.na(x)) < 3) return(rep(NA_real_, length(x)))
  q   <- stats::quantile(x, c(winsor_lo, winsor_hi), na.rm = TRUE)
  x_w <- pmin(pmax(x, q[1]), q[2])
  sdv <- stats::sd(x_w, na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(0, length(x)))
  (x_w - mean(x_w, na.rm = TRUE)) / sdv
}
