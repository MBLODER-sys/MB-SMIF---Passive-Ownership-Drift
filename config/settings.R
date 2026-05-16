# config/settings.R
# Global settings for the Passive Ownership Drift strategy.
# Loaded by both Bloomberg extraction and local backtest pipelines.

settings <- list(

  # Date windows
  full_start_date   = as.Date("2005-01-01"),
  full_end_date     = as.Date("2024-12-31"),
  ten_year_start    = as.Date("2015-01-01"),
  twenty_year_start = as.Date("2005-01-01"),
  backtest_end      = as.Date("2024-12-31"),

  # Universe
  index_ticker          = "SPX Index",
  benchmark_tr_ticker   = "SPXT Index",     # Total-return S&P 500
  benchmark_ew_ticker   = "SPW Index",      # Equal-weight S&P 500 (price); use SPXEWTR for TR if entitled
  cash_ticker           = "USGG3M Index",   # 3-month T-bill for cash sleeve

  # Rebalance cadences (in trading days, applied via month/quarter membership in code)
  quarterly_months      = c(1, 4, 7, 10),
  monthly_rebalance     = TRUE,
  daily_vix_override    = TRUE,

  # Portfolio construction
  long_quintile_pct           = 0.20,   # top-quintile by composite
  hold_decay_pct              = 0.25,   # incumbent kept unless below 25th pctile
  entry_threshold_pct         = 0.80,   # new entrants must be above 80th pctile
  max_position_weight         = 0.05,   # 5% cap
  sector_neutral_tolerance    = 0.03,   # +/- 3% of SPX sector weight
  quality_exclude_decile      = 1,      # exclude bottom decile on quality
  value_exclude_decile        = 1,      # exclude bottom decile on value
  min_news_components_positive = 3,     # 3-of-5 multi-signal confirmation
  vol_window_days             = 90,     # for inverse-vol weighting

  # Passive ownership lag (institutional filing lag, ~45 calendar days)
  passive_ownership_lag_days  = 45,

  # Transaction costs (bps per side)
  tcost_bps_per_side          = 10,

  # Daily VIX override
  vix_spike_level             = 30,
  vix_spike_5d_jump           = 10,
  vix_release_level           = 25,
  vix_release_dwell_days      = 5,
  vix_override_target_invested = 0.80,

  # Reproducibility
  random_seed                 = 20260516
)

# Path helpers — absolute paths derived from project root.
# The project root is the directory containing config/, data/, R/, bloomberg/, output/.
# Resolves via rprojroot if installed; otherwise assumes the current working
# directory is project root (the convention enforced in the README).
proj_root <- function() {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(tryCatch(
      rprojroot::find_root(rprojroot::has_file("README.md")),
      error = function(e) normalizePath(getwd(), mustWork = FALSE)
    ))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

paths <- list(
  root      = proj_root(),
  raw       = file.path(proj_root(), "data", "raw"),
  processed = file.path(proj_root(), "data", "processed"),
  universe  = file.path(proj_root(), "data", "universe"),
  cache     = file.path(proj_root(), "data", "cache"),
  figures   = file.path(proj_root(), "output", "figures"),
  tables    = file.path(proj_root(), "output", "tables"),
  reports   = file.path(proj_root(), "output", "reports")
)

for (p in paths) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

set.seed(settings$random_seed)
