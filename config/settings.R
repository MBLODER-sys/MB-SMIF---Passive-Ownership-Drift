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

  # Universe / benchmarks
  index_ticker        = "SPX Index",
  benchmark_tr_ticker = "SPXT Index",
  benchmark_ew_ticker = "SPXEW Index",
  cash_ticker         = "USGG3M Index",

  # Rebalance cadence
  quarterly_months    = c(1, 4, 7, 10),

  # Portfolio construction
  long_quintile_pct            = 0.20,
  hold_decay_pct               = 0.25,
  entry_threshold_pct          = 0.80,
  max_position_weight          = 0.05,
  sector_neutral_tolerance     = 0.03,
  quality_exclude_decile       = 1,
  value_exclude_decile         = 1,
  min_news_components_positive = 3,
  vol_window_days              = 90,

  # Passive ownership filing lag (~45 calendar days)
  passive_ownership_lag_days   = 45,

  # Transaction costs (bps per side)
  tcost_bps_per_side           = 10,

  # Reproducibility
  random_seed                  = 20260516
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
