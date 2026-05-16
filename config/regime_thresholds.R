# config/regime_thresholds.R
# Four-state regime classifier parameters.
# See README section 2 ("Regime Overlay").

regime_thresholds <- list(

  # State-trigger thresholds
  vix_stressed_level   = 25,
  hy_spread_pctile     = 0.80,
  breadth_below_ma_pct = 0.30,
  curve_late_cycle_bp  = 25,        # 10Y - 3M below 25bp = flattening
  curve_inverted_bp    = 0,

  # Recovery
  breadth_recovery_pct = 0.50,
  vix_falling_window   = 20,        # business days

  # Dwell to suppress flicker
  min_dwell_days       = 60,

  # Exposure tilts by regime (multiplier on highest-passive-ownership quintile weight)
  exposure_multipliers = list(
    `Calm-Expansion` = 1.00,
    `Late-Cycle`     = 0.70,
    `Stressed`       = 0.40,
    `Recovery`       = 0.90
  ),

  # Default state at series start
  default_state        = "Calm-Expansion"
)
