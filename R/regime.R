# R/regime.R
# Four-state regime classifier with 60-day minimum dwell time.
# Inputs: daily macro panel (VIX, HY OAS, yield-curve slope, market breadth).

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
})

classify_regime <- function(macro_dt,
                            prices_dt,
                            thresholds) {

  setDT(macro_dt); setDT(prices_dt)
  macro_dt[, date := as.Date(date)]

  # Defensive: ensure the macro panel has the expected structural columns
  # before reshaping. Without `series_name`, dcast collapses every series
  # into one column and the regime classifier degrades silently.
  required_macro_cols <- c("date", "series_name", "px_last")
  missing_cols <- setdiff(required_macro_cols, names(macro_dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("[regime] macro panel is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Reshape macro to wide: one column per series.
  macro_w <- dcast(macro_dt[, .(date, series_name, px_last)],
                   date ~ series_name,
                   value.var = "px_last")
  setnames(macro_w, tolower(names(macro_w)))

  needed_series <- c("vix", "hy_oas", "yc_10y", "yc_3m")
  missing_series <- setdiff(needed_series, names(macro_w))
  if (length(missing_series) > 0) {
    warning(sprintf("[regime] macro panel missing series: %s — defaulting to neutral state",
                    paste(missing_series, collapse = ", ")), call. = FALSE)
  }

  # Compute HY OAS percentile (expanding window).
  if ("hy_oas" %in% names(macro_w)) {
    macro_w[, hy_oas_pctile := frank(hy_oas, na.last = "keep") /
                               sum(!is.na(hy_oas))]
  } else {
    macro_w[, hy_oas_pctile := 0.5]
  }

  # Market breadth: % of S&P 500 above their 200-day MA.
  breadth <- prices_dt[, .(ticker, date = as.Date(date), px_last)]
  setkey(breadth, ticker, date)
  breadth[, ma200 := zoo::rollapplyr(px_last,
                                     200,
                                     mean,
                                     partial = TRUE,
                                     fill = NA_real_,
                                     na.rm = TRUE),
          by = ticker]
  breadth[, above_ma := px_last > ma200]
  bd <- breadth[, .(breadth_pct = mean(above_ma, na.rm = TRUE)), by = date]
  macro_w <- merge(macro_w, bd, by = "date", all.x = TRUE)

  # Yield-curve slope in bps.
  if (!"yc_slope" %in% names(macro_w) &&
      all(c("yc_10y", "yc_3m") %in% names(macro_w))) {
    macro_w[, yc_slope := (yc_10y - yc_3m) * 100]
  }

  # VIX falling-from-peak (over a 20d window).
  setorder(macro_w, date)
  if ("vix" %in% names(macro_w)) {
    macro_w[, vix_20d_high := zoo::rollapplyr(vix, 20, max,
                                              partial = TRUE, fill = NA_real_)]
    macro_w[, vix_falling := vix < vix_20d_high * 0.85]
  }

  # State logic, deterministic, evaluated row by row with dwell-time gating.
  macro_w[, regime_raw := thresholds$default_state]
  for (i in seq_len(nrow(macro_w))) {
    r <- macro_w[i]
    state <- thresholds$default_state

    is_stressed <- (!is.na(r$vix) && r$vix > thresholds$vix_stressed_level) &&
                   (((!is.na(r$hy_oas_pctile) &&
                      r$hy_oas_pctile > thresholds$hy_spread_pctile)) ||
                    ((!is.na(r$breadth_pct) &&
                      r$breadth_pct < thresholds$breadth_below_ma_pct)))

    is_late <- (!is.na(r$yc_slope) && r$yc_slope < thresholds$curve_late_cycle_bp) ||
               (!is.na(r$breadth_pct) && r$breadth_pct <
                  thresholds$breadth_below_ma_pct + 0.20)

    is_recovery <- (!is.na(r$vix_falling) && r$vix_falling) &&
                   (!is.na(r$breadth_pct) && r$breadth_pct >
                      thresholds$breadth_recovery_pct)

    if (is_stressed)      state <- "Stressed"
    else if (is_recovery) state <- "Recovery"
    else if (is_late)     state <- "Late-Cycle"
    else                  state <- "Calm-Expansion"

    macro_w[i, regime_raw := state]
  }

  # Enforce 60-day minimum dwell.
  macro_w[, regime := regime_raw]
  current <- macro_w$regime_raw[1]
  current_start <- 1L
  for (i in seq_len(nrow(macro_w))) {
    if (macro_w$regime_raw[i] != current) {
      days_in_current <- i - current_start
      if (days_in_current < thresholds$min_dwell_days) {
        # Revert candidate transition.
        macro_w$regime[i] <- current
      } else {
        current <- macro_w$regime_raw[i]
        current_start <- i
      }
    } else {
      macro_w$regime[i] <- current
    }
  }

  macro_w[, .(date, vix, hy_oas, yc_slope, breadth_pct, regime)]
}
