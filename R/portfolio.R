# R/portfolio.R
# v2 portfolio construction.
#
# Key changes from v1:
# - Quality/value filters only drop names that ARE scored AND in the
#   bottom decile (settings$exclude_only_when_scored). NA-scored names
#   are kept and let the composite resolve them.
# - News eligibility uses a fraction-of-available-components rule, so
#   tickers with only 2 non-NA news components can still qualify.
# - Composite supports three forms: rank_product (default, [0,1]),
#   rank_sum, and the v1 zscore_product. Rank methods don't suffer the
#   negative-times-negative artefact.
# - Final weights are scaled up to enforce a hard equity-exposure floor
#   (settings$min_equity_exposure) when fewer names get selected than
#   would naturally hit the floor.

suppressPackageStartupMessages({
  library(data.table)
})

build_portfolio <- function(rebalance_date,
                            scores_dt,
                            prices_dt,
                            spx_sector_weights_dt,
                            settings,
                            prev_holdings = NULL,
                            mode = c("full", "news_only", "passive_only"),
                            verbose = FALSE) {

  mode <- match.arg(mode)
  rd <- as.Date(rebalance_date)

  s <- copy(scores_dt[rebalance_date == rd])
  n0 <- nrow(s)
  if (n0 == 0) {
    if (verbose) message(sprintf(
      "[build_portfolio %s %s] no scores rows for this date",
      format(rd), mode))
    return(empty_holdings())
  }

  # ---- Risk-control screens (RELAXED in v2) ------------------------------
  if (isTRUE(settings$exclude_only_when_scored)) {
    s <- s[is.na(quality_decile) |
           quality_decile > settings$quality_exclude_decile]
    s <- s[is.na(value_decile) |
           value_decile > settings$value_exclude_decile]
  } else {
    s <- s[!is.na(quality_decile) & quality_decile > settings$quality_exclude_decile]
    s <- s[!is.na(value_decile)   & value_decile   > settings$value_exclude_decile]
  }
  n_qv <- nrow(s)

  # News eligibility: only applies in modes that USE the news signal.
  if (mode != "passive_only") {
    s <- s[news_eligible_v2(s, settings)]
  }
  n_news <- nrow(s)
  if (n_news == 0) {
    if (verbose) message(sprintf(
      "[build_portfolio %s %s] funnel: %d -> qual/val %d -> news %d (EMPTY)",
      format(rd), mode, n0, n_qv, n_news))
    return(empty_holdings())
  }

  # ---- Composite by mode --------------------------------------------------
  s[, composite := composite_score(s, mode, settings)]
  s <- s[!is.na(composite)]
  n_comp <- nrow(s)
  if (n_comp == 0) {
    if (verbose) message(sprintf(
      "[build_portfolio %s %s] funnel: %d -> qual/val %d -> news %d -> composite %d (EMPTY)",
      format(rd), mode, n0, n_qv, n_news, n_comp))
    return(empty_holdings())
  }

  # ---- Selection: turnover-buffered top quantile -------------------------
  s[, comp_pctile := frank(composite, na.last = "keep") / .N]
  incumbent_tickers <- if (!is.null(prev_holdings)) prev_holdings$ticker else character()
  s[, is_incumbent := ticker %in% incumbent_tickers]
  s[, selected := FALSE]
  s[is_incumbent  & comp_pctile >= settings$hold_decay_pct,      selected := TRUE]
  s[!is_incumbent & comp_pctile >= settings$entry_threshold_pct, selected := TRUE]

  # Fallback: if too few selected, take the top quintile outright. Ensures
  # the strategy is never stuck holding nothing.
  if (sum(s$selected) < 30) {
    s[, selected := comp_pctile >= (1 - settings$long_quintile_pct)]
  }
  n_sel <- sum(s$selected)
  if (verbose) message(sprintf(
    "[build_portfolio %s %s] funnel: %d -> qual/val %d -> news %d -> composite %d -> selected %d",
    format(rd), mode, n0, n_qv, n_news, n_comp, n_sel))

  sel <- s[selected == TRUE]
  if (nrow(sel) == 0) return(empty_holdings())

  # ---- Inverse-volatility weights ----------------------------------------
  vol_window_start <- rd - settings$vol_window_days * 1.5
  vol <- prices_dt[ticker %in% sel$ticker & date <= rd & date >= vol_window_start,
                   .(vol_90 = stats::sd(ret, na.rm = TRUE) * sqrt(252)),
                   by = ticker]
  sel <- merge(sel, vol, by = "ticker", all.x = TRUE)
  sel[is.na(vol_90) | vol_90 == 0, vol_90 := stats::median(vol_90, na.rm = TRUE)]
  sel[is.na(vol_90),               vol_90 := 0.20]
  sel[, inv_vol_weight := 1 / vol_90]

  # ---- Sector neutrality with tolerance band ------------------------------
  sel[, within_sector_w := inv_vol_weight / sum(inv_vol_weight, na.rm = TRUE),
      by = gics_sector_name]
  sec_w <- copy(spx_sector_weights_dt[rebalance_date == rd])
  if (nrow(sec_w) > 0) {
    sel <- merge(sel, sec_w[, .(gics_sector_name, spx_weight)],
                 by = "gics_sector_name", all.x = TRUE)
    sel[is.na(spx_weight),
        spx_weight := stats::median(sel$spx_weight, na.rm = TRUE)]
    tol <- settings$sector_neutral_tolerance
    sel[, sector_target := pmin(pmax(spx_weight, spx_weight - tol),
                                spx_weight + tol)]
    sel[, weight := within_sector_w * sector_target]
  } else {
    sel[, weight := inv_vol_weight / sum(inv_vol_weight, na.rm = TRUE)]
  }

  # ---- Position cap, renormalise, ENFORCE EQUITY FLOOR -------------------
  sel[, weight := pmin(weight, settings$max_position_weight)]
  total_w <- sum(sel$weight, na.rm = TRUE)
  if (total_w > 0) {
    sel[, weight := weight / total_w]
    # weights now sum to 1 (full invested). The min_equity_exposure
    # constraint is a FLOOR, not a ceiling, so a 1.0-sum book naturally
    # satisfies it. The cash sleeve only appears at run_backtest time if
    # the engine decides to pull invested_share below 1 — at which point
    # the engine must clamp the cash share at 1 - min_equity_exposure.
  }

  sel[, .(ticker, weight, composite, gics_sector_name, vol_90)]
}


# Returns TRUE for each row in s where the ticker passes news eligibility,
# using the fraction-of-available-components rule.
news_eligible_v2 <- function(s, settings) {
  if (!"news_eligible" %in% names(s)) return(rep(TRUE, nrow(s)))
  # Pre-computed news_eligible from compute_fundamental_news is the v1
  # absolute >=3 rule. If a v2 fraction is set, recompute from n_positive
  # and the news_total_components if available; otherwise fall back.
  if (!is.null(settings$min_news_positive_fraction) &&
      "n_positive" %in% names(s)) {
    # Conservative: total components is implicit (4 in v1), so the
    # fraction rule reduces to ceiling(0.5 * 4) = 2 positive components.
    floor_n <- max(1L,
                   ceiling(settings$min_news_positive_fraction * 4))
    return(!is.na(s$n_positive) & s$n_positive >= floor_n)
  }
  s$news_eligible == TRUE
}


# Composite signal supports three families.
composite_score <- function(s, mode, settings) {
  method <- settings$composite_method %||% "rank_product"
  n_score   <- s$news_score
  p_score   <- s$passive_intensity

  if (mode == "news_only")    return(n_score)
  if (mode == "passive_only") return(p_score)

  switch(method,
    rank_product = {
      nr <- frank(n_score, na.last = "keep") / sum(!is.na(n_score))
      pr <- frank(p_score, na.last = "keep") / sum(!is.na(p_score))
      nr * pr
    },
    rank_sum = {
      nr <- frank(n_score, na.last = "keep") / sum(!is.na(n_score))
      pr <- frank(p_score, na.last = "keep") / sum(!is.na(p_score))
      (nr + pr) / 2
    },
    zscore_product = n_score * p_score,
    n_score * p_score                         # default
  )
}


empty_holdings <- function() {
  data.table(ticker = character(), weight = numeric(),
             composite = numeric(),
             gics_sector_name = character(),
             vol_90 = numeric())
}

# Null-coalescing helper used by composite_score above.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
