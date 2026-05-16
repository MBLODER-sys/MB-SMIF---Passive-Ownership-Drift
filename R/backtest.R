# R/backtest.R
# Main backtest engine. Walks forward through rebalance dates, builds the
# target portfolio for each rebalance, computes daily returns, applies the
# regime overlay, applies the daily VIX override, and accumulates returns
# net of transaction costs.

suppressPackageStartupMessages({
  library(data.table)
})

run_backtest <- function(rebalance_dates,
                         scores_dt,
                         prices_dt,
                         spx_sector_weights_dt,
                         passive_intensity_dt,
                         regime_dt,
                         macro_dt,
                         cash_dt,                # daily cash rate series
                         settings,
                         thresholds,
                         mode = c("full", "news_only", "passive_only", "static")) {

  mode <- match.arg(mode)
  # build_portfolio() and apply_regime_overlay() come from R/portfolio.R;
  # sourced by main.R.

  # Local null-coalescing helper (no rlang dependency).
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  rebalance_dates <- sort(unique(as.Date(rebalance_dates)))
  prices_dt[, date := as.Date(date)]
  setkey(prices_dt, ticker, date)

  # Calendar of trading days (any date appearing in SPX history).
  trading_days <- sort(unique(
    prices_dt[ticker == settings$index_ticker, date]
  ))
  if (length(trading_days) == 0) {
    trading_days <- sort(unique(prices_dt$date))
  }

  # State.
  current_weights <- data.table(ticker = character(), weight = numeric())
  cash_weight <- 1
  prev_holdings_raw <- NULL          # pre-overlay weights, for next-rebal TC compare
  next_rebal_idx <- 1L
  vix_override_active <- FALSE
  vix_release_consecutive <- 0L
  pending_target <- NULL             # decision made today, applied tomorrow
  pending_cash_weight <- NULL
  pending_raw_target <- NULL
  pending_tc_drag <- 0
  daily_results <- vector("list", length(trading_days))

  for (i in seq_along(trading_days)) {
    d <- trading_days[i]

    # 0. Apply yesterday's pending decision FIRST. This is the lookahead guard:
    # signals are computed using data through close on rebal day, but the
    # trade only takes effect at the next open. Returns earned today are
    # still on yesterday's book.
    if (!is.null(pending_target)) {
      current_weights <- pending_target
      cash_weight     <- pending_cash_weight
      prev_holdings_raw <- pending_raw_target
      tc_drag_today   <- pending_tc_drag
      pending_target  <- NULL
      pending_cash_weight <- NULL
      pending_raw_target  <- NULL
      pending_tc_drag <- 0
    } else {
      tc_drag_today <- 0
    }

    # 1. Quarterly rebalance? Build the target but stage for tomorrow.
    if (next_rebal_idx <= length(rebalance_dates) &&
        d >= rebalance_dates[next_rebal_idx]) {
      rd <- d                        # use actual trading day as rebal effective date
      raw_target <- build_portfolio(
        rebalance_date = rebalance_dates[next_rebal_idx],
        scores_dt = scores_dt,
        prices_dt = prices_dt,
        spx_sector_weights_dt = spx_sector_weights_dt,
        settings = settings,
        prev_holdings = prev_holdings_raw,
        mode = mode
      )

      # 2. Regime overlay (skip in mode = "news_only", "passive_only", "static").
      target <- raw_target
      if (mode == "full" && nrow(target) > 0 && nrow(regime_dt) > 0) {
        regime_state <- tail(regime_dt[date <= rd, regime], 1)
        if (length(regime_state) == 0 || is.na(regime_state))
          regime_state <- thresholds$default_state
        target <- apply_regime_overlay(target, regime_state,
                                       passive_intensity_dt,
                                       rebalance_dates[next_rebal_idx],
                                       thresholds)
        cash_weight_rb <- if ("cash_weight" %in% names(target))
                            target$cash_weight[1] else 0
      } else {
        cash_weight_rb <- 0
      }

      # 3. Transaction costs computed on PRE-OVERLAY turnover so that overlay-
      # driven scaling doesn't show up as a trade cost. (Cash sleeve changes
      # don't trade individual names.)
      tc_drag_pending <- compute_tcost_drag(
        prev_holdings_raw %||% data.table(ticker = character(), weight = numeric()),
        raw_target[, .(ticker, weight)],
        settings$tcost_bps_per_side
      )

      pending_target      <- target[, .(ticker, weight)]
      pending_cash_weight <- cash_weight_rb
      pending_raw_target  <- raw_target[, .(ticker, weight)]
      pending_tc_drag     <- tc_drag_pending
      next_rebal_idx <- next_rebal_idx + 1L
    }
    tc_drag <- tc_drag_today

    # 4. Daily VIX override (only in full mode).
    if (mode == "full" && settings$daily_vix_override) {
      vix_today <- macro_dt[series_name == "vix" & date == d, px_last]
      # 5-trading-day lookback rather than 5-calendar-day to make the spike
      # detector calendar-agnostic. Trading-day index is used since
      # macro_dt has only daily observations.
      vix_window <- macro_dt[series_name == "vix" & date <= d, px_last]
      vix_5d_ago <- if (length(vix_window) > 5)
                      vix_window[length(vix_window) - 5] else NA_real_
      vix_jump <- if (length(vix_today) > 0 && !is.na(vix_5d_ago))
                    vix_today - vix_5d_ago else 0

      # State machine: missing data is treated as "no signal" — neither trip
      # nor reset — so a brief VIX gap doesn't break the dwell-day counter.
      if (length(vix_today) == 0 || is.na(vix_today)) {
        # No-op; preserve current state.
      } else if (!vix_override_active &&
                 (vix_today > settings$vix_spike_level ||
                  vix_jump  > settings$vix_spike_5d_jump)) {
        vix_override_active <- TRUE
        vix_release_consecutive <- 0L
      } else if (vix_override_active &&
                 vix_today < settings$vix_release_level) {
        vix_release_consecutive <- vix_release_consecutive + 1L
        if (vix_release_consecutive >= settings$vix_release_dwell_days) {
          vix_override_active <- FALSE
          vix_release_consecutive <- 0L
        }
      } else if (vix_override_active &&
                 vix_today >= settings$vix_release_level) {
        # Above release level — reset the dwell counter but stay active.
        vix_release_consecutive <- 0L
      }

      override_invested <- if (vix_override_active)
                             settings$vix_override_target_invested else 1
    } else {
      override_invested <- 1
    }

    # 5. Daily return: weighted asset returns + cash interest. Weights sum to
    # (1 - cash_weight) after the regime overlay; override_invested scales the
    # invested book further on a VIX-spike day.
    day_rets <- prices_dt[date == d & ticker %in% current_weights$ticker,
                          .(ticker, ret)]
    if (nrow(current_weights) > 0) {
      m <- merge(current_weights, day_rets, by = "ticker", all.x = TRUE)
      # A held name missing a return today (suspended, just listed, etc.)
      # contributes 0 to the day's return.
      m[is.na(ret), ret := 0]
      port_ret_invested <- sum(m$weight * m$ret, na.rm = TRUE)
    } else {
      port_ret_invested <- 0
    }

    cash_rate_daily <- cash_dt[date == d, daily_rate]
    if (length(cash_rate_daily) == 0) cash_rate_daily <- 0

    invested_share <- (1 - cash_weight) * override_invested
    cash_share     <- 1 - invested_share

    port_ret <- override_invested * port_ret_invested +
                cash_share         * cash_rate_daily -
                tc_drag

    regime_today <- if (mode == "full" && nrow(regime_dt) > 0) {
      r <- tail(regime_dt[date <= d, regime], 1)
      if (length(r) == 0) NA_character_ else r
    } else NA_character_

    daily_results[[i]] <- data.table(
      date = d,
      ret  = port_ret,
      gross_invested = invested_share,
      regime = regime_today,
      vix_override = vix_override_active
    )
  }

  out <- rbindlist(daily_results, use.names = TRUE, fill = TRUE)
  out[, equity := cumprod(1 + ret)]
  out[, mode := mode]
  out[]
}


# Approximate transaction-cost drag: half of turnover times two-side bps.
compute_tcost_drag <- function(prev_weights, new_weights, tcost_bps_per_side) {
  if (nrow(prev_weights) == 0 && nrow(new_weights) == 0) return(0)
  all_t <- unique(c(prev_weights$ticker, new_weights$ticker))
  prev <- setNames(prev_weights$weight, prev_weights$ticker)
  new  <- setNames(new_weights$weight,  new_weights$ticker)
  diffs <- sapply(all_t, function(t) abs(as.numeric(new[t]) - as.numeric(prev[t])))
  diffs[is.na(diffs)] <- 0
  turnover_one_way <- sum(diffs) / 2
  turnover_one_way * 2 * (tcost_bps_per_side / 10000)
}


# Build per-rebalance sector weights for the index, so the portfolio knows the
# target sector exposures to neutralise toward.
build_spx_sector_weights <- function(universe_dt, rebalance_dates) {
  setDT(universe_dt)
  # Try common column names from BDS INDX_MWEIGHT_HIST.
  weight_col <- intersect(c("percent_weight", "weight", "actual_weight"),
                          names(universe_dt))[1]
  if (is.na(weight_col)) {
    return(data.table(rebalance_date = as.Date(character()),
                      gics_sector_name = character(),
                      spx_weight = numeric()))
  }
  out_rows <- vector("list", length(rebalance_dates))
  for (i in seq_along(rebalance_dates)) {
    rd <- as.Date(rebalance_dates[i])
    snap <- universe_dt[snapshot_date <= rd, .SD[snapshot_date == max(snapshot_date)]]
    sec_w <- snap[, .(spx_weight = sum(get(weight_col), na.rm = TRUE) / 100),
                  by = gics_sector_name]
    sec_w[, rebalance_date := rd]
    out_rows[[i]] <- sec_w
  }
  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
