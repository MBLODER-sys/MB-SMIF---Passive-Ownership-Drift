# R/backtest.R
# Walk-forward backtest engine. Rebalances quarterly using the composite
# signal, applies transaction-cost drag, and compounds daily returns.
# Lookahead is avoided by staging today's decision and applying it on the
# next trading day's open.

suppressPackageStartupMessages({
  library(data.table)
})

run_backtest <- function(rebalance_dates,
                         scores_dt,
                         prices_dt,
                         spx_sector_weights_dt,
                         cash_dt,
                         settings,
                         mode = c("full", "news_only", "passive_only")) {

  mode <- match.arg(mode)
  # Local null-coalescing helper (avoids depending on rlang).
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  rebalance_dates <- sort(unique(as.Date(rebalance_dates)))
  prices_dt <- copy(prices_dt)
  prices_dt[, date := as.Date(date)]
  setkey(prices_dt, ticker, date)

  trading_days <- sort(unique(prices_dt$date))

  # State.
  current_weights   <- data.table(ticker = character(), weight = numeric())
  prev_holdings     <- NULL
  next_rebal_idx    <- 1L
  pending_target    <- NULL
  pending_tc_drag   <- 0

  daily_results <- vector("list", length(trading_days))

  for (i in seq_along(trading_days)) {
    d <- trading_days[i]

    # Apply yesterday's pending decision FIRST (lookahead guard).
    tc_drag_today <- 0
    if (!is.null(pending_target)) {
      current_weights <- pending_target
      tc_drag_today   <- pending_tc_drag
      pending_target  <- NULL
      pending_tc_drag <- 0
    }

    # Quarterly rebalance? Build target, stage for tomorrow.
    if (next_rebal_idx <= length(rebalance_dates) &&
        d >= rebalance_dates[next_rebal_idx]) {
      rd <- rebalance_dates[next_rebal_idx]
      target <- build_portfolio(
        rebalance_date        = rd,
        scores_dt             = scores_dt,
        prices_dt             = prices_dt,
        spx_sector_weights_dt = spx_sector_weights_dt,
        settings              = settings,
        prev_holdings         = prev_holdings,
        mode                  = mode
      )

      pending_tc_drag <- compute_tcost_drag(
        prev_holdings %||% data.table(ticker = character(), weight = numeric()),
        target[, .(ticker, weight)],
        settings$tcost_bps_per_side
      )
      pending_target <- target[, .(ticker, weight)]
      prev_holdings  <- pending_target
      next_rebal_idx <- next_rebal_idx + 1L
    }

    # Daily return: weighted asset returns + cash on the unallocated share.
    if (nrow(current_weights) > 0) {
      day_rets <- prices_dt[date == d & ticker %in% current_weights$ticker,
                            .(ticker, ret)]
      m <- merge(current_weights, day_rets, by = "ticker", all.x = TRUE)
      m[is.na(ret), ret := 0]
      port_ret_invested <- sum(m$weight * m$ret, na.rm = TRUE)
      invested_share    <- sum(current_weights$weight, na.rm = TRUE)
    } else {
      port_ret_invested <- 0
      invested_share    <- 0
    }
    cash_rate_daily <- cash_dt[date == d, daily_rate]
    if (length(cash_rate_daily) == 0) cash_rate_daily <- 0
    cash_share <- 1 - invested_share

    port_ret <- port_ret_invested +
                cash_share * cash_rate_daily -
                tc_drag_today

    daily_results[[i]] <- data.table(date = d, ret = port_ret)
  }

  out <- rbindlist(daily_results, use.names = TRUE, fill = TRUE)
  out[, equity := cumprod(1 + ret)]
  out[, mode := mode]
  out[]
}

# Half of one-way turnover * 2 sides * bps drag.
compute_tcost_drag <- function(prev_weights, new_weights, tcost_bps_per_side) {
  if (nrow(prev_weights) == 0 && nrow(new_weights) == 0) return(0)
  all_t <- unique(c(prev_weights$ticker, new_weights$ticker))
  prev  <- setNames(prev_weights$weight, prev_weights$ticker)
  new   <- setNames(new_weights$weight,  new_weights$ticker)
  diffs <- vapply(all_t, function(t) {
    p <- as.numeric(prev[t]); n <- as.numeric(new[t])
    abs((if (is.na(n)) 0 else n) - (if (is.na(p)) 0 else p))
  }, numeric(1))
  turnover_one_way <- sum(diffs) / 2
  turnover_one_way * 2 * (tcost_bps_per_side / 10000)
}

# Build per-rebalance sector weights for the index for sector-neutrality.
build_spx_sector_weights <- function(universe_dt, rebalance_dates) {
  setDT(universe_dt)
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
    snap <- universe_dt[snapshot_date <= rd,
                        .SD[snapshot_date == max(snapshot_date)]]
    sec_w <- snap[, .(spx_weight = sum(get(weight_col), na.rm = TRUE) / 100),
                  by = gics_sector_name]
    sec_w[, rebalance_date := rd]
    out_rows[[i]] <- sec_w
  }
  rbindlist(out_rows, use.names = TRUE, fill = TRUE)
}
