# R/portfolio.R
# Portfolio construction: top-quintile selection, inverse-vol weighting,
# sector-neutrality constraint, position cap, turnover buffer.

suppressPackageStartupMessages({
  library(data.table)
})

build_portfolio <- function(rebalance_date,
                            scores_dt,             # has rebalance_date, ticker, composite, news_score, passive_intensity, news_eligible, quality_decile, value_decile, gics_sector_name
                            prices_dt,             # daily prices, for vol calc
                            spx_sector_weights_dt, # snapshot of S&P 500 sector weights at rebalance_date
                            settings,
                            prev_holdings = NULL,  # data.table(ticker, weight) from previous rebal
                            mode = c("full", "news_only", "passive_only", "static")) {

  mode <- match.arg(mode)
  rd <- as.Date(rebalance_date)

  # Use ..rd to escape data.table NSE and reference the function-scope scalar
  # rather than the in-table column.
  s <- copy(scores_dt[rebalance_date == ..rd])
  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Apply hard exclusions.
  s <- s[!is.na(quality_decile) & quality_decile > settings$quality_exclude_decile]
  s <- s[!is.na(value_decile)   & value_decile   > settings$value_exclude_decile]
  s <- s[news_eligible == TRUE | (mode == "passive_only")]

  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Composite by mode. For news_only mode, require a non-NA news_score.
  s[, composite := switch(
    mode,
    full         = news_score * passive_intensity,
    news_only    = news_score,
    passive_only = passive_intensity,
    static       = news_score * passive_intensity
  )]
  s <- s[!is.na(composite)]
  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Turnover buffer: incumbents kept unless < hold_decay_pct;
  # entrants only above entry_threshold_pct.
  s[, comp_pctile := frank(composite, na.last = "keep") / .N]

  incumbent_tickers <- if (!is.null(prev_holdings)) prev_holdings$ticker else character()
  s[, is_incumbent := ticker %in% incumbent_tickers]

  s[, selected := FALSE]
  s[is_incumbent  & comp_pctile >= settings$hold_decay_pct,     selected := TRUE]
  s[!is_incumbent & comp_pctile >= settings$entry_threshold_pct, selected := TRUE]

  # If selection too small (early periods or first rebalance), backfill
  # using the top-quintile rule.
  if (sum(s$selected) < 30) {
    top_q_threshold <- 1 - settings$long_quintile_pct
    s[, selected := comp_pctile >= top_q_threshold]
  }

  sel <- s[selected == TRUE]
  if (nrow(sel) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Inverse-volatility weighting using 90D realized vol.
  vol_window_start <- rd - settings$vol_window_days * 1.5  # buffer for non-trading days
  vol <- prices_dt[ticker %in% sel$ticker & date <= ..rd & date >= vol_window_start,
                   .(vol_90 = stats::sd(ret, na.rm = TRUE) * sqrt(252)),
                   by = ticker]
  sel <- merge(sel, vol, by = "ticker", all.x = TRUE)
  sel[is.na(vol_90) | vol_90 == 0,
      vol_90 := stats::median(vol_90, na.rm = TRUE)]
  if (any(is.na(sel$vol_90))) {
    sel[is.na(vol_90), vol_90 := 0.20]   # 20% annualised fallback if no history
  }

  sel[, inv_vol_weight := (1 / vol_90)]

  # ---- Sector-neutrality with tolerance --------------------------------
  # Step 1: inverse-vol-weighted contribution within sector
  sel[, within_sector_w := inv_vol_weight / sum(inv_vol_weight, na.rm = TRUE),
      by = gics_sector_name]
  # Step 2: target sector exposure = SPX sector weight clipped to
  #         max(0, SPX - tol) .. min(1, SPX + tol)
  sec_w <- copy(spx_sector_weights_dt[rebalance_date == ..rd])
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
    # No sector weight reference — fall back to pure inverse-vol normalised
    # across the full selection.
    sel[, weight := inv_vol_weight / sum(inv_vol_weight, na.rm = TRUE)]
  }

  # Position cap, then renormalise across the full book.
  sel[, weight := pmin(weight, settings$max_position_weight)]
  total_w <- sum(sel$weight, na.rm = TRUE)
  if (total_w > 0) sel[, weight := weight / total_w]

  sel[, .(ticker, weight, composite, gics_sector_name, vol_90)]
}


apply_regime_overlay <- function(target_holdings,
                                 regime_state,
                                 passive_intensity_dt,
                                 rd,
                                 thresholds) {

  if (nrow(target_holdings) == 0) return(target_holdings)
  mult <- thresholds$exposure_multipliers[[regime_state]]
  if (is.null(mult)) mult <- 1.0

  rd <- as.Date(rd)
  # Identify top-quintile-by-passive holdings, scale their weights.
  pi_snap <- passive_intensity_dt[rebalance_date == ..rd]
  if (nrow(pi_snap) == 0) return(target_holdings)
  pi_snap[, passive_pctile := frank(passive_intensity, na.last = "keep") / .N]
  high_passive <- pi_snap[!is.na(passive_pctile) & passive_pctile >= 0.80, ticker]

  tg <- copy(target_holdings)
  tg[, regime_mult := 1.0]
  tg[ticker %in% high_passive, regime_mult := mult]
  tg[, weight := weight * regime_mult]

  # Cash residual: weights now sum to total_w; rest is cash.
  total_w <- sum(tg$weight)
  if (total_w > 1) {
    tg[, weight := weight / total_w]
    tg[, cash_weight := 0]
  } else {
    tg[, cash_weight := max(0, 1 - total_w)]
  }
  tg
}
