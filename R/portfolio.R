# R/portfolio.R
# Portfolio construction: top-quintile selection, inverse-vol weighting,
# sector-neutrality constraint, position cap, turnover buffer.

suppressPackageStartupMessages({
  library(data.table)
})

build_portfolio <- function(rebalance_date,
                            scores_dt,
                            prices_dt,
                            spx_sector_weights_dt,
                            settings,
                            prev_holdings = NULL,
                            mode = c("full", "news_only", "passive_only")) {

  mode <- match.arg(mode)
  rd <- as.Date(rebalance_date)

  # `rd` is a function-scope scalar; `rebalance_date` is a column on
  # scores_dt and spx_sector_weights_dt. Because the parameter was
  # renamed to `rd`, plain references find the scalar in calling scope
  # with no NSE shadowing risk.
  s <- copy(scores_dt[rebalance_date == rd])
  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Hard exclusions.
  s <- s[!is.na(quality_decile) & quality_decile > settings$quality_exclude_decile]
  s <- s[!is.na(value_decile)   & value_decile   > settings$value_exclude_decile]
  # News-eligibility only applies when the mode actually uses the news signal.
  if (mode != "passive_only") {
    s <- s[news_eligible == TRUE]
  }
  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Composite by mode.
  s[, composite := switch(mode,
                          full         = news_score * passive_intensity,
                          news_only    = news_score,
                          passive_only = passive_intensity)]
  s <- s[!is.na(composite)]
  if (nrow(s) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Turnover buffer.
  s[, comp_pctile := frank(composite, na.last = "keep") / .N]
  incumbent_tickers <- if (!is.null(prev_holdings)) prev_holdings$ticker else character()
  s[, is_incumbent := ticker %in% incumbent_tickers]
  s[, selected := FALSE]
  s[is_incumbent  & comp_pctile >= settings$hold_decay_pct,      selected := TRUE]
  s[!is_incumbent & comp_pctile >= settings$entry_threshold_pct, selected := TRUE]

  # If selection too small (early periods or first rebalance), fall back to
  # the top-quintile rule.
  if (sum(s$selected) < 30) {
    s[, selected := comp_pctile >= (1 - settings$long_quintile_pct)]
  }

  sel <- s[selected == TRUE]
  if (nrow(sel) == 0) return(data.table(ticker = character(), weight = numeric()))

  # Inverse-volatility weighting using 90D realized vol.
  vol_window_start <- rd - settings$vol_window_days * 1.5
  vol <- prices_dt[ticker %in% sel$ticker & date <= rd & date >= vol_window_start,
                   .(vol_90 = stats::sd(ret, na.rm = TRUE) * sqrt(252)),
                   by = ticker]
  sel <- merge(sel, vol, by = "ticker", all.x = TRUE)
  sel[is.na(vol_90) | vol_90 == 0, vol_90 := stats::median(vol_90, na.rm = TRUE)]
  sel[is.na(vol_90),               vol_90 := 0.20]   # 20% annualised fallback
  sel[, inv_vol_weight := 1 / vol_90]

  # Sector-neutrality with tolerance band.
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

  # Position cap, then renormalise across the full book.
  sel[, weight := pmin(weight, settings$max_position_weight)]
  total_w <- sum(sel$weight, na.rm = TRUE)
  if (total_w > 0) sel[, weight := weight / total_w]

  sel[, .(ticker, weight, composite, gics_sector_name, vol_90)]
}
