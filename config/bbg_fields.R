# config/bbg_fields.R
# Centralized Bloomberg field mappings, grouped by extraction phase.
# Reference: README sections 2 (Signal Construction) and 3 (Data Architecture).

bbg_fields <- list(

  # Universe membership
  universe = list(
    members_field = "INDX_MWEIGHT_HIST",     # bds() with END_DATE_OVERRIDE
    index         = "SPX Index"
  ),

  # Daily price / market data
  prices = c(
    "PX_LAST", "PX_OPEN", "PX_HIGH", "PX_LOW", "PX_VOLUME",
    "EQY_SH_OUT", "CUR_MKT_CAP", "VOLATILITY_90D", "BETA_ADJ_OVERRIDABLE",
    "TOT_RETURN_INDEX_GROSS_DVDS"            # total-return index for performance attribution
  ),

  # Quality screen fundamentals (point-in-time)
  quality = c(
    "RETURN_COM_EQY",
    "GROSS_PROFIT_MARGIN",
    "GROSS_PROFIT",
    "BS_TOT_ASSET",
    "TOT_DEBT_TO_TOT_EQY",
    "EARN_GROWTH_TTM_STDEV",
    "CF_CASH_FROM_OPER",
    "ASSET_TURNOVER"
  ),

  # Value screen fundamentals
  value = c(
    "PE_RATIO",
    "PX_TO_BOOK_RATIO",
    "EV_TO_T12M_EBITDA",
    "CF_FREE_CASH_FLOW",
    "FCF_YIELD",
    "EARN_YLD"
  ),

  # Fundamental news signal inputs.
  # Note: EARN_ANN_DT_TIME_HIST_WITH_EPS is a bulk reference field and must
  # be pulled via bds(); it is intentionally excluded from this BDH pull.
  news = c(
    "BEST_EPS",
    "IS_EPS",
    "BEST_SALES",
    "SALES_REV_TURN",
    "OPER_MARGIN",
    "BEST_EPS_REVISION_RATIO_1M",
    "LATEST_ANNOUNCEMENT_DT"
  ),

  # Passive ownership (THE key field set)
  ownership = c(
    "PASSIVELY_HELD_PCT_OUT",
    "ACTIVELY_HELD_PCT_OUT",
    "INSTITUTIONAL_PERCENT_HELD",
    "EQY_FLOAT"
  ),

  # GICS sector membership (for sector neutrality and within-sector z-scoring)
  reference = c(
    "GICS_SECTOR_NAME",
    "GICS_INDUSTRY_NAME",
    "ID_BB_GLOBAL"          # stable identifier across renamings
  ),

  # Macro / regime inputs
  macro_tickers = list(
    vix          = "VIX Index",
    hy_oas       = "LF98TRUU Index",       # Bloomberg US Corporate HY YTW spread
    yc_slope     = "USYC3M10 Index",       # 10Y - 3M slope
    yc_10y       = "USGG10YR Index",
    yc_3m        = "USGG3M Index",
    move         = "MOVE Index",
    spx          = "SPX Index",
    spxew        = "SPXEW Index",   # equal-weight (price). Use SPXEWTR for TR if entitled.
    spx_tr       = "SPXT Index",    # S&P 500 Total Return Index
    cash_rate    = "USGG3M Index"
  ),
  macro_field = "PX_LAST",

  # Periodicity options used in BDH calls
  options_quarterly = c(periodicitySelection = "QUARTERLY",
                        periodicityAdjustment = "FISCAL"),
  options_monthly   = c(periodicitySelection = "MONTHLY",
                        periodicityAdjustment = "ACTUAL"),
  options_daily     = c(periodicitySelection = "DAILY",
                        periodicityAdjustment = "ACTUAL")
)
