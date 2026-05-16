# config/bbg_fields.R
# Centralized Bloomberg field mappings, grouped by extraction phase.
# Reference: README sections 2 (Signal Construction) and 3 (Data Architecture).

bbg_fields <- list(

  # Universe membership
  universe = list(
    members_field = "INDX_MWEIGHT_HIST",     # bds() with END_DATE_OVERRIDE
    index         = "SPX Index"
  ),

  # Daily price / market data.
  # Trimmed to the four fields actually used by the v1 backtest:
  #   PX_LAST       -> daily returns (build_daily_returns)
  #   PX_VOLUME     -> turnover numerator (passive proxy)
  #   EQY_SH_OUT    -> turnover fallback denominator
  #   CUR_MKT_CAP   -> size signal within sector (passive proxy)
  # PX_OPEN/HIGH/LOW are not referenced anywhere in the simplified
  # strategy; pulling them inflates the parquet by ~40% with no benefit.
  prices = c(
    "PX_LAST", "PX_VOLUME", "EQY_SH_OUT", "CUR_MKT_CAP"
  ),

  # Quality screen fundamentals (point-in-time).
  # Note: GROSS_PROFIT_MARGIN is not a valid BBG field — compute gross
  # profitability locally as GROSS_PROFIT / BS_TOT_ASSET.
  # EARN_GROWTH_TTM_STDEV is not universally available; if present the
  # quality screen uses it, otherwise it degrades to a 3-component score.
  quality = c(
    "RETURN_COM_EQY",
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

  # Fundamental news signal inputs (4 components, multi-signal confirmation).
  news = c(
    "BEST_EPS",
    "IS_EPS",
    "BEST_SALES",
    "SALES_REV_TURN",
    "OPER_MARGIN",
    "BEST_EPS_REVISION_RATIO_1M"
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

  # Macro / benchmark inputs. No regime layer in v1, so this is just the
  # primary benchmark, the equal-weight comparator, and the cash rate.
  macro_tickers = list(
    spx_tr    = "SPXT Index",       # S&P 500 Total Return Index
    spxew     = "SPXEW Index",      # equal-weight S&P 500 (use SPXEWTR for TR if entitled)
    cash_rate = "USGG3M Index"      # 3-month T-bill
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
