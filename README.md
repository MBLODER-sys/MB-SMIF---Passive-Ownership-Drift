# MB-SMIF---Passive-Ownership-Drift
# The Passive Ownership Drift Strategy

A long-only, rules-based equity portfolio strategy implemented in R, sourcing point-in-time data from the Bloomberg Terminal via `Rblpapi`. Built on a behavioural-economics thesis about how the structural rise of passive investing slows fundamental information diffusion into equity prices.

---

## 1. Investment Thesis

### Core Claim

Within the S&P 500, **stocks whose ownership has been disproportionately captured by passive investors experience systematically slower incorporation of fundamental information into prices.** This creates a predictable post-news drift that an active investor can systematically capture.

### The Mechanism

Passive investors — index funds and index-tracking ETFs — hold securities by mandate, not by conviction. They cannot, by construction, respond to firm-specific news: they buy what the index tells them to buy and sell what the index tells them to sell. As passive ownership of a stock rises, the *marginal price-setter* shifts from a broad pool of active, attentive investors to a narrower pool of remaining active holders. The rate of price adjustment to new fundamental information is bounded by the updating speed of that shrinking pool.

This produces three testable predictions:

1. **Post-announcement drift is stronger in high-passive-ownership stocks.** Classic Post-Earnings Announcement Drift (PEAD), first documented by Bernard & Thomas (1989) and long assumed to be arbitraged away, should re-emerge — and intensify — in stocks whose marginal price-setter has thinned.

2. **The drift is asymmetric across regimes.** In calm regimes, slow information diffusion produces clean drift. At inflection points, the same ownership structure produces *fragility* — abrupt repricing as the small pool of active investors all update simultaneously while the large pool of passive holders cannot absorb the flow.

3. **The effect is independent of analyst coverage.** Unlike the older limited-attention literature, this is not a story about *how many eyes are watching*; it is a story about *what fraction of the holder base can act on what they see*. Passive ownership is the structural variable; coverage is a consequence.

### Theoretical Grounding

- **Bernard & Thomas (1989), *Journal of Accounting Research*** — Post-Earnings Announcement Drift foundational paper
- **Hong & Stein (1999), *Journal of Finance*** — Gradual information diffusion and underreaction
- **Hirshleifer & Teoh (2003), *Journal of Accounting and Economics*** — Limited attention and information processing
- **Ben-David, Franzoni & Moussawi (2018), *Journal of Finance*** — ETF ownership and underlying volatility
- **Gabaix & Koijen (2021)** — The Inelastic Markets Hypothesis
- **Coles, Heath & Ringgenberg (2022), *Journal of Financial Economics*** — Indexing and information production
- **Bond & Garcia (2022)** — Implications of passive ownership for price informativeness

### Why This Edge Persists

Three structural reasons it has not been competed away:

1. **Limits to arbitrage.** The very condition that creates the opportunity — passive ownership crowding out active price discovery — also reduces the supply of arbitrage capital allocated to these names.
2. **Mandate constraints.** Passive funds *cannot* trade on this signal even when they observe it. Active long-only managers face career risk in deviating from the cap-weighted benchmark.
3. **Slow-moving phenomenon.** The growth of passive investing is a multi-decade structural shift. The mispricing is not a flash anomaly to be picked off in milliseconds; it is a slow drift over weeks to months, which most quantitative funds and short-horizon traders are not structured to capture.

### Identifying the Marginal Seller

Every alpha trade has a counterparty. In this strategy, the counterparty is:

- **Mandated passive flows** that mechanically buy and sell stocks based on index weight, regardless of fundamentals.
- **Slow-updating active investors** whose models or processes have not yet incorporated the news.
- **Behavioural holders** who anchor to stale prices and underreact to new information.

We are systematically buying from holders who cannot, will not, or have not yet repriced fundamental news.

---

## 2. Strategy Specification

### Universe

- **Investable universe:** Historical S&P 500 constituents, point-in-time.
- **Sample period:** 2005-01-01 to 2024-12-31 (captures GFC, Eurozone crisis, taper tantrum, COVID shock, 2022 inflation regime, 2024 carry unwind — six distinct regimes).
- **Survivorship-bias avoided:** Universe refreshed at each rebalance using Bloomberg's `INDX_MWEIGHT_HIST` to reflect historical membership, not current membership.

### Signal Construction

The portfolio is driven by a **single composite score** combining two interacting components:

#### Component A — Confirmed Fundamental News Score

Captures the *strength and breadth* of recent positive fundamental news. To avoid the measurement problem where low-coverage stocks have noisy analyst consensus estimates, we use **multi-signal confirmation** rather than raw earnings surprise.

A stock receives a high Fundamental News Score if it has, over the most recent reported quarter:

1. **Standardized Unexpected Earnings (SUE)** — actual EPS minus consensus, normalized by the historical standard deviation of surprises (not by consensus level). Bloomberg: derived from `BEST_EPS`, `IS_EPS`, and a rolling sigma of past surprises.
2. **Revenue surprise** — actual revenue vs analyst consensus. Bloomberg: `SALES_REV_TURN` vs `BEST_SALES`.
3. **Margin expansion vs trend** — operating margin in the latest quarter vs the firm's own trailing 3-year trend. Bloomberg: derived from `OPER_MARGIN`.
4. **Guidance signal** — direction of management guidance revisions. Bloomberg: `BEST_EPS_REVISION_RATIO_1M`.
5. **Price confirmation** — non-negative cumulative abnormal return in the 1–3 days after the announcement, confirming the market interpreted the news as positive.

Each component is z-scored cross-sectionally and winsorized at the 1st/99th percentile. The Fundamental News Score is the average of these z-scores. **A minimum of 3 of the 5 components must be positive** for a stock to be eligible for the long portfolio — this is the multi-signal confirmation requirement that filters out type-(b) artifacts where a "surprise" is just measurement noise.

#### Component B — Passive Ownership Intensity Score

Measures the degree to which the stock's marginal holder is non-attentive.

Constructed from:

1. **Bloomberg `PASSIVELY_HELD_PCT_OUT`** — direct passive ownership measure (the key field).
2. **Sector-relative normalization** — passive ownership levels differ systematically across sectors (utilities ≫ tech historically). Z-score is computed *within sector* to avoid the strategy becoming a sector bet on staples and utilities.
3. **Growth in passive ownership** — first difference over trailing 12 months. Stocks where passive share is *rising* most rapidly have the most newly-degraded price discovery.

The Passive Ownership Intensity Score is z-scored cross-sectionally within sector, winsorized at the 1st/99th percentile.

#### Composite Score

The interaction is the alpha source:

```
Composite Score = Fundamental News Score × Passive Ownership Intensity Score
                  (multiplicative, sign-preserved)
```

A stock scores high only when **both** conditions hold: it has experienced confirmed positive fundamental news **and** its ownership structure implies that information will diffuse slowly. The multiplicative form is essential — a high-news, low-passive stock should not be selected (no drift to capture); a high-passive, no-news stock should not be selected (no information to be slow about).

### Selection Rule

- **Long portfolio:** Top quintile (~100 stocks) by Composite Score among S&P 500 members at each rebalance.
- **Eligibility cuts (hard exclusions, applied before scoring):**
  - Excluded if in the bottom decile on the **Quality screen** (combined ROE, gross profitability, debt/equity, earnings stability — value-trap and quality-blowup defence)
  - Excluded if in the bottom decile on the **Value screen** (combined earnings yield, book-to-market, FCF yield — euphoric-bubble defence)
  - Excluded if 3-of-5 fundamental news components are not positive (multi-signal confirmation)
- **Final portfolio:** ~50–75 stocks after constraints below.

Note: Quality and Value are **risk controls, not alpha generators**. They keep the strategy out of tail risks; they do not generate the edge.

### Position Sizing & Weighting

- **Inverse-volatility weighted** within sector buckets — lower-vol stocks receive larger weights.
- **Vol measure:** 90-day realized volatility.
- **Sector neutrality:** Sector weights held within ±3% of S&P 500 sector weights, enforced after composite ranking and inverse-vol weighting.
- **Position cap:** 5% maximum per stock.
- **Turnover buffer:** A held position remains held unless its Composite Score falls below the 25th percentile; a non-held stock only enters if it ranks above the 80th percentile. Prevents marginal score changes from triggering trades.

### Regime Overlay

The thesis predicts **different drift behaviour in different regimes**:

- **Calm regimes:** Slow information diffusion produces clean drift. The strategy operates at full conviction.
- **Stressed regimes:** Forced selling and liquidity vacuums in high-passive-ownership names invert the drift — exactly the fragility observed at inflection points. The strategy should *de-risk* high-passive exposure precisely when the mechanism that creates the edge reverses sign.

#### Regime Classifier

A four-state model uses:

- **VIX** (level and percentile) — Bloomberg: `VIX Index`
- **High-yield credit spreads** — Bloomberg: `LF98TRUU Index` OAS
- **Yield curve slope** (10Y – 3M) — Bloomberg: `USYC3M10 Index`
- **Market breadth** (% of S&P 500 above 200-day MA) — computed locally from price data

States:

| Regime | Trigger | Strategy Behaviour |
|---|---|---|
| **Calm-Expansion** | Default; low VIX, tight spreads, positive slope, broad participation | Full exposure, baseline weights |
| **Late-Cycle** | Curve flattening/inverted, breadth deteriorating | Reduce exposure to highest-passive-ownership quintile by 30% |
| **Stressed** | VIX > 25 AND (spreads above 80th pctile OR breadth < 30%) | Reduce exposure to highest-passive-ownership quintile by 60%; tilt toward lower-passive names within selected set |
| **Recovery** | Coming out of Stressed: VIX falling from peak, breadth rising through 50% | Restore exposure; mildly tilt toward stocks that drew down most in Stressed phase |

A **60-day minimum dwell time** prevents regime flickering. State transitions can only occur after the trigger conditions are sustained.

### Rebalancing Cadence

Three nested cadences operating on different signal frequencies:

| Cadence | When | What Happens |
|---|---|---|
| **Quarterly** | First trading day of Jan/Apr/Jul/Oct | Full universe refresh, score recomputation, full portfolio reconstruction |
| **Monthly** | First trading day of each month | Regime classifier re-evaluated; pillar/overlay weights adjusted; position sizes scaled — *no new entries or exits* |
| **Daily** | Continuous | Event-triggered volatility override: if VIX > 30 OR VIX rises >10 points in 5 trading days, scale all positions to 80% of target with 20% held in cash equivalents (`^IRX`). Override reverses when VIX falls back below 25 and remains there for 5 consecutive days. |

### Transaction Costs

- **Modelled at 10 bps per side** (round-trip 20 bps) in the base case.
- **Stress-tested at 5, 25, and 50 bps** in robustness analysis.
- Cash earns the 3-month T-bill rate (Bloomberg: `USGG3M Index`).

---

## 3. Data Architecture

### Bloomberg Stage (runs on terminal via `Rblpapi`)

All data extraction happens on a Bloomberg-licensed machine. Extracted data is cached as `.parquet` (via `arrow`) or `.rds` files, then transferred to the local analysis machine. No Bloomberg dependency exists in the analysis stage.

**Data extracted:**

#### Universe
- `INDX_MWEIGHT_HIST` on `SPX Index` at each quarter-end from 2005-01-01 to 2024-12-31 → historical S&P 500 constituents panel with date, ticker, weight, GICS sector.

#### Prices & Market Data
- Daily OHLCV for all historical S&P 500 tickers
- Fields: `PX_LAST`, `PX_OPEN`, `PX_HIGH`, `PX_LOW`, `PX_VOLUME`, `EQY_SH_OUT`, `CUR_MKT_CAP`, `VOLATILITY_90D`, `BETA_ADJ_OVERRIDABLE`

#### Fundamentals (Quality + Value screens, point-in-time)
- `RETURN_COM_EQY`, `GROSS_PROFIT_MARGIN`, `GROSS_PROFIT`, `BS_TOT_ASSET`, `TOT_DEBT_TO_TOT_EQY`, `EARN_GROWTH_TTM_STDEV`, `CF_CASH_FROM_OPER`, `ASSET_TURNOVER`
- `PE_RATIO`, `PX_TO_BOOK_RATIO`, `EV_TO_T12M_EBITDA`, `CF_FREE_CASH_FLOW`, `FCF_YIELD`, `EARN_YLD`
- Quarterly periodicity, with `LATEST_ANNOUNCEMENT_DT` override for proper PIT alignment

#### Fundamental News Signals
- `BEST_EPS` (consensus EPS estimate, historical)
- `IS_EPS` (actual reported EPS)
- `BEST_SALES`, `SALES_REV_TURN`
- `OPER_MARGIN` (quarterly history for trend computation)
- `BEST_EPS_REVISION_RATIO_1M`
- Earnings announcement dates for windowing the 1-3 day price confirmation check

#### Passive Ownership (the key field)
- `PASSIVELY_HELD_PCT_OUT` — direct passive ownership measure, historical quarterly snapshots
- `ACTIVELY_HELD_PCT_OUT` — complementary check
- `INSTITUTIONAL_PERCENT_HELD` — broader institutional measure
- `EQY_FLOAT` — for sanity-checking ownership ratios

#### Macro / Regime Inputs
- `VIX Index` — daily
- `LF98TRUU Index` — daily OAS
- `USYC3M10 Index` (or compute from `USGG10YR Index` and `USGG3M Index`)
- `MOVE Index` — bond volatility as auxiliary regime signal
- `SPX Index`, `SPXEW Index` — primary and equal-weighted benchmarks
- `USGG3M Index` — cash rate

#### Factor Returns (for performance attribution)
- Fama-French 5 factors, Momentum, BAB — downloaded from Kenneth French data library and AQR's public site (no Bloomberg needed)

### Storage Convention

```
data/raw/                  Bloomberg extractions, one parquet per logical dataset
data/processed/            Cleaned, PIT-aligned panels ready for analysis
data/universe/             Historical S&P 500 membership
data/cache/                Intermediate computations (factor z-scores, regime series)
```

A `data/manifest.yaml` records the extraction date, ticker count, and field coverage of each dataset — every backtest run is tagged against a manifest version for reproducibility.

---

## 4. Repository Structure

```
passive_ownership_drift/
├── README.md                    # This file
├── DESCRIPTION                  # R package metadata (if treating as package)
├── renv.lock                    # Reproducible R environment via renv
├── config/
│   ├── settings.R               # Dates, thresholds, position caps, costs
│   ├── regime_thresholds.R      # Regime classifier parameters
│   └── bbg_fields.R             # Centralized Bloomberg field mappings
├── data/
│   ├── raw/                     # Bloomberg pulls (gitignored)
│   ├── processed/               # Cleaned panels
│   ├── universe/                # Historical S&P 500 membership
│   ├── cache/                   # Intermediate cached objects
│   └── manifest.yaml            # Data version tracking
├── bloomberg/                   # RUNS ON BLOOMBERG TERMINAL ONLY
│   ├── 01_extract_universe.R
│   ├── 02_extract_prices.R
│   ├── 03_extract_fundamentals.R
│   ├── 04_extract_news_signals.R
│   ├── 05_extract_ownership.R
│   ├── 06_extract_macro.R
│   ├── 07_extract_factors.R
│   ├── run_all_extractions.R    # Orchestrator
│   └── README_BLOOMBERG.md      # Terminal-specific instructions
├── R/                           # RUNS LOCALLY — no Bloomberg dependency
│   ├── data_loader.R            # Load cached data; PIT alignment
│   ├── signals/
│   │   ├── fundamental_news.R   # SUE, revenue surprise, margin trend, etc.
│   │   ├── passive_intensity.R  # Passive ownership composite
│   │   ├── quality.R            # Risk-control screen
│   │   └── value.R              # Risk-control screen
│   ├── regime.R                 # Four-state classifier
│   ├── portfolio.R              # Selection, weighting, constraints
│   ├── backtest.R               # Main backtest engine
│   ├── metrics.R                # Performance and risk metrics
│   └── visualization.R          # All plots
├── analysis/
│   ├── 01_data_validation.Rmd
│   ├── 02_signal_diagnostics.Rmd
│   ├── 03_regime_analysis.Rmd
│   ├── 04_backtest_results.Rmd
│   └── 05_robustness_checks.Rmd
├── output/
│   ├── figures/                 # PNG + PDF, 300dpi
│   ├── tables/                  # CSV
│   └── reports/                 # Rendered Rmd outputs
├── tests/
│   ├── testthat/
│   │   ├── test-signals.R
│   │   ├── test-regime.R
│   │   └── test-portfolio.R
├── extract.R                    # Bloomberg pipeline entry point
└── main.R                       # Local pipeline entry point
```

---

## 5. Comparators

The backtest produces results for **five portfolios** to enable clean attribution of where the alpha comes from:

| Portfolio | Purpose |
|---|---|
| **Strategy (full)** | The full Passive Ownership Drift strategy with regime overlay |
| **News-only** | Fundamental News signal alone, no passive ownership interaction. Isolates the contribution of the passive ownership variable. |
| **Passive-only** | Passive ownership intensity alone, no news signal. Isolates the contribution of the fundamental news component. |
| **Static** | Full composite but no regime overlay. Isolates the contribution of regime conditioning. |
| **S&P 500 TR (SPX)** | Primary benchmark |
| **S&P 500 EW (SPXEW)** | Concentration-adjusted comparator |

If the Strategy outperforms each ablation, the alpha is *jointly* attributable to the interaction. If News-only or Passive-only matches the full strategy, the interaction is not earning its keep.

---

## 6. Performance & Risk Metrics

### Absolute return
Total return; CAGR; annualized return; best/worst year and month; hit rate (% months outperforming benchmark).

### Risk
Annualized volatility; downside deviation; maximum drawdown and time-to-recovery; Conditional VaR (Expected Shortfall) at 95% and 99%; Ulcer Index; Pain Index.

### Risk-adjusted
Sharpe ratio (with bootstrap 1,000-resample confidence intervals); Sortino ratio; Calmar ratio; Information ratio vs S&P 500; Treynor ratio; Omega ratio.

### Attribution
- Jensen's α and β via CAPM regression
- **Fama-French 5-factor regression** with reported α, t-statistics, p-values
- **6-factor regression including BAB** to test for low-volatility overlap (critical sanity check — does the strategy generate alpha *after* controlling for the low-vol factor that high-passive-ownership stocks may load on?)

### Regime-conditional
- Sharpe ratio in each regime
- Drawdown statistics by regime
- Return contribution decomposition by regime
- Behaviour at identified inflection points (event study)

---

## 7. Visualizations

All saved as PNG (300dpi) and PDF to `output/figures/`. Colorblind-friendly palette; consistent professional style via `ggplot2`.

### Headline Visual (opens the presentation)
A three-panel composite figure:
1. **Top:** Cumulative log returns of all five portfolios with regime-shaded background
2. **Middle:** Rolling 12-month return spread vs S&P 500 TR
3. **Bottom:** Underwater drawdown curves with NBER recession bands

### Supporting Visualizations
1. **The motivating chart** — PEAD magnitude binned by passive ownership decile (single chart that visualizes the entire thesis)
2. **Regime classification timeline** — banded chart of regime state with VIX and HY OAS overlay
3. **Signal performance heatmap** — News × Passive interaction returns by regime
4. **Drawdown comparison** — underwater chart with all drawdowns >5% labeled
5. **Rolling 24-month Sharpe** — all comparators on one chart
6. **Return distribution** — overlaid histograms + KDE with skew, kurtosis, CVaR annotations
7. **Sector exposure over time** — difference plot vs S&P 500
8. **Passive ownership vs realized volatility** — scatter colored by regime, demonstrating the empirical motivation
9. **Fama-French + BAB regression** — coefficient bar chart with error bars and reported alpha
10. **Turnover and transaction cost analysis** — cumulative costs and rolling turnover
11. **Performance metrics summary table** — clean rendered table for the slide deck

---

## 8. Robustness Checks

To be documented in `analysis/05_robustness_checks.Rmd`:

1. **Subsample stability** — performance in 2005-09, 2010-14, 2015-19, 2020-24
2. **Rebalancing frequency** — monthly vs quarterly vs semi-annual
3. **Signal weight sensitivity** — grid search over News vs Passive contributions
4. **Regime threshold sensitivity** — VIX thresholds at 20/25/30; spread percentiles at 70/80/90
5. **Transaction cost stress** — 5, 10, 25, 50 bps
6. **Universe robustness** — Russell 1000 alternative (top 1000 by mkt cap)
7. **Placebo test** — apply the regime overlay and selection logic to a *random* subset of stocks with similar size/sector characteristics rather than high-passive-ownership names. The alpha should disappear, confirming the passive-ownership channel is doing the work.
8. **News-signal robustness** — compare raw earnings surprise vs SUE vs multi-signal confirmation. The multi-signal version should deliver cleaner alpha, validating the measurement-noise concern.
9. **Bootstrap CI on Sharpe** — 1,000 resamples
10. **Out-of-sample validation** — train signal weights on 2005-2014, test on 2015-2024

---

## 9. Limitations

To be addressed honestly in the presentation:

- **Data lag on passive ownership.** Bloomberg's `PASSIVELY_HELD_PCT_OUT` updates with institutional filing lags (typically 45 days). The strategy applies a corresponding lag to avoid look-ahead, but this also means the signal is somewhat stale relative to real-time passive flow.
- **Factor crowding.** If this strategy or close cousins become widely adopted, the drift will compress. The current edge depends on passive ownership remaining a structurally under-exploited variable.
- **Behavioural endurance.** Multi-year underperformance periods are likely. The drift mechanism does not operate uniformly — calm-regime conditions are when it works best, and prolonged stress regimes will produce negative tracking.
- **Low-volatility overlap.** High-passive-ownership stocks are disproportionately large, stable, low-vol mega-caps. The 6-factor regression including BAB explicitly tests whether the strategy generates alpha *after* controlling for this overlap. If the alpha does not survive the BAB control, the strategy is partly a repackaged low-vol bet — an honest finding either way.
- **Implementation drag.** Bloomberg-grade signals are not free in practice; real-money implementation requires the licensing costs to be amortized over portfolio AUM.
- **Regime classification noise.** A four-state classifier is necessarily lossy. False transitions can trigger costly reweighting. The 60-day dwell time mitigates this but does not eliminate it.

---

## 10. Reproducibility

- **R version pinned** in `DESCRIPTION` and `renv.lock`
- **All random seeds fixed** (signal cross-sectional ranking ties, bootstrap resamples)
- **Manifest tracking** — every backtest run logs the input data manifest version, the code git SHA, and the configuration used. Output filenames embed the manifest version.
- **Tested.** Unit tests via `testthat` cover signal construction (z-scoring, winsorization, multi-signal logic), regime classification (state transitions, dwell time), and portfolio constraints (sector neutrality, position caps, turnover buffer).

---

## 11. Quick Start

### Stage 1 — Bloomberg extraction (on the licensed terminal)

```r
# On the Bloomberg-licensed machine
source("extract.R")
# or with arguments
Rscript extract.R --start-date 2005-01-01 --end-date 2024-12-31 --phase all
```

The extraction produces `data/raw/*.parquet` files. Copy `data/` to the local analysis machine.

### Stage 2 — Local backtest

```r
# On the local machine, no Bloomberg required
source("main.R")
# or
Rscript main.R                       # full pipeline
Rscript main.R --skip-robustness     # quick run
Rscript main.R --regenerate-figures  # rerender plots from cached results
```

### Dependencies

- **R ≥ 4.2**
- **Bloomberg stage:** `Rblpapi`, `arrow`, `data.table`
- **Local stage:** `tidyverse`, `data.table`, `lubridate`, `arrow`, `slider`, `roll`, `PerformanceAnalytics`, `xts`, `ggplot2`, `patchwork`, `gt`, `testthat`, `renv`
- Environment managed via `renv` for reproducibility

---

## 12. One-Sentence Summary

> A long-only S&P 500 strategy that systematically buys stocks experiencing confirmed positive fundamental news where passive ownership is structurally elevated — capturing the slow drift that arises when the marginal price-setter has been crowded out of the holder base — with a regime overlay that de-risks the same exposure during stress phases when the drift mechanism inverts.
