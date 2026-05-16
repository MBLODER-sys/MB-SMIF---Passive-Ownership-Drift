# bloomberg/02_extract_prices.R
# Daily OHLCV + total return + market cap + 90D vol for all ever-S&P 500 tickers.
#
# Hardened against Bloomberg BDH per-request data-point caps. The earlier
# version requested 50 tickers x ~10 fields x 5,000 trading days ≈ 2.5M
# data points per call — well over Bloomberg's per-request limit — which
# caused silent empty returns for every chunk.
#
# This rewrite:
#   - reduces chunk_size to 10 tickers (~500k points/call worst case)
#   - drops BETA_ADJ_OVERRIDABLE (requires an override) and
#     TOT_RETURN_INDEX_GROSS_DVDS (often patchy); both unused by the v1
#     backtest, which builds total return from PX_LAST + dividends if
#     needed
#   - splits the date range into 5-year sub-windows per chunk
#   - prints per-chunk diagnostics (rows, first ticker) so silent
#     empty returns are visible

suppressPackageStartupMessages({
  library(Rblpapi)
  library(data.table)
  library(arrow)
})

# Resolve project root robustly and load settings + field map.
if (requireNamespace("rprojroot", quietly = TRUE)) {
  .proj_root <- rprojroot::find_root(rprojroot::has_file("README.md"))
} else {
  .proj_root <- normalizePath(getwd(), mustWork = FALSE)
}
source(file.path(.proj_root, "config", "settings.R"))
source(file.path(.proj_root, "config", "bbg_fields.R"))

blpConnect()

universe_dt <- as.data.table(read_parquet(
  file.path(paths$universe, "spx_constituents_quarterly.parquet"),
  mmap = FALSE
))
all_tickers <- unique(universe_dt$ticker)

# Trim field set to those guaranteed to return for US large caps.
prices_fields <- c(
  "PX_LAST", "PX_OPEN", "PX_HIGH", "PX_LOW", "PX_VOLUME",
  "EQY_SH_OUT", "CUR_MKT_CAP"
)

message(sprintf("[prices] pulling daily history for %d tickers (fields: %s)",
                length(all_tickers), paste(prices_fields, collapse = ", ")))

# Chunking strategy: small ticker chunks + sub-windows in time.
chunk_size <- 10
ticker_chunks <- split(all_tickers,
                       ceiling(seq_along(all_tickers) / chunk_size))

# Date windows: 5 years each.
date_seq <- seq(settings$full_start_date,
                settings$full_end_date,
                by = "5 years")
date_seq <- c(date_seq, settings$full_end_date)
date_seq <- unique(date_seq)
date_windows <- mapply(
  function(a, b) list(start = a, end = b - 1L),
  date_seq[-length(date_seq)],
  date_seq[-1],
  SIMPLIFY = FALSE
)
# Last window ends exactly on full_end_date.
date_windows[[length(date_windows)]]$end <- settings$full_end_date

px_list <- vector("list", length(ticker_chunks) * length(date_windows))
slot <- 1L

total_calls <- length(ticker_chunks) * length(date_windows)
message(sprintf("[prices] %d ticker chunks x %d date windows = %d BDH calls",
                length(ticker_chunks), length(date_windows), total_calls))

for (i in seq_along(ticker_chunks)) {
  ch <- ticker_chunks[[i]]
  for (j in seq_along(date_windows)) {
    w <- date_windows[[j]]
    message(sprintf("[prices] chunk %d/%d  window %d/%d  (%s -> %s, %d tickers)",
                    i, length(ticker_chunks),
                    j, length(date_windows),
                    w$start, w$end, length(ch)))
    res <- tryCatch(
      bdh(securities    = ch,
          fields        = prices_fields,
          start.date    = w$start,
          end.date      = w$end,
          int.as.double = TRUE,
          simplify      = FALSE),
      error = function(e) {
        message(sprintf("[prices] chunk %d/%d window %d FAILED: %s",
                        i, length(ticker_chunks), j, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(res) || length(res) == 0) {
      message(sprintf("[prices]   -> empty result"))
      slot <- slot + 1L
      next
    }

    rows <- lapply(names(res), function(tk) {
      d <- res[[tk]]
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d <- as.data.table(d)
      d[, ticker := tk]
      d
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0) {
      message(sprintf("[prices]   -> all tickers empty"))
      slot <- slot + 1L
      next
    }
    chunk_dt <- rbindlist(rows, use.names = TRUE, fill = TRUE)
    message(sprintf("[prices]   -> %d rows across %d tickers",
                    nrow(chunk_dt), length(rows)))
    px_list[[slot]] <- chunk_dt
    slot <- slot + 1L
  }
}

prices_dt <- rbindlist(px_list, use.names = TRUE, fill = TRUE)

if (nrow(prices_dt) == 0) {
  stop("[prices] EVERY chunk returned empty. ",
       "Check Bloomberg connection, data-point quota (DAPI <GO>), ",
       "and that bdp('AAPL US Equity','PX_LAST') works in this session.",
       call. = FALSE)
}

setnames(prices_dt, names(prices_dt), tolower(names(prices_dt)))

# Defensive: the Bloomberg date column should be "date" after lowercasing.
if (!"date" %in% names(prices_dt)) {
  date_candidates <- grep("date", names(prices_dt),
                          ignore.case = TRUE, value = TRUE)
  if (length(date_candidates) == 1L) {
    setnames(prices_dt, date_candidates, "date")
  } else {
    stop(sprintf("[prices] cannot identify date column. Got columns: %s",
                 paste(names(prices_dt), collapse = ", ")), call. = FALSE)
  }
}
setkey(prices_dt, ticker, date)

out_path <- file.path(paths$raw, "prices_daily.parquet")

gc(verbose = FALSE)
if (file.exists(out_path)) try(file.remove(out_path), silent = TRUE)

write_parquet(prices_dt, out_path)
message(sprintf("[prices] wrote %s (%d rows, %d tickers)",
                out_path, nrow(prices_dt), length(unique(prices_dt$ticker))))
