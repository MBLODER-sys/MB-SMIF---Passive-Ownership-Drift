# bloomberg/02_extract_prices.R
# Daily OHLCV + total return + market cap + 90D vol for all ever-S&P 500 tickers.

suppressPackageStartupMessages({
  library(Rblpapi)
  library(data.table)
  library(arrow)
})

# Resolve project root robustly and load settings + field map.
if (requireNamespace("rprojroot", quietly = TRUE)) {
  .proj_root <- rprojroot::find_root(rprojroot::has_file("README.md"))
} else {
  .proj_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."),
                              mustWork = FALSE)
}
source(file.path(.proj_root, "config", "settings.R"))
source(file.path(.proj_root, "config", "bbg_fields.R"))

blpConnect()

universe_dt <- as.data.table(read_parquet(
  file.path(paths$universe, "spx_constituents_quarterly.parquet")
))
all_tickers <- unique(universe_dt$ticker)

message(sprintf("[prices] pulling daily history for %d tickers", length(all_tickers)))

# Chunk to keep request size sane.
chunk_size <- 50
ticker_chunks <- split(all_tickers,
                       ceiling(seq_along(all_tickers) / chunk_size))

px_list <- vector("list", length(ticker_chunks))

for (i in seq_along(ticker_chunks)) {
  ch <- ticker_chunks[[i]]
  message(sprintf("[prices] chunk %d/%d (%d tickers)",
                  i, length(ticker_chunks), length(ch)))
  res <- tryCatch(
    bdh(securities = ch,
        fields     = bbg_fields$prices,
        start.date = settings$full_start_date,
        end.date   = settings$full_end_date,
        int.as.double = TRUE,
        simplify   = FALSE),
    error = function(e) {
      message(sprintf("[prices] chunk %d failed: %s", i, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(res)) next

  rows <- lapply(names(res), function(tk) {
    d <- res[[tk]]
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- as.data.table(d)
    d[, ticker := tk]
    d
  })
  px_list[[i]] <- rbindlist(rows, use.names = TRUE, fill = TRUE)
}

prices_dt <- rbindlist(px_list, use.names = TRUE, fill = TRUE)
setnames(prices_dt, tolower(names(prices_dt)))
setnames(prices_dt, "date", "date")
setkey(prices_dt, ticker, date)

out_path <- file.path(paths$raw, "prices_daily.parquet")
write_parquet(prices_dt, out_path)
message(sprintf("[prices] wrote %s (%d rows)", out_path, nrow(prices_dt)))
