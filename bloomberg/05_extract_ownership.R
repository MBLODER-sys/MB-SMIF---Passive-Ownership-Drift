# bloomberg/05_extract_ownership.R
# Passive ownership panel — the key field for this strategy.
# PASSIVELY_HELD_PCT_OUT updates with institutional filing lag (~45d), so we
# apply settings$passive_ownership_lag_days in the local analysis layer.

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
  file.path(paths$universe, "spx_constituents_quarterly.parquet")
))
all_tickers <- unique(universe_dt$ticker)

fields <- bbg_fields$ownership

# Ownership snapshots are reported quarterly via institutional filings.
opts <- bbg_fields$options_quarterly

chunk_size <- 25
ticker_chunks <- split(all_tickers,
                       ceiling(seq_along(all_tickers) / chunk_size))

own_list <- vector("list", length(ticker_chunks))

for (i in seq_along(ticker_chunks)) {
  ch <- ticker_chunks[[i]]
  message(sprintf("[ownership] chunk %d/%d (%d tickers)",
                  i, length(ticker_chunks), length(ch)))
  res <- tryCatch(
    bdh(securities = ch,
        fields     = fields,
        start.date = settings$full_start_date,
        end.date   = settings$full_end_date,
        options    = opts,
        simplify   = FALSE),
    error = function(e) {
      message(sprintf("[ownership] chunk %d failed: %s",
                      i, conditionMessage(e)))
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
  own_list[[i]] <- rbindlist(rows, use.names = TRUE, fill = TRUE)
}

ownership_dt <- rbindlist(own_list, use.names = TRUE, fill = TRUE)
setnames(ownership_dt, tolower(names(ownership_dt)))
setkey(ownership_dt, ticker, date)

out_path <- file.path(paths$raw, "ownership_quarterly.parquet")
write_parquet(ownership_dt, out_path)
message(sprintf("[ownership] wrote %s (%d rows)", out_path, nrow(ownership_dt)))
