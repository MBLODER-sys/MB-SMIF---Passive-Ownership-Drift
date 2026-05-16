# bloomberg/01_extract_universe.R
# Extract historical S&P 500 constituents at each quarter-end.
# Uses INDX_MWEIGHT_HIST with END_DATE_OVERRIDE for point-in-time membership.

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

# Build quarter-end dates across the full window. Construct as character
# first to avoid sapply(...) collapsing Date objects into a numeric vector
# that as.Date refuses to reinterpret.
.years <- seq.int(as.integer(format(settings$full_start_date, "%Y")),
                  as.integer(format(settings$full_end_date,   "%Y")))
quarter_ends <- as.Date(unlist(lapply(.years, function(y) {
  c(sprintf("%d-03-31", y),
    sprintf("%d-06-30", y),
    sprintf("%d-09-30", y),
    sprintf("%d-12-31", y))
})))
quarter_ends <- quarter_ends[quarter_ends >= settings$full_start_date &
                             quarter_ends <= settings$full_end_date]
quarter_ends <- sort(unique(quarter_ends))

message(sprintf("[universe] pulling INDX_MWEIGHT_HIST for %d quarter-ends",
                length(quarter_ends)))

universe_rows <- vector("list", length(quarter_ends))

for (i in seq_along(quarter_ends)) {
  d <- quarter_ends[i]
  ovrd <- c(END_DATE_OVERRIDE = format(d, "%Y%m%d"))
  res <- tryCatch(
    bds(bbg_fields$universe$index,
        bbg_fields$universe$members_field,
        overrides = ovrd),
    error = function(e) {
      message(sprintf("[universe] failed for %s: %s", d, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(res) || nrow(res) == 0) next

  # Normalise column names (BDS returns "Index Member" and "Percent Weight" usually)
  names(res) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(res)))
  setDT(res)
  res[, snapshot_date := d]
  universe_rows[[i]] <- res
}

universe_dt <- rbindlist(universe_rows, use.names = TRUE, fill = TRUE)

# Canonicalise tickers to "XXXX US Equity". INDX_MWEIGHT_HIST can return
# tickers in several formats ("AAPL", "AAPL US", "AAPL UW Equity",
# "AAPL US Equity") depending on the Bloomberg session — strip any
# existing suffix and re-append the canonical " US Equity".
ticker_col <- intersect(c("index_member", "ticker", "security"), names(universe_dt))[1]
setnames(universe_dt, ticker_col, "ticker_raw")
normalize_us_ticker <- function(t) {
  t <- trimws(t)
  t <- gsub("\\s+", " ", t)
  t <- sub("\\s+Equity$", "", t, ignore.case = TRUE)   # strip trailing Equity
  t <- sub("\\s+[A-Z]{1,3}$", "", t)                   # strip exchange/country code
  paste(t, "US Equity")
}
universe_dt[, ticker := vapply(ticker_raw, normalize_us_ticker, character(1))]
message(sprintf("[universe] sample normalised tickers: %s",
                paste(head(unique(universe_dt$ticker), 5), collapse = " | ")))

# Attach GICS sector once (current snapshot; for true PIT, override with date in a future iteration).
unique_tickers <- unique(universe_dt$ticker)
ref <- bdp(unique_tickers, bbg_fields$reference)
setDT(ref, keep.rownames = "ticker")
# BDP returns columns in the requested case (GICS_SECTOR_NAME, etc.) —
# lowercase them so the merged panel has a consistent schema with the
# BDS-derived columns.
setnames(ref, names(ref), tolower(names(ref)))
universe_dt <- merge(universe_dt, ref, by = "ticker", all.x = TRUE)

out_path <- file.path(paths$universe, "spx_constituents_quarterly.parquet")

# Force release of any mmap held by a prior read in the same R session
# (Windows: parquet write fails with WinError 1224 otherwise).
gc(verbose = FALSE)
if (file.exists(out_path)) try(file.remove(out_path), silent = TRUE)

write_parquet(universe_dt, out_path)
message(sprintf("[universe] wrote %s (%d rows)", out_path, nrow(universe_dt)))
