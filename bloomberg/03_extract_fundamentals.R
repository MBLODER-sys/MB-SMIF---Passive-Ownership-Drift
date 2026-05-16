# bloomberg/03_extract_fundamentals.R
# Quality + Value screen fundamentals, quarterly periodicity.

suppressPackageStartupMessages({
  library(Rblpapi)
  library(data.table)
  library(arrow)
})

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

fields <- c(bbg_fields$quality, bbg_fields$value)

opts <- c(bbg_fields$options_quarterly,
          c(nonTradingDayFillOption = "ALL_CALENDAR_DAYS",
            nonTradingDayFillMethod = "NIL_VALUE"))
ovrd <- c(FUND_PER = "FQ")

# ---- Field preflight ------------------------------------------------------
# bdh rejects an entire request if ANY field name is invalid for the
# current Bloomberg version (e.g. "Bad field: GROSS_PROFIT_MARGIN"). Test
# each field individually against AAPL and drop ones the terminal doesn't
# recognise. Logged so the user can see which fields were skipped.
message("[fundamentals] preflight: validating fields against AAPL...")
valid_fields <- character(0)
for (f in fields) {
  ok <- tryCatch({
    res <- bdh("AAPL US Equity", f,
               start.date = as.Date("2022-01-01"),
               end.date   = as.Date("2023-12-31"),
               options    = opts,
               overrides  = ovrd)
    !is.null(res) && nrow(res) > 0
  }, error = function(e) FALSE)
  if (ok) {
    valid_fields <- c(valid_fields, f)
  } else {
    message(sprintf("[fundamentals]   DROPPING bad/empty field: %s", f))
  }
}
if (length(valid_fields) == 0) {
  stop("[fundamentals] all fields failed validation — check entitlement.",
       call. = FALSE)
}
message(sprintf("[fundamentals] proceeding with %d valid fields: %s",
                length(valid_fields), paste(valid_fields, collapse = ", ")))
fields <- valid_fields

chunk_size <- 25
ticker_chunks <- split(all_tickers,
                       ceiling(seq_along(all_tickers) / chunk_size))

fnd_list <- vector("list", length(ticker_chunks))

for (i in seq_along(ticker_chunks)) {
  ch <- ticker_chunks[[i]]
  message(sprintf("[fundamentals] chunk %d/%d (%d tickers)",
                  i, length(ticker_chunks), length(ch)))
  res <- tryCatch(
    bdh(securities = ch,
        fields     = fields,
        start.date = settings$full_start_date,
        end.date   = settings$full_end_date,
        options    = opts,
        overrides  = ovrd,
        simplify   = FALSE),
    error = function(e) {
      message(sprintf("[fundamentals] chunk %d failed: %s",
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
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) next
  fnd_list[[i]] <- rbindlist(rows, use.names = TRUE, fill = TRUE)
}

fundamentals_dt <- rbindlist(fnd_list, use.names = TRUE, fill = TRUE)

if (nrow(fundamentals_dt) == 0) {
  stop("[fundamentals] every chunk returned empty after field validation.",
       call. = FALSE)
}

setnames(fundamentals_dt, names(fundamentals_dt),
         tolower(names(fundamentals_dt)))
if (!"date" %in% names(fundamentals_dt)) {
  cand <- grep("date", names(fundamentals_dt),
               ignore.case = TRUE, value = TRUE)
  if (length(cand) == 1L) setnames(fundamentals_dt, cand, "date")
  else stop("[fundamentals] no date column. Columns: ",
            paste(names(fundamentals_dt), collapse = ", "), call. = FALSE)
}
setkey(fundamentals_dt, ticker, date)

out_path <- file.path(paths$raw, "fundamentals_quarterly.parquet")

gc(verbose = FALSE)
if (file.exists(out_path)) try(file.remove(out_path), silent = TRUE)

write_parquet(fundamentals_dt, out_path)
message(sprintf("[fundamentals] wrote %s (%d rows, %d fields)",
                out_path, nrow(fundamentals_dt),
                length(names(fundamentals_dt)) - 2L))   # minus date + ticker
