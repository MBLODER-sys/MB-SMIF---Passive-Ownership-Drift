# bloomberg/04_extract_news_signals.R
# Fundamental news signal inputs: SUE, revenue surprise, margin trend, guidance.
# Pulled quarterly and aligned to actual announcement dates.

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

fields <- bbg_fields$news

opts <- c(bbg_fields$options_quarterly,
          c(nonTradingDayFillOption = "ALL_CALENDAR_DAYS",
            nonTradingDayFillMethod = "NIL_VALUE"))
ovrd <- c(FUND_PER = "FQ")

# ---- Field preflight: drop bad/empty fields against AAPL ----------------
message("[news] preflight: validating fields against AAPL...")
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
    message(sprintf("[news]   DROPPING bad/empty field: %s", f))
  }
}
if (length(valid_fields) == 0) {
  stop("[news] all fields failed validation — check entitlement.",
       call. = FALSE)
}
message(sprintf("[news] proceeding with %d valid fields: %s",
                length(valid_fields), paste(valid_fields, collapse = ", ")))
fields <- valid_fields

chunk_size <- 25
ticker_chunks <- split(all_tickers,
                       ceiling(seq_along(all_tickers) / chunk_size))

news_list <- vector("list", length(ticker_chunks))

for (i in seq_along(ticker_chunks)) {
  ch <- ticker_chunks[[i]]
  message(sprintf("[news] chunk %d/%d (%d tickers)",
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
      message(sprintf("[news] chunk %d failed: %s", i, conditionMessage(e)))
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
  news_list[[i]] <- rbindlist(rows, use.names = TRUE, fill = TRUE)
}

news_dt <- rbindlist(news_list, use.names = TRUE, fill = TRUE)
setnames(news_dt, names(news_dt), tolower(names(news_dt)))
if (!"date" %in% names(news_dt)) {
  cand <- grep("date", names(news_dt),
               ignore.case = TRUE, value = TRUE)
  if (length(cand) == 1L) setnames(news_dt, cand, "date")
  else stop("[news] no date column. Columns: ",
            paste(names(news_dt), collapse = ", "), call. = FALSE)
}
setkey(news_dt, ticker, date)

out_path <- file.path(paths$raw, "news_signals_quarterly.parquet")

gc(verbose = FALSE)
if (file.exists(out_path)) try(file.remove(out_path), silent = TRUE)

write_parquet(news_dt, out_path)
message(sprintf("[news] wrote %s (%d rows)", out_path, nrow(news_dt)))
