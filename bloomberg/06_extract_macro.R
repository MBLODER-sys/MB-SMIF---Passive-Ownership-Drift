# bloomberg/06_extract_macro.R
# Benchmark + cash rate series. The full regime layer has been removed in
# v1; this extraction now pulls only SPXT (benchmark), SPXEW (equal-weight
# comparator), and USGG3M (cash rate).

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

macro_tickers <- unlist(bbg_fields$macro_tickers, use.names = TRUE)
message(sprintf("[macro] pulling daily history for %d series",
                length(macro_tickers)))

res <- bdh(securities = unname(macro_tickers),
           fields     = bbg_fields$macro_field,
           start.date = settings$full_start_date,
           end.date   = settings$full_end_date,
           simplify   = FALSE)

rows <- lapply(names(res), function(tk) {
  d <- res[[tk]]
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- as.data.table(d)
  d[, ticker := tk]
  d[, series_name := names(macro_tickers)[match(tk, unname(macro_tickers))]]
  d
})

macro_dt <- rbindlist(rows, use.names = TRUE, fill = TRUE)
setnames(macro_dt, names(macro_dt), tolower(names(macro_dt)))
if (!"date" %in% names(macro_dt)) {
  cand <- grep("date", names(macro_dt),
               ignore.case = TRUE, value = TRUE)
  if (length(cand) == 1L) setnames(macro_dt, cand, "date")
  else stop("[macro] no date column. Columns: ",
            paste(names(macro_dt), collapse = ", "), call. = FALSE)
}
setkey(macro_dt, ticker, date)

out_path <- file.path(paths$raw, "macro_daily.parquet")

gc(verbose = FALSE)
if (file.exists(out_path)) try(file.remove(out_path), silent = TRUE)

write_parquet(macro_dt, out_path)
message(sprintf("[macro] wrote %s (%d rows)", out_path, nrow(macro_dt)))
