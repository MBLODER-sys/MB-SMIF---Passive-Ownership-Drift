# bloomberg/run_all_extractions.R
# Orchestrator: runs each extraction phase in dependency order.
# Intended for a Bloomberg-licensed terminal session.

stages <- c(
  "01_extract_universe.R",
  "02_extract_prices.R",
  "03_extract_fundamentals.R",
  "04_extract_news_signals.R",
  "05_extract_ownership.R",
  "06_extract_macro.R"
)

bloomberg_dir <- dirname(sys.frame(1)$ofile)

for (s in stages) {
  message(sprintf("\n===== running %s =====", s))
  t0 <- Sys.time()
  source(file.path(bloomberg_dir, s), echo = FALSE, local = new.env())
  message(sprintf("    finished in %.1fs", as.numeric(Sys.time() - t0, units = "secs")))
}

message("\n[extract] all phases complete. Copy data/ to local machine to run main.R.")
