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

# Resolve project root robustly. `sys.frame(1)$ofile` is unreliable in
# RStudio's source() and breaks when nested, so use rprojroot or fall
# back to the working directory.
if (requireNamespace("rprojroot", quietly = TRUE)) {
  .proj_root <- rprojroot::find_root(rprojroot::has_file("README.md"))
} else {
  .proj_root <- normalizePath(getwd(), mustWork = FALSE)
}
bloomberg_dir <- file.path(.proj_root, "bloomberg")

if (!dir.exists(bloomberg_dir)) {
  stop(sprintf("[extract] bloomberg/ directory not found at '%s'. ",
               bloomberg_dir),
       "Run from the project root, or install rprojroot.",
       call. = FALSE)
}

for (s in stages) {
  script_path <- file.path(bloomberg_dir, s)
  if (!file.exists(script_path)) {
    stop(sprintf("[extract] script not found: %s", script_path), call. = FALSE)
  }
  message(sprintf("\n===== running %s =====", s))
  t0 <- Sys.time()
  source(script_path, echo = FALSE, local = new.env())
  message(sprintf("    finished in %.1fs",
                  as.numeric(Sys.time() - t0, units = "secs")))
}

message("\n[extract] all phases complete. ",
        "Copy data/ to the analysis machine (or run main.R here).")
