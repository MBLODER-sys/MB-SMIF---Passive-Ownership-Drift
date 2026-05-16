# bloomberg/run_all_extractions.R
# Orchestrator: runs each extraction phase in dependency order.
# Intended for a Bloomberg-licensed terminal session.
#
# AUTO-SKIP: a phase is skipped if its output parquet already exists.
# To force re-run a phase, set `.force_phases` before sourcing:
#
#   .force_phases <- c("02_extract_prices.R")     # re-run prices only
#   source("extract.R")
#
#   .force_phases <- "all"                        # force re-run everything
#   source("extract.R")
#
# To run a specific subset, just delete (or move) the parquet you want
# regenerated and source extract.R — the orchestrator will see it missing
# and rebuild it.

if (requireNamespace("rprojroot", quietly = TRUE)) {
  .proj_root <- rprojroot::find_root(rprojroot::has_file("README.md"))
} else {
  .proj_root <- normalizePath(getwd(), mustWork = FALSE)
}
source(file.path(.proj_root, "config", "settings.R"))    # provides `paths`

bloomberg_dir <- file.path(.proj_root, "bloomberg")
if (!dir.exists(bloomberg_dir)) {
  stop(sprintf("[extract] bloomberg/ not found at '%s'. ", bloomberg_dir),
       "Run from the project root.", call. = FALSE)
}

# Stage definitions: each entry maps a script to its expected output file.
# Auto-skip checks file.exists(output_path).
stages <- list(
  list(script = "01_extract_universe.R",
       output = file.path(paths$universe, "spx_constituents_quarterly.parquet"),
       label  = "universe"),
  list(script = "02_extract_prices.R",
       output = file.path(paths$raw, "prices_daily.parquet"),
       label  = "prices"),
  list(script = "03_extract_fundamentals.R",
       output = file.path(paths$raw, "fundamentals_quarterly.parquet"),
       label  = "fundamentals"),
  list(script = "04_extract_news_signals.R",
       output = file.path(paths$raw, "news_signals_quarterly.parquet"),
       label  = "news"),
  list(script = "05_extract_ownership.R",
       output = file.path(paths$raw, "ownership_quarterly.parquet"),
       label  = "ownership"),
  list(script = "06_extract_macro.R",
       output = file.path(paths$raw, "macro_daily.parquet"),
       label  = "macro")
)

force_phases <- if (exists(".force_phases")) .force_phases else character(0)
force_all    <- "all" %in% force_phases

skipped <- character(0)
ran     <- character(0)

for (st in stages) {
  script_path <- file.path(bloomberg_dir, st$script)
  if (!file.exists(script_path)) {
    stop(sprintf("[extract] script not found: %s", script_path), call. = FALSE)
  }

  should_run <- force_all || (st$script %in% force_phases) ||
                !file.exists(st$output)

  if (!should_run) {
    sz_mb <- round(file.info(st$output)$size / 1e6, 1)
    message(sprintf("[skip] %-30s output exists (%.1f MB) — %s",
                    st$script, sz_mb, basename(st$output)))
    skipped <- c(skipped, st$script)
    next
  }

  message(sprintf("\n===== running %s =====", st$script))
  t0 <- Sys.time()
  source(script_path, echo = FALSE, local = new.env())
  message(sprintf("    finished in %.1fs",
                  as.numeric(Sys.time() - t0, units = "secs")))
  ran <- c(ran, st$script)
}

message("\n[extract] ---------------- summary ----------------")
if (length(ran) > 0) {
  message(sprintf("[extract] ran (%d): %s",
                  length(ran), paste(ran, collapse = ", ")))
}
if (length(skipped) > 0) {
  message(sprintf("[extract] skipped (%d, output already cached): %s",
                  length(skipped), paste(skipped, collapse = ", ")))
}
if (length(ran) == 0 && length(skipped) > 0) {
  message("[extract] all phases cached. ",
          "To force re-run, set `.force_phases <- c(...)` before source().")
}
