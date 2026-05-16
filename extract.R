# extract.R
# Bloomberg pipeline entry point. RUN ON A BLOOMBERG-LICENSED TERMINAL.
# Sources the orchestrator in bloomberg/.

bloomberg_dir <- file.path(getwd(), "bloomberg")
if (!dir.exists(bloomberg_dir)) {
  stop("Run from project root (the directory containing bloomberg/).")
}
source(file.path(bloomberg_dir, "run_all_extractions.R"))
