###############################################################################
# 00_setup.R
#
# Purpose:
# Global setup for the research project.
# - Load required packages
# - Define project paths
# - Set global options
# - Ensure reproducibility
#

###############################################################################

message("Running 00_setup.R ...")

# ---------------------------------------------------------------------------
# 1. Clean environment (only if interactive)
# ---------------------------------------------------------------------------
if (interactive()) {
  rm(list = ls())
  gc()
}

# ---------------------------------------------------------------------------
# 2. Required packages
# ---------------------------------------------------------------------------
required_packages <- c(
  "tidyverse",
  "here",
  "janitor",
  "fixest",
  "readr"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))



# ---------------------------------------------------------------------------
# 4. Project paths (NO setwd())
# ---------------------------------------------------------------------------
paths <- list(
  data_raw       = here::here("data", "raw"),
  data_processed = here::here("data", "processed"),
  output_tables  = here::here("paper", "tables"),
  output_figures = here::here("paper", "figures"),
  output_models  = here::here("output", "models")
)
dir.create("data/raw", recursive = TRUE)
# Create folders if missing
dir.create(paths$data_processed, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$output_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$output_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$output_models, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 5. Sanity checks
# ---------------------------------------------------------------------------
if (!dir.exists(paths$data_raw)) {
  stop("Folder 'data/raw' not found. Place raw data there.")
}



message("00_setup.R completed successfully.")
