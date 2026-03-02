###############################################################################
# 01_load_data.R
#
# Purpose:
# Load all raw data used in the project.
# NO cleaning, NO transformations.
###############################################################################

message("Running 01_load_data.R ...")

# ---------------------------------------------------------------------------
# 1. Load main dataset (CSV)
# ---------------------------------------------------------------------------

raw_data_main <- readr::read_csv(
  file.path(paths$data_raw, "Benjamin_Kirchler_2022.csv"),
  show_col_types = FALSE
)
