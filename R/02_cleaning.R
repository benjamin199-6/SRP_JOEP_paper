###############################################################################
# 02_clean_data.R
#
# Purpose:
# Clean and transform raw experimental survey data into analysis dataset.
#
# Reproducibility principles:
# 1. Raw data NEVER modified
# 2. Deterministic transformations only
# 3. Panel structure defined by (ID × Round)
#
# Input  : raw_data_main
# Output : data/processed/analysis_data.rds
###############################################################################

message("Running 02_clean_data.R ...")

# ---------------------------------------------------------------------------
# 1. Copy raw data
# ---------------------------------------------------------------------------

data <- raw_data_main

# ---------------------------------------------------------------------------
# 2. Keep completed respondents
# ---------------------------------------------------------------------------

data <- data %>%
  dplyr::filter(Progress >= 100)

# ---------------------------------------------------------------------------
# 3. Remove survey metadata columns
# ---------------------------------------------------------------------------

data <- data[, 19:89]

# Stable participant identifier
data$ID <- factor(seq_len(nrow(data)))

# ---------------------------------------------------------------------------
# 4. Construct gameplay panel (ID × Round)
# ---------------------------------------------------------------------------

# Clicks
clicks_long <- data %>%
  dplyr::select(ID, totalBoxesClickedArr1:totalBoxesClickedArr5) %>%
  tidyr::pivot_longer(
    cols = -ID,
    names_to = "Round",
    values_to = "Clicks"
  ) %>%
  mutate(Round = as.numeric(gsub("\\D", "", Round)))

# Hidden bombs
bombs_hidden <- data %>%
  dplyr::select(ID, nBombsArr1:nBombsArr5) %>%
  tidyr::pivot_longer(
    cols = -ID,
    names_to = "Round",
    values_to = "Bombs_hidden"
  ) %>%
  mutate(Round = as.numeric(gsub("\\D", "", Round)))

# Bomb clicks
bombs_clicked <- data %>%
  dplyr::select(ID, bombsClickedArr1:bombsClickedArr5) %>%
  tidyr::pivot_longer(
    cols = -ID,
    names_to = "Round",
    values_to = "Bombs_clicked"
  ) %>%
  mutate(Round = as.numeric(gsub("\\D", "", Round)))

# Merge safely using panel keys
long <- clicks_long %>%
  dplyr::left_join(bombs_hidden,  by = c("ID","Round")) %>%
  dplyr::left_join(bombs_clicked, by = c("ID","Round"))

# Panel sanity checks
stopifnot(!anyDuplicated(names(long)))
stopifnot(nrow(long) == dplyr::n_distinct(long$ID, long$Round))

rm(clicks_long, bombs_hidden, bombs_clicked)

# ---------------------------------------------------------------------------
# 5. Reshape belief elicitation data
# ---------------------------------------------------------------------------

belief_long <- data %>%
  dplyr::select(ID, Belief_R1.0:Belief_R5.2) %>%
  tidyr::pivot_longer(
    cols = -ID,
    names_to = c("Round", ".value"),
    names_pattern = "Belief_R(\\d)\\.(\\d)"
  ) %>%
  dplyr::rename(
    Belief_0 = `0`,
    Belief_1 = `1`,
    Belief_2 = `2`
  ) %>%
  mutate(Round = as.numeric(Round))

long <- long %>%
  dplyr::left_join(belief_long, by = c("ID","Round"))

# ---------------------------------------------------------------------------
# 6. Merge subject-level characteristics
# ---------------------------------------------------------------------------

subject_data <- data %>%
  dplyr::select(
    ID,
    risk,
    highAmountOfMoney,
    Age,
    Sex,
    EDU,
    Income_1,
    Risk_Task_Answer,
    UG_Person_A_1,
    UG_Person_A_2,
    UG_P_B
  )

long <- long %>%
  dplyr::left_join(subject_data, by = "ID")


# ---------------------------------------------------------------------------
# 7. Behavioral variables
# ---------------------------------------------------------------------------

long <- long %>%
  dplyr::arrange(ID, Round) %>%
  dplyr::mutate(
    Bomb_new = ifelse(Bombs_clicked %in% c(1,2),1,0)
  ) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(Feedback = dplyr::lag(Bomb_new)) %>%
  dplyr::ungroup()

long$Feedback <- dplyr::case_when(
  is.na(long$Feedback) ~ "no feedback",
  long$Feedback == 1 ~ "Feedback_bomb",
  TRUE ~ "Feedback_no_bomb"
)

long$Feedback <- factor(
  long$Feedback,
  levels = c("no feedback","Feedback_no_bomb","Feedback_bomb")
)

# ---------------------------------------------------------------------------
# 8. Treatment conditions
# ---------------------------------------------------------------------------

long <- long %>%
  dplyr::mutate(
    Condition_Uncertainty =
      factor(ifelse(risk < 1, "Ambiguity", "Risk")),
    Condition_SRP =
      factor(ifelse(highAmountOfMoney < 1, "Low", "High"))
  )

# Explicit reference categories (important for regressions)
long$Condition_Uncertainty <-
  stats::relevel(long$Condition_Uncertainty, ref = "Ambiguity")

long$Condition_SRP <-
  stats::relevel(long$Condition_SRP, ref = "High")

# ---------------------------------------------------------------------------
# 9. Demographics and controls
# ---------------------------------------------------------------------------

analysis_data <- long %>%
  dplyr::mutate(
    Female = factor(ifelse(Sex == 2,1,0)),
    Education_Level = dplyr::case_when(
      EDU == 1 ~ "Low",
      EDU %in% 2:3 ~ "Medium",
      EDU %in% 4:6 ~ "High"
    ),
    Round = factor(Round),
    Subject = ID
  ) %>%
  dplyr::filter(!is.na(Clicks))


analysis_data <- long %>%
  mutate(
    
    Female = factor(ifelse(Sex == 2, 1, 0)),
    
    Education_Level = case_when(
      EDU == 1 ~ "Low",
      EDU %in% 2:3 ~ "Medium",
      EDU %in% 4:6 ~ "High",
      TRUE ~ "Other"
    ),
    
    # -------------------------------------------------
    # Risk attitude categories (USED IN PAPER TABLE 1)
    # -------------------------------------------------
    Risk_attitude = case_when(
      Risk_Task_Answer <= 2 ~ "High aversion",
      Risk_Task_Answer > 2 & Risk_Task_Answer <= 5 ~ "Moderate aversion",
      Risk_Task_Answer > 5 & Risk_Task_Answer <= 7 ~ "Low aversion",
      Risk_Task_Answer > 7 ~ "Risk loving"
    ),
    
    Risk_attitude = factor(
      Risk_attitude,
      levels = c(
        "High aversion",
        "Low aversion",
        "Moderate aversion",
        "Risk loving"
      )
    ),
    
    Round = factor(Round),
    Subject = ID
  ) %>%
  filter(!is.na(Clicks))

# ---------------------------------------------------------------------------
# 10. Exclude 
analysis_data2 <- analysis_data 

analysis_data <- analysis_data %>%
  dplyr::filter(!(Condition_Uncertainty == "Risk" & Clicks >= 100))

# ---------------------------------------------------------------------------
# 11. Save processed dataset
# ---------------------------------------------------------------------------

saveRDS(
  analysis_data,
  file.path(paths$data_processed, "analysis_data.rds")
)


saveRDS(
  analysis_data2,
  file.path(paths$data_processed, "analysis_data_full.rds")
)

message("Clean dataset saved.")
message("02_clean_data.R completed successfully.")
