###############################################################################
# 03_descriptives.R
#
# Purpose:
# Produce descriptive statistics and figures reported in the paper.
#
# Outputs:
# - Table 1: Randomization check
# - Figure 1: Mean clicks (Round 1)
# - Figure 2: Click dynamics
# - Figure 3: Feedback mechanism (% change)
# - Appendix Figure: Change from baseline
###############################################################################

message("Running 03_descriptives.R ...")

# ---------------------------------------------------------------------------
# 0. Libraries
# ---------------------------------------------------------------------------

library(tidyverse)
library(tableone)
library(wesanderson)

wes_cols <- wes_palette("Darjeeling1", 2, type = "discrete")

# ---------------------------------------------------------------------------
# 1. Load analysis dataset
# ---------------------------------------------------------------------------
data <- readRDS(
  file.path(paths$data_processed, "analysis_data_full.rds")
)

analysis_data <- readRDS(
  file.path(paths$data_processed, "analysis_data.rds")
)

# ---------------------------------------------------------------------------
# 2. TABLE 1 — Randomization check (paper Table 1)
# ---------------------------------------------------------------------------

baseline <- data %>%
  filter(Round == 1) %>%
  mutate(
    Condition_Uncertainty =
      factor(Condition_Uncertainty,
             levels = c("Ambiguity","Risk")),
    Condition_SRP =
      factor(Condition_SRP,
             levels = c("High","Low"))
  )
names(baseline)
vars <- c(
  "Age",
  "Female",
  "Education_Level",
  "Income_1",
  "Risk_attitude",
  "UG_Person_A_1",
  "UG_P_B"
)

catVars <- c(
  "Female",
  "Education_Level",
  "Risk_attitude"
)
library(tableone)

table1 <- CreateTableOne(
  vars = vars,
  strata = c("Condition_Uncertainty","Condition_SRP"),
  data = baseline,
  factorVars = catVars,
  test = TRUE
)
print(table1, showAllLevels = TRUE)

baseline_amb <- baseline %>%
  filter(Condition_Uncertainty == "Ambiguity")

table1_overall <- CreateTableOne(
  vars = vars,
  data = baseline,
  factorVars = catVars,
  test = TRUE
)

print(table1_overall, showAllLevels = TRUE)


# ---------------------------------------------------------------------------
# 3. FIGURE — Mean Clicks Round 1
# --------------------------------------------------------------------------
fig_4 <- analysis_data %>%
  filter(Round == 1) %>%
  group_by(Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    Mean_Clicks = mean(Clicks, na.rm = TRUE),
    SD = sd(Clicks, na.rm = TRUE),
    SE = SD / sqrt(n()),
    N = n(),
    .groups = "drop"
  )

fig_4$Condition_Uncertainty <- factor(
  fig_4$Condition_Uncertainty,
  levels = c("Risk", "Ambiguity"),
  labels = c("Risk", "Ambiguity")
)


figure4 <- ggplot(fig_4,
             aes(x = Condition_SRP,
                 y = Mean_Clicks,
                 fill = Condition_Uncertainty)) +
  geom_col(width = .7, color = "black",
           position = position_dodge(.8)) +
  geom_errorbar(aes(ymin = Mean_Clicks - SE,
                    ymax = Mean_Clicks + SE),
                width = .2,
                position = position_dodge(.8)) +
  scale_fill_manual(values = wes_cols) +
  labs(
    x = "SRP condition",
    y = "Mean number of clicks (round 1)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())


figure4

ggsave(
  file.path(paths$output_figures,
            "Figure_4.png"),
  figure4, width = 7, height = 5, dpi = 300
)

# ---------------------------------------------------------------------------
# 4. FIGURE — Click dynamics across rounds
# ---------------------------------------------------------------------------
fig_5 <- analysis_data %>%
  group_by(Round, Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    se = sd(Clicks, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

diff_srp <- analysis_data %>%
  group_by(Round, Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Condition_SRP,
    values_from = mean_clicks
  ) %>%
  mutate(
    diff_high_low = High - Low   # IMPORTANT: matches your factor labels
  )

fig_5$Condition_Uncertainty <- factor(
  fig_5$Condition_Uncertainty,
  levels = c("Risk","Ambiguity"),
  labels = c("Risk","Ambiguity")
)

figure5 <- ggplot(
  fig_5,
  aes(
    x = Round,
    y = mean_clicks,
    color = Condition_SRP,
    group = Condition_SRP
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(ymin = mean_clicks - se,
        ymax = mean_clicks + se),
    width = .15,
    alpha = .5
  ) +
  scale_color_manual(
    values = wes_cols,
    labels = c("High-SRP","Low-SRP")
  ) +
  facet_wrap(~Condition_Uncertainty, nrow = 1) +
  labs(
    x = "Round",
    y = "Mean number of clicks",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

figure5



ggsave(
  file.path(paths$output_figures,
            "Figure_5.png"),
  figure5, width = 8, height = 4.5, dpi = 300
)


message("Appendix figures saved successfully.")
