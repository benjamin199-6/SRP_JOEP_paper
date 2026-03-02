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

vars <- c(
  "Age",
  "Female",
  "Education_Level",
  "Income_1",
  "Risk_attitude",
  "UG_Person_A_2",
  "UG_Person_A_1"
)

catVars <- c(
  "Female",
  "Education_Level",
  "Risk_attitude"
)

table1 <- CreateTableOne(
  vars = vars,
  strata = c("Condition_Uncertainty","Condition_SRP"),
  data = baseline,
  factorVars = catVars,
  test = TRUE
)

capture.output(
  print(table1,
        showAllLevels = TRUE,
        quote = FALSE,
        noSpaces = TRUE),
  file = file.path(paths$output_tables,
                   "Table1_randomization.txt")
)


# ---------------------------------------------------------------------------
# 3. FIGURE — Mean Clicks Round 1
# ---------------------------------------------------------------------------

fig_round1 <- analysis_data %>%
  filter(Round == 1) %>%
  group_by(Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    Mean_Clicks = mean(Clicks),
    SE = sd(Clicks)/sqrt(n()),
    .groups = "drop"
  )

p1 <- ggplot(fig_round1,
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
    x = "SRP Condition",
    y = "Mean Clicks (Round 1)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())


p1
ggsave(
  file.path(paths$output_figures,
            "Figure_mean_clicks_round1.png"),
  p1, width = 7, height = 5, dpi = 300
)

# ---------------------------------------------------------------------------
# 4. FIGURE — Click dynamics across rounds
# ---------------------------------------------------------------------------

fig_dyn <- analysis_data %>%
  group_by(Round, Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    Mean_Clicks = mean(Clicks),
    SE = sd(Clicks)/sqrt(n()),
    .groups = "drop"
  )

p2 <- ggplot(fig_dyn,
             aes(x = as.numeric(as.character(Round)),
                 y = Mean_Clicks,
                 color = Condition_SRP,
                 group = Condition_SRP)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin = Mean_Clicks - SE,
                    ymax = Mean_Clicks + SE),
                width = .15,
                alpha = .5) +
  scale_color_manual(
    values = wes_cols,
    labels = c("High SRP","Low SRP")
  ) +
  facet_wrap(~Condition_Uncertainty) +
  labs(
    x = "Round",
    y = "Mean Clicks",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
p2
ggsave(
  file.path(paths$output_figures,
            "Figure_click_dynamics.png"),
  p2, width = 8, height = 4.5, dpi = 300
)

# ---------------------------------------------------------------------------
# 5. MECHANISM FIGURE — Immediate feedback effect
# ---------------------------------------------------------------------------

df_changes <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Percent_Change_Clicks =
      100 * (Clicks - lag(Clicks)) / lag(Clicks),
    Feedback_prev = lag(Feedback)
  ) %>%
  ungroup() %>%
  filter(!is.na(Percent_Change_Clicks),
         Feedback_prev != "no feedback") %>%
  mutate(
    Interaction =
      interaction(Condition_Uncertainty,
                  Condition_SRP,
                  sep = "_")
  )

mean_changes <- df_changes %>%
  group_by(Feedback_prev, Interaction) %>%
  summarise(
    Mean_Change = mean(Percent_Change_Clicks),
    SE_Change = sd(Percent_Change_Clicks)/sqrt(n()),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Feedback_prev = recode(
      Feedback_prev,
      "Feedback_bomb" = "Feedback: Bomb",
      "Feedback_no_bomb" = "Feedback: No Bomb"
    ),
    Interaction = recode(
      Interaction,
      "Ambiguity_High" = "Ambiguity – High SRP",
      "Ambiguity_Low"  = "Ambiguity – Low SRP",
      "Risk_High"      = "Risk – High SRP",
      "Risk_Low"       = "Risk – Low SRP"
    )
  )

p_feedback <- ggplot(mean_changes,
                     aes(x = Interaction,
                         y = Mean_Change,
                         fill = Feedback_prev)) +
  
  geom_col(
    position = position_dodge(.7),
    width = .65
  ) +
  
  geom_errorbar(
    aes(
      ymin = Mean_Change - SE_Change,
      ymax = Mean_Change + SE_Change
    ),
    width = .18,
    position = position_dodge(.7),
    alpha = .4
  ) +
  
  # -------------------------------
# SHOW SAMPLE SIZE (N)
# -------------------------------
geom_text(
  aes(
    label = paste0("N=", N),
    vjust = ifelse(Mean_Change >= 0, -0.8, 1.4)
  ),
  position = position_dodge(.7),
  size = 3
) +
  
  scale_fill_manual(values = wes_cols) +
  
  labs(
    x = NULL,
    y = "Mean % Change in Clicks",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p_feedback

ggsave(
  file.path(paths$output_figures,
            "Fig_Feedback_Effect_Clicks.png"),
  p_feedback,
  width = 10, height = 5, dpi = 300
)


#### Updating 

df_amb <- analysis_data %>%
  dplyr::filter(Condition_Uncertainty == "Ambiguity") %>%
  dplyr::mutate(
    
    # beliefs → probabilities
    pi0 = Belief_0 / 100,
    pi1 = Belief_1 / 100,
    pi2 = Belief_2 / 100,
    
    # expected bombs
    exp_bombs =
      (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100,
    
    bomb_occured =
      ifelse(Feedback == "Feedback_bomb", 1, 0)
  )



df_amb <- analysis_data %>%
  dplyr::filter(Condition_Uncertainty == "Ambiguity") %>%
  dplyr::mutate(
    
    # beliefs → probabilities
    pi0 = Belief_0 / 100,
    pi1 = Belief_1 / 100,
    pi2 = Belief_2 / 100,
    
    # expected bombs
    exp_bombs =
      (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100,
    
    bomb_occured =
      ifelse(Feedback == "Feedback_bomb", 1, 0)
  )


safe_prob <- function(s, k, N = 100) {
  if (k > (N - s)) return(0)
  choose(N - s, k) / choose(N, k)
}

expected_bomb_prob_mixture <- function(pi0, pi1, pi2, k, N = 100) {
  
  p_safe0 <- safe_prob(0, k, N)
  p_safe1 <- safe_prob(1, k, N)
  p_safe2 <- safe_prob(2, k, N)
  
  p_safe <- pi0*p_safe0 + pi1*p_safe1 + pi2*p_safe2
  1 - p_safe
}


df_amb <- df_amb %>%
  rowwise() %>%
  mutate(
    exp_p_bomb =
      expected_bomb_prob_mixture(pi0, pi1, pi2,
                                 k = Clicks, N = 100),
    surprise = bomb_occured - exp_p_bomb
  ) %>%
  ungroup() %>%
  arrange(Subject, Round)


panel <- df_amb %>%
  group_by(Subject) %>%
  arrange(Round, .by_group = TRUE) %>%
  mutate(
    exp_bombs_next = lead(exp_bombs),
    Clicks_next    = lead(Clicks),
    
    d_exp_bombs = exp_bombs_next - exp_bombs,
    d_clicks    = Clicks_next - Clicks
  ) %>%
  ungroup() %>%
  filter(!is.na(d_exp_bombs),
         !is.na(d_clicks))

panel_clicks <- panel %>%
  mutate(
    surprise_dir = case_when(
      surprise > 0  ~ "Unexpected Bomb (worse)",
      TRUE          ~ "Safer than Expected"
    )
  ) %>%
  group_by(Condition_SRP, surprise_dir) %>%
  summarise(
    mean_d_clicks = mean(d_clicks, na.rm = TRUE),
    se_d_clicks   = sd(d_clicks, na.rm = TRUE)/sqrt(n()),
    N = n(),
    .groups = "drop"
  )


p_behavior_update <- ggplot(
  panel_clicks,
  aes(x = surprise_dir,
      y = mean_d_clicks,
      fill = Condition_SRP)
) +
  
  geom_col(position = position_dodge(.7),
           width = .65,
           color = NA) +
  
  geom_errorbar(
    aes(ymin = mean_d_clicks - se_d_clicks,
        ymax = mean_d_clicks + se_d_clicks),
    width = .18,
    alpha = .4,
    position = position_dodge(.7)
  ) +
  
  geom_text(
    aes(label = paste0("N=", N),
        vjust = ifelse(mean_d_clicks >= 0, -0.8, 1.4)),
    position = position_dodge(.7),
    size = 3
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_fill_manual(
    values = wes_cols,
    labels = c("High SRP","Low SRP")
  ) +
  
  labs(
    x = NULL,
    y = "Change in Clicks (Δ Risk-Taking)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_behavior_update

ggsave(
  file.path(paths$output_figures,
            "Figure_Behavioral_Updating.png"),
  p_behavior_update,
  width = 10,
  height = 5,
  dpi = 300
)
# ---------------------------------------------------------------------------
# 6. APPENDIX — Change relative to Round 1
# ---------------------------------------------------------------------------
feedback_cols <- wes_palette("Moonrise2", 2, type = "discrete")
names(feedback_cols) <- c(
  "Feedback: Bomb",
  "Feedback: No Bomb"
)


df_changes_R1 <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Clicks_R1 = first(Clicks),
    Change_from_R1 = Clicks - Clicks_R1
  ) %>%
  ungroup() %>%
  filter(Round != 1)



mean_changes_R1 <- df_changes_R1 %>%
  group_by(
    Round,
    Feedback,
    Condition_Uncertainty,
    Condition_SRP
  ) %>%
  summarise(
    Mean_Change = mean(Change_from_R1, na.rm = TRUE),
    SE_Change   = sd(Change_from_R1, na.rm = TRUE)/sqrt(n()),
    N           = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Feedback = recode(
      Feedback,
      "Feedback_bomb"    = "Feedback: Bomb",
      "Feedback_no_bomb" = "Feedback: No Bomb"
    )
  )

mean_changes_ambiguity <- mean_changes_R1 %>%
  filter(Condition_Uncertainty == "Ambiguity")

p_ambiguity <- ggplot(
  mean_changes_ambiguity,
  aes(x = factor(Round),
      y = Mean_Change,
      fill = Feedback)
) +
  geom_col(position = position_dodge(.7),
           width = .65,
           color = NA) +
  
  geom_errorbar(
    aes(ymin = Mean_Change - SE_Change,
        ymax = Mean_Change + SE_Change),
    width = .18,
    alpha = .4,
    position = position_dodge(.7)
  ) +
  
  # ---- Ns ----
geom_text(
  aes(label = paste0("N=", N),
      vjust = ifelse(Mean_Change >= 0, -0.8, 1.4)),
  position = position_dodge(.7),
  size = 3
) +
  
  scale_fill_manual(values = feedback_cols)+
  
  facet_wrap(
    ~Condition_SRP,
    labeller = labeller(
      Condition_SRP = c(
        High = "High SRP",
        Low  = "Low SRP"
      )
    )
  ) +
  
  labs(
    x = "Round",
    y = "Mean Change in Clicks (relative to Round 1)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


p_ambiguity


mean_changes_risk <- mean_changes_R1 %>%
  filter(Condition_Uncertainty == "Risk")

p_risk <- ggplot(
  mean_changes_risk,
  aes(x = factor(Round),
      y = Mean_Change,
      fill = Feedback)
) +
  geom_col(position = position_dodge(.7),
           width = .65,
           color = NA) +
  
  geom_errorbar(
    aes(ymin = Mean_Change - SE_Change,
        ymax = Mean_Change + SE_Change),
    width = .18,
    alpha = .4,
    position = position_dodge(.7)
  ) +
  
  geom_text(
    aes(label = paste0("N=", N),
        vjust = ifelse(Mean_Change >= 0, -0.8, 1.4)),
    position = position_dodge(.7),
    size = 3
  ) +
  
  scale_fill_manual(values = feedback_cols) +
  
  facet_wrap(
    ~Condition_SRP,
    labeller = labeller(
      Condition_SRP = c(
        High = "High SRP",
        Low  = "Low SRP"
      )
    )
  ) +
  
  labs(
    x = "Round",
    y = "Mean Change in Clicks (relative to Round 1)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_risk

ggsave(
  file.path(paths$output_figures,
            "FigureA12_Change_Baseline_Risk.png"),
  p_risk,
  width = 8,
  height = 5,
  dpi = 600
)

message("03_descriptives.R completed successfully.")