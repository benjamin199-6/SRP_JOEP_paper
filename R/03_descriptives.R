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
library(tableone)

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
# --------------------------------------------------------------------------


fig_round1 <- analysis_data %>%
  filter(Round == 1) %>%
  group_by(Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    Mean_Clicks = mean(Clicks),
    SE = sd(Clicks)/sqrt(n()),
    .groups = "drop"
  )

fig_round1$Condition_Uncertainty <- factor(
  fig_round1$Condition_Uncertainty,
  levels = c("Risk", "Ambiguity"),
  labels = c("Risk", "Ambiguity")
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
    x = "SRP condition",
    y = "Mean number of clicks (round 1)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())


p1

ggsave(
  file.path(paths$output_figures,
            "Figure_4.png"),
  p1, width = 7, height = 5, dpi = 300
)

# ---------------------------------------------------------------------------
# 4. FIGURE — Click dynamics across rounds
# ---------------------------------------------------------------------------
fig_dyn <- analysis_data %>%
  group_by(Round, Condition_SRP, Condition_Uncertainty) %>%
  summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    se = sd(Clicks, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

# force panel order: risk left, ambiguity right
fig_dyn$Condition_Uncertainty <- factor(
  fig_dyn$Condition_Uncertainty,
  levels = c("Risk","Ambiguity"),
  labels = c("Risk","Ambiguity")
)

p2 <- ggplot(
  fig_dyn,
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

p2



ggsave(
  file.path(paths$output_figures,
            "Figure_5.png"),
  p2, width = 8, height = 4.5, dpi = 300
)

# ---------------------------------------------------------------------------
# 5. MECHANISM Table 
# ---------------------------------------------------------------------------
df <- analysis_data %>%
  mutate(Round = as.numeric(as.character(Round)))

tab_r1 <- df %>%
  filter(Round == 1) %>%
  group_by(Condition_Uncertainty, Condition_SRP) %>%
  summarise(
    mean = mean(Clicks, na.rm = TRUE),
    sd   = sd(Clicks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(value = sprintf("%.2f (%.2f)", mean, sd))



tab_fb <- df %>%
  filter(Round > 1) %>%
  group_by(
    Condition_Uncertainty,
    Condition_SRP,
    Feedback,
    Round
  ) %>%
  summarise(
    mean = mean(Clicks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(value = sprintf("%.2f", mean))


tab_fb_wide <- tab_fb %>%
  select(-mean) %>%
  pivot_wider(
    names_from = Round,
    values_from = value,
    names_prefix = "Round_"
  )

tab_fb_wide <- tab_fb_wide %>%
  mutate(
    Feedback = recode(Feedback,
                      "Feedback_bomb" = "Bomb",
                      "Feedback_no_bomb" = "No bomb"
    ),
    Condition = paste(Condition_Uncertainty, Condition_SRP, sep = ", ")
  )



df <- analysis_data %>%
  dplyr::select(
    ID,
    Round,
    Clicks,
    Condition_Uncertainty,
    Condition_SRP,
    Feedback, 
    Bomb_new
  ) %>%
  dplyr::mutate(
    Round = as.numeric(as.character(Round))
  ) %>%
  dplyr::arrange(ID, Round)

tabA_mean <- df %>%
group_by(Condition_Uncertainty, Condition_SRP, Round) %>%
  summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    .groups = "drop"
  )

tabA <- tabA_mean %>%
  group_by(Condition_Uncertainty, Condition_SRP) %>%
  arrange(Round) %>%
  mutate(
    delta = mean_clicks - lag(mean_clicks),
    pct_change = 100 * delta / lag(mean_clicks)
  ) %>%
  ungroup()


tabB_mean <- df %>%
  filter(Round > 1) %>%
  group_by(
    Condition_Uncertainty,
    Condition_SRP,
    Feedback,
    Round
  ) %>%
  summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    .groups = "drop"
  )

tabB <- tabB_mean %>%
  group_by(
    Condition_Uncertainty,
    Condition_SRP,
    Feedback
  ) %>%
  arrange(Round) %>%
  mutate(
    delta = mean_clicks - lag(mean_clicks),
    pct_change = 100 * delta / lag(mean_clicks)
  ) %>%
  ungroup()

tabB_r1 <- df %>%
  filter(Round == 1) %>%
  group_by(Condition_Uncertainty, Condition_SRP) %>%
  summarise(
    mean_clicks = mean(Clicks),
    .groups = "drop"
  ) %>%
  mutate(
    Feedback = "no feedback",
    Round = 1,
    delta = NA,
    pct_change = NA
  )
tabB_full <- bind_rows(tabB_r1, tabB)



# 

df_r1r2 <- data %>%
  filter(Round %in% c(1,2)) %>%
  group_by(ID) %>%
  summarise(
    Condition_Uncertainty = first(Condition_Uncertainty),
    Condition_SRP = first(Condition_SRP),
    Clicks_R1 = Clicks[Round == 1][1],
    Clicks_R2 = Clicks[Round == 2][1],
    Bomb_R1   = Bomb_new[Round == 1][1],
    .groups = "drop"
  )


df_r1r2 <- df_r1r2 %>%
  mutate(
    Feedback = ifelse(Bomb_R1 == 1, "Bomb", "No bomb"),
    delta_clicks = Clicks_R2 - Clicks_R1,
    pct_change = 100 * delta_clicks / Clicks_R1
  )

table=df_r1r2 %>%
  group_by(Condition_Uncertainty, Condition_SRP,Feedback) %>%
  summarise(
    mean_R1 = mean(Clicks_R1),
    mean_R2 = mean(Clicks_R2),
    mean_delta = mean(delta_clicks),
    n = n(),
    .groups = "drop"
  )

table

feols(
  Clicks_R2 ~ Bomb_R1 + Clicks_R1,
  data = df_r1r2
)

feols(
  Clicks ~ lag(Bomb_new) | ID + Round,
  cluster = ~ID,
  data = data
)

df_r1r2 <- data %>%
  filter(Round %in% c(1,2)) %>%
  group_by(ID) %>%
  summarise(
    Condition_Uncertainty = first(Condition_Uncertainty),
    Condition_SRP = first(Condition_SRP),
    Clicks_R1 = Clicks[Round == 1][1],
    Clicks_R2 = Clicks[Round == 2][1],
    Bomb_R1   = Bomb_new[Round == 1][1],
    delta = Clicks_R2 - Clicks_R1,
    .groups = "drop"
  )
anova_r1r2 <- aov(
  delta ~ Bomb_R1 * Condition_SRP * Condition_Uncertainty,
  data = df_r1r2
)

summary(anova_r1r2)


anova_r1r2_ctrl <- aov(
  Clicks_R2 ~ Clicks_R1 + Bomb_R1 * Condition_SRP * Condition_Uncertainty,
  data = df_r1r2
)

summary(anova_r1r2_ctrl)


library(ppcor)

pcor.test(
  df_r1r2$Clicks_R2,
  df_r1r2$Bomb_R1,
  df_r1r2$Clicks_R1
)
#### new

detla=df %>%
  group_by(ID) %>%
  mutate(delta = Clicks - lag(Clicks)) %>%
  filter(Round > 1) %>%
  group_by(Condition_Uncertainty, Condition_SRP, Feedback, Round) %>%
  summarise(mean_delta = mean(delta, na.rm = TRUE))







df_changes <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Clicks_lag = lag(Clicks),
    Feedback_prev = lag(Feedback),
    
    # Safe percentage change (avoid division by zero)
    Percent_Change_Clicks = dplyr::if_else(
      Clicks_lag > 0,
      100 * (Clicks - Clicks_lag) / Clicks_lag,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(
    !is.na(Percent_Change_Clicks),
    Feedback_prev != "no feedback"
  ) %>%
  mutate(
    Interaction = interaction(
      Condition_Uncertainty,
      Condition_SRP,
      sep = "_"
    )
  )

mean_changes <- df_changes %>%
  group_by(Feedback_prev, Interaction) %>%
  summarise(
    Mean_Change = mean(Percent_Change_Clicks, na.rm = TRUE),
    SE_Change   = sd(Percent_Change_Clicks, na.rm = TRUE) / sqrt(n()),
    N = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Explicit namespace prevents recode conflicts
    Feedback_prev = dplyr::recode(
      Feedback_prev,
      "Feedback_bomb"    = "Feedback: Bomb",
      "Feedback_no_bomb" = "Feedback: No Bomb"
    ),
    
    Interaction = dplyr::recode(
      Interaction,
      "Ambiguity_High" = "Ambiguity – High SRP",
      "Ambiguity_Low"  = "Ambiguity – Low SRP",
      "Risk_High"      = "Risk – High SRP",
      "Risk_Low"       = "Risk – Low SRP"
    )
  )

feedback_cols <- wes_palette("Moonrise2", 2, type = "discrete")

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
  
  scale_fill_manual(values = feedback_cols) +
  
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
    SE_Change   = sd(Change_from_R1, na.rm = TRUE)/sqrt(dplyr::n()),
    N           = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    # SAFE recoding (no masking possible)
    Feedback = dplyr::case_match(
      Feedback,
      "Feedback_bomb"    ~ "Feedback: Bomb",
      "Feedback_no_bomb" ~ "Feedback: No Bomb"
    )
  )


mean_changes_ambiguity <- mean_changes_R1 %>%
  filter(Condition_Uncertainty == "Ambiguity")

p_ambiguity <- ggplot(
  mean_changes_ambiguity,
  aes(
    x = factor(Round),
    y = Mean_Change,
    fill = Feedback
  )
) +
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
    alpha = .4,
    position = position_dodge(.7)
  ) +
  geom_text(
    aes(
      label = paste0("N=", N),
      vjust = ifelse(Mean_Change >= 0, -0.8, 1.4)
    ),
    position = position_dodge(.7),
    size = 3
  ) +
  scale_fill_manual(values = feedback_cols) +
  facet_wrap(
    ~Condition_SRP,
    labeller = labeller(
      Condition_SRP = c(
        High = "High SRP (Ambiguity)",
        Low  = "Low SRP (Ambiguity)"
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

# SAVE
ggsave(
  file.path(paths$output_figures,
            "FigureA11_Change_Baseline_Ambiguity.png"),
  p_ambiguity,
  width = 8,
  height = 5,
  dpi = 600
)

# ===========================================================================
# RISK FIGURE
# ===========================================================================

mean_changes_risk <- mean_changes_R1 %>%
  filter(Condition_Uncertainty == "Risk")

p_risk <- ggplot(
  mean_changes_risk,
  aes(
    x = factor(Round),
    y = Mean_Change,
    fill = Feedback
  )
) +
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
    alpha = .4,
    position = position_dodge(.7)
  ) +
  geom_text(
    aes(
      label = paste0("N=", N),
      vjust = ifelse(Mean_Change >= 0, -0.8, 1.4)
    ),
    position = position_dodge(.7),
    size = 3
  ) +
  scale_fill_manual(values = feedback_cols) +
  facet_wrap(
    ~Condition_SRP,
    labeller = labeller(
      Condition_SRP = c(
        High = "High SRP (Risk)",
        Low  = "Low SRP (Risk)"
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
# SAVE
ggsave(
  file.path(paths$output_figures,
            "FigureA12_Change_Baseline_Risk.png"),
  p_risk,
  width = 8,
  height = 5,
  dpi = 600
)

message("Appendix figures saved successfully.")
