###############################################################################
# 04_analysis.R
#
# Purpose:
# Replicate econometric analysis of the JOEP paper.
#
# Sections:
# - Round-specific regressions
# - Robustness (OLS)
# - Pooled models
# - Belief models
# - Inequality preferences
# - Nonparametric tests
###############################################################################

message("Running 04_analysis.R ...")

library(dplyr)
library(fixest)
library(lmtest)
library(sandwich)
library(stargazer)
library(mfx)
library(coin)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

analysis_data <- readRDS(
  file.path(paths$data_processed, "analysis_data.rds")
)

r1 <- filter(analysis_data, Round == 1)


library(rstatix)
anova_test(data = r1, Clicks~Condition_SRP*Condition_Uncertainty, effect.size = "pes")


### non parametric:
#install.packages("rcompanion")
library(rcompanion)

scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = r1)
scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = analysis_data)



library(afex)


analysis_data$Round <- factor(analysis_data$Round)

model <- aov_ez(
  id = "ID",                      # or "ID" depending on your data
  dv = "Clicks",
  data = analysis_data,
  within = "Round",
  between = c("Condition_SRP", "Condition_Uncertainty")
)

model

library(dplyr)
analysis_data %>%
  group_by(Condition_SRP, Condition_Uncertainty, Round) %>%
  summarise(
    n = n(),
    mean_clicks = mean(Clicks, na.rm = TRUE),
    sd_clicks = sd(Clicks, na.rm = TRUE),
    .groups = "drop"
  )
library(emmeans)

emm <- emmeans(
  model,
  ~ Condition_SRP | Condition_Uncertainty * Round
)

emm


contrast_results <- contrast(
  emm,
  method = "revpairwise"
)

contrast_results

anova_table <- anova(model)
anova_table


names(analysis_data)

str(analysis_data)

#4.3.	Underlying decision-mechanisms: beliefs and feedback 

df_beliefs <- analysis_data %>%
  filter(
    !is.na(Belief_0) |
      !is.na(Belief_1) |
      !is.na(Belief_2)
  )


df_beliefs %>%
  group_by(Condition_SRP, Round) %>%
  summarise(
    mean_Belief_0 = mean(Belief_0, na.rm = TRUE),
    mean_Belief_1 = mean(Belief_1, na.rm = TRUE),
    mean_Belief_2 = mean(Belief_2, na.rm = TRUE),
    n = n()
  )


df_long <- analysis_data %>%
  dplyr::select(ID, Condition_SRP, Belief_0, Belief_1, Belief_2) %>%
  tidyr::pivot_longer(
    cols = starts_with("Belief"),
    names_to = "Event",
    values_to = "Belief"
  ) %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(all(!is.na(Belief))) %>%   # 🔥 wichtig
  dplyr::ungroup() %>%
  dplyr::mutate(
    Event = factor(Event, levels = c("Belief_0", "Belief_1", "Belief_2")),
    Condition_SRP = factor(Condition_SRP),
    ID = factor(ID)
  )

anova_model <- afex::aov_ez(
  id = "ID",
  dv = "Belief",
  data = df_long,
  within = "Event",
  between = "Condition_SRP",
  type = 3
)

anova_model



analysis_data <- analysis_data %>%
  mutate(
    expected_bombs =
      (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100
  )


# High SRP
cor.test(
  analysis_data$expected_bombs[analysis_data$Condition_SRP == "High"],
  analysis_data$Clicks[analysis_data$Condition_SRP == "High"]
)

# Low SRP
cor.test(
  analysis_data$expected_bombs[analysis_data$Condition_SRP == "Low"],
  analysis_data$Clicks[analysis_data$Condition_SRP == "Low"]
)

#### Correlation of bomb in R1 and Clicks in subsequent round(s)

df_r1r2 <- analysis_data %>%
  filter(Round %in% c(1,2)) %>%
  group_by(ID) %>%
  summarise(
    Clicks_R1 = Clicks[Round == 1][1],
    Clicks_R2 = Clicks[Round == 2][1],
    Bomb_R1   = Bomb_new[Round == 1][1],
    .groups = "drop"
  )

cor.test(df_r1r2$Bomb_R1, df_r1r2$Clicks_R2)

# over all rounds 


df_panel <- analysis_data %>% select(ID, Round, Clicks, Bomb_new, Condition_SRP,Condition_Uncertainty) %>% 
  arrange(ID, Round) %>%
  group_by(ID) %>%
  mutate(
    lag_bomb = lag(Bomb_new),
    mean_clicks = mean(Clicks, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_bomb))

cor.test(df_panel$lag_bomb, df_panel$Clicks)


# For SRP and Uncertainty conditons 
res_full <- df_panel %>%
  group_split(Condition_SRP, Condition_Uncertainty) %>%
  map_df(function(df_sub) {
    test <- cor.test(df_sub$lag_bomb, df_sub$Clicks)
    tibble(
      Condition_SRP = df_sub$Condition_SRP[1],
      Condition_Uncertainty = df_sub$Condition_Uncertainty[1],
      r = test$estimate,
      p = test$p.value,
      n = nrow(df_sub)
    )
  })

df_panel$Bomb_new=as.factor(df_panel$Bomb_new)


feols(
  Clicks ~ lag(Bomb_new) |ID+ Round,
  cluster = ~ID,
  data = df_panel
)

df_r1r2 <- analysis_data %>%
  filter(Round %in% c(1, 2)) %>%
  group_by(ID) %>%
  filter(any(Round == 1) & any(Round == 2)) %>%  # ensure complete pairs
  summarise(
    Condition_Uncertainty = first(Condition_Uncertainty),
    Condition_SRP = first(Condition_SRP),
    Clicks_R1 = Clicks[Round == 1][1],
    Clicks_R2 = Clicks[Round == 2][1],
    Bomb_R1   = Bomb_new[Round == 1][1],
    delta = Clicks_R2 - Clicks_R1,
    .groups = "drop"
  )
df_r1r2$Bomb_R1=as.factor(df_r1r2$Bomb_R1)
anova_model=aov(delta ~ Bomb_R1 + Condition_Uncertainty, data = df_r1r2)
TukeyHSD(anova_model)

### Corr between belief and feedback +

analysis_data <- analysis_data %>%
  mutate(
    expected_bombs =
      (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100
  )


df_belief_panel <- analysis_data %>%
  arrange(ID, Round) %>%
  group_by(ID) %>%
  mutate(
    lag_bomb = lag(Bomb_new),
    belief_next = expected_bombs   # belief in current round
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_bomb), !is.na(belief_next))


cor.test(df_belief_panel$lag_bomb, df_belief_panel$belief_next)


##hiers
res_srp_beliefs <- df_belief_panel %>%
  group_split(Condition_SRP) %>%
  map_df(function(df_sub) {
    test <- cor.test(df_sub$lag_bomb, df_sub$belief_next)
    tibble(
      Condition_SRP = df_sub$SRP[1],
      r = test$estimate,
      p = test$p.value,
      n = nrow(df_sub)
    )
  })

res_srp_beliefs











#old
####

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



table_changes <- df_changes %>%
  group_by(Feedback_prev, Interaction, Round) %>%
  summarise(
    Mean_Change = mean(Percent_Change_Clicks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Round,
    values_from = Mean_Change
  )
table_changes



df_panel <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    # previous round variables
    Feedback_prev = lag(Feedback),
    Clicks_prev   = lag(Clicks),
    
    # optional: change in behavior
    d_clicks = Clicks - Clicks_prev
  ) %>%
  ungroup()


df_panel <- df_panel %>%
  filter(!is.na(Feedback_prev))

analysis_data <- analysis_data %>%
  dplyr::mutate(
    Bomb_any = ifelse(Bombs_clicked > 0, 1, 0)
  )


analysis_data <- analysis_data %>%
  dplyr::mutate(
    Bomb_any = factor(Bomb_any,
                      levels = c(0,1),
                      labels = c("No Bomb","Bomb"))
  )



df_panel <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Bomb_prev   = lag(Bomb_any),
    Clicks_prev = lag(Clicks)
  ) %>%
  ungroup() %>%
  filter(!is.na(Bomb_prev))

names(df_panel)
library(tidyr)

table_feedback <- df_panel %>%
  group_by(Condition_Uncertainty,Condition_SRP, Bomb_prev, Round) %>%
  summarise(
    Mean_Clicks = mean(Clicks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Round,
    values_from = Mean_Clicks
  ) %>%
  arrange(Condition_SRP, Bomb_prev)



df_changes <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Clicks_lag = lag(Clicks),
    Bomb_prev  = lag(Bombs_clicked > 0),
    d_clicks   = Clicks - Clicks_lag
  ) %>%
  ungroup() %>%
  filter(!is.na(Bomb_prev)) %>%
  mutate(
    Bomb_prev = factor(Bomb_prev,
                       levels = c(FALSE, TRUE),
                       labels = c("No Bomb","Bomb"))
  )


table_round <- df_changes %>%
  group_by(Condition_Uncertainty,
           Condition_SRP,
           Bomb_prev) %>%
  summarise(
    Mean_Change = mean(d_clicks, na.rm = TRUE),
    SE = sd(d_clicks, na.rm = TRUE)/sqrt(n()),
    N = n(),
    .groups = "drop"
  )


library(dplyr)

df_panel <- analysis_data %>%
  arrange(Subject, Round) %>%
  group_by(Subject) %>%
  mutate(
    Bomb_prev = lag(Bombs_clicked > 0),   # binary: hit bomb before
    Clicks_t  = Clicks
  ) %>%
  ungroup() %>%
  filter(!is.na(Bomb_prev))


names(analysis_data)
x=analysis_data %>%
  dplyr::group_by(
    Condition_SRP,
    Condition_Uncertainty,
    Round,
    Feedback
  ) %>%
  dplyr::summarise(
    mean_clicks = mean(Clicks, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )
