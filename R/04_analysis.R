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
library(rcompanion)
library(rstatix)
library(afex)
library(emmeans)
# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
analysis_data <- readRDS(
  file.path(paths$data_processed, "analysis_data.rds")
)

r1 <- filter(analysis_data, Round == 1)

# Section 4.1: desicion in the first round
## Hypothese 1 & 2: ANOVA for desicion in round 1 
anova_test(data = r1, Clicks~Condition_SRP*Condition_Uncertainty, effect.size = "pes")


# Hypothese 1 & 2: non-parametric test 
scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = r1)
scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = analysis_data)



#Section 4.2: desicion across rounds

analysis_data$Round <- factor(analysis_data$Round)

model <- aov_ez(
  id = "ID",                      
  dv = "Clicks",
  data = analysis_data,
  within = "Round",
  between = c("Condition_SRP", "Condition_Uncertainty")
)


anova(model)

means_clicks=analysis_data %>%
  group_by(Condition_SRP, Condition_Uncertainty, Round) %>%
  summarise(
    n = n(),
    mean_clicks = mean(Clicks, na.rm = TRUE),
    sd_clicks = sd(Clicks, na.rm = TRUE),
    .groups = "drop"
  )

print(means_clicks)

emm <- emmeans(
  model,
  ~ Condition_SRP | Condition_Uncertainty * Round
)


contrast_results <- contrast(
  emm,
  method = "revpairwise"
)

print(contrast_results)

mean_clicks_by_uncertainty <- analysis_data %>%
  group_by(Condition_Uncertainty, Round) %>%
  summarise(
    n = n(),
    mean_clicks = mean(Clicks, na.rm = TRUE),
    sd_clicks = sd(Clicks, na.rm = TRUE),
    .groups = "drop"
  )

print(mean_clicks_by_uncertainty)


#Section 4.3:	Underlying decision-mechanisms: beliefs and feedback 

# Correlation between beliefs about the number of hidden bombs and the number of collected boxes 

analysis_data <- analysis_data %>%
  mutate(
    expected_bombs =
      (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100
  )


# Correlation: High SRP
cor.test(
  analysis_data$expected_bombs[analysis_data$Condition_SRP == "High"],
  analysis_data$Clicks[analysis_data$Condition_SRP == "High"]
)

# Correlation: Low SRP
cor.test(
  analysis_data$expected_bombs[analysis_data$Condition_SRP == "Low"],
  analysis_data$Clicks[analysis_data$Condition_SRP == "Low"]
)

## Belief summary  
belief_summary <- analysis_data %>%
  group_by(Condition_SRP) %>%
  summarise(
    p0 = mean(Belief_0, na.rm = TRUE) / 100,
    p1 = mean(Belief_1, na.rm = TRUE) / 100,
    p2 = mean(Belief_2, na.rm = TRUE) / 100,
    .groups = "drop"
  )

print(belief_summary)




df_long <- analysis_data %>%
  select(ID, Condition_SRP, Belief_0, Belief_1, Belief_2) %>%
  pivot_longer(
    cols = starts_with("Belief"),
    names_to = "Bombs",
    values_to = "Belief"
  ) 

## Anova belief 
df_long=df_long %>%
  filter(!is.na(Belief)) %>%
  group_by(ID) %>%
  ungroup() %>%
  mutate(
    Bombs = factor(Bombs, levels = c("Belief_0","Belief_1","Belief_2")),
    Condition_SRP = factor(Condition_SRP),
    ID = factor(ID)
  )


anova_model_belief <- aov_ez(
  id = "ID",
  dv = "Belief",
  data = df_long,
  within = "Bombs",
  between = "Condition_SRP",
  type = 3
)

anova_model_belief # checked



# Feedback
df_panel <- analysis_data %>% select(ID, Round, Clicks, Bomb_new, Condition_SRP,Condition_Uncertainty) %>% 
  arrange(ID, Round) %>%
  group_by(ID) %>%
  mutate(
    lag_bomb = lag(Bomb_new),
    mean_clicks = mean(Clicks, na.rm = TRUE)
  ) %>%
  ungroup()

df_r2 <- df_panel %>%
  filter(Round == 2) %>%
  group_by(ID) %>%
  mutate(clicks_r1 = lag(Clicks)) %>%
  ungroup()

desc_feedback <- df_r2 %>%
  group_by(lag_bomb) %>%
  summarise(
    n = n(),
    mean_clicks = mean(Clicks, na.rm = TRUE),
    sd_clicks = sd(Clicks, na.rm = TRUE),
    .groups = "drop"
  )

print(desc_feedback)


# Anova feedback 
anova_model_feedback <- aov_ez(
  id = "ID",
  dv = "Clicks",
  data = df_panel %>% filter(Round == 2),
  between = c("lag_bomb", "Condition_Uncertainty", "Condition_SRP"),
  type = 3
)
print(anova_model_feedback)


# Correlation 
cor.test(
  df_panel$lag_bomb[df_panel$Round == 2],
  df_panel$Clicks[df_panel$Round == 2]
)


cor.test(df_panel$lag_bomb, df_panel$Clicks)

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



