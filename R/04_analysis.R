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



# ---------------------------------------------------------------------------
# Round-specific regressions
# ---------------------------------------------------------------------------

r1 <- filter(analysis_data, Round == 1)
r2 <- filter(analysis_data, Round == 2)
r3 <- filter(analysis_data, Round == 3)
r4 <- filter(analysis_data, Round == 4)
r5 <- filter(analysis_data, Round == 5)

names(r1)

model_formula1 <- Clicks ~ Condition_SRP +
  Condition_Uncertainty 

model_formula <- Clicks ~ Condition_SRP +
Condition_Uncertainty + Condition_SRP *
  Condition_Uncertainty
  
  Condition_Uncertainty 
# Poisson marginal effects (paper specification)

reg1.1 <- poissonmfx(model_formula1, data = r1, atmean = TRUE, robust = TRUE)
reg1.2 <- poissonmfx(model_formula,  data = r2, atmean = TRUE, robust = TRUE)
reg1.3 <- poissonmfx(model_formula,  data = r3, atmean = TRUE, robust = TRUE)
reg1.4 <- poissonmfx(model_formula,  data = r4, atmean = TRUE, robust = TRUE)
reg1.5 <- poissonmfx(model_formula,  data = r5, atmean = TRUE, robust = TRUE)

# ---------------------------------------------------------------------------
# OLS robustness (HC1 SE)
# ---------------------------------------------------------------------------

ols1 <- lm(model_formula1, data = r1)


library(rstatix)
anova_test(data = r1, Clicks~Condition_SRP*Condition_Uncertainty, effect.size = "pes")


### non parametric:
install.packages("rcompanion")
library(rcompanion)

scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = r1)
scheirerRayHare(Clicks ~ Condition_SRP + Condition_Uncertainty + Condition_SRP:Condition_Uncertainty, data = analysis_data)

# wilcox.test(Clicks ~ Condition_SRP, data = analysis_data)












str(analysis_data$Round)




anova_test(
  data = analysis_data,
  dv = Clicks,
  wid = ID,
  within = Round,
  between = c(Condition_SRP, Condition_Uncertainty),
  effect.size = "pes"
)

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
### old

lm=lm(data=r1,  Clicks~Condition_SRP:Condition_Uncertainty)
summary(lm)
car::Anova(lm, type = 2)

r1 %>% group_by(Condition_SRP,Condition_Uncertainty) %>% summarise(mean=mean(Clicks), sd=sd(Clicks))



ols2 <- lm(model_formula,  data = r2)
ols3 <- lm(model_formula,  data = r3)
ols4 <- lm(model_formula,  data = r4)
ols5 <- lm(model_formula,  data = r5)

summary(ols1)
summary(ols2)
summary(ols3)
summary(ols4)
summary(ols5)

emm1 <- emmeans(ols1, ~ Condition_SRP * Condition_Uncertainty)

pairs(emm1, adjust = "tukey")

se1 <- sqrt(diag(vcovHC(ols1, type="HC1")))
se2 <- sqrt(diag(vcovHC(ols2, type="HC1")))
se3 <- sqrt(diag(vcovHC(ols3, type="HC1")))
se4 <- sqrt(diag(vcovHC(ols4, type="HC1")))
se5 <- sqrt(diag(vcovHC(ols5, type="HC1")))

# Print regression table
stargazer(
  ols1, ols2, ols3, ols4, ols5,
  type = "text",
  se = list(se1,se2,se3,se4,se5),
  title = "Round-specific OLS regressions"
)


stargazer(
  reg1.1$fit,
  reg1.2$fit,
  reg1.3$fit,
  reg1.4$fit,
  reg1.5$fit,
  type = "text",
  title = "Round-specific Poisson regressions (Marginal Effects)",
  
  coef = list(
    reg1.1$mfxest[,1],
    reg1.2$mfxest[,1],
    reg1.3$mfxest[,1],
    reg1.4$mfxest[,1],
    reg1.5$mfxest[,1]
  ),
  
  se = list(
    reg1.1$mfxest[,2],
    reg1.2$mfxest[,2],
    reg1.3$mfxest[,2],
    reg1.4$mfxest[,2],
    reg1.5$mfxest[,2]
  ),
  
  column.labels = c("R1","R2","R3","R4","R5"),
  dep.var.labels = "Clicks",
  digits = 3
)



### ANCOVA
library(dplyr)
library(purrr)
library(car)


r1 <- analysis_data %>%
  filter(Round == 1)


model_r1 <- lm(
  Clicks ~ Condition_SRP * Condition_Uncertainty, 
  data = r1
)

library(car)

Anova(model_r1, type = 2)


library(emmeans)

emm <- emmeans(model_r1, ~ Condition_SRP * Condition_Uncertainty)
pairs(emm, adjust = "tukey")
emmeans(model_r1, pairwise ~ Condition_SRP | Condition_Uncertainty,
        adjust = "tukey")


r1 <- analysis_data %>%
  filter(Round == 1)

r1$Condition_SRP <- factor(r1$Condition_SRP)
r1$Condition_Uncertainty <- factor(r1$Condition_Uncertainty)
r1$group <- interaction(r1$Condition_SRP, r1$Condition_Uncertainty)

kruskal.test(Clicks ~ group, data = r1)



#### Joint analysis 
  
#---- 4) Joint / pooled analysis (all rounds; paper Table 3 style) ----

model_1 <- Clicks ~ Condition_SRP * Condition_Uncertainty*Round
model_2 <- Clicks ~ Condition_SRP:Condition_Uncertainty

# Poisson marginal effects 
pois_all_1 <- poissonmfx(data = analysis_data, formula = model_1, atmean = TRUE, robust = TRUE)
pois_all_2 <- poissonmfx(data = analysis_data, formula = model_2, atmean = TRUE, robust = TRUE)

# OLS robustness 
ols_all_1 <- lm(model_1, data = analysis_data)
ols_all_2 <- lm(model_2, data = analysis_data)

summary(ols_all_1)
emm1 <- emmeans(ols_all_1, ~ Condition_SRP * Condition_Uncertainty*Round)
pairs(emm1, adjust = "tukey")

se_ols_all_1 <- sqrt(diag(vcovHC(ols_all_1, type = "HC1")))
se_ols_all_2 <- sqrt(diag(vcovHC(ols_all_2, type = "HC1")))

stargazer(
  # underlying fitted models (needed only for structure)
  pois_all_1$fit,
  pois_all_2$fit,
  ols_all_1,
  ols_all_2,
  
  type = "text",   # change to "latex" for paper
  title = "Pooled Regression Results (All Rounds)",
  
  dep.var.labels = "Clicks",
  
  column.labels = c(
    "Poisson: Main",
    "Poisson: Interaction",
    "OLS: Main",
    "OLS: Interaction"
  ),
  
  # ---- IMPORTANT PART ----
  coef = list(
    pois_all_1$mfxest[,1],
    pois_all_2$mfxest[,1],
    coef(ols_all_1),
    coef(ols_all_2)
  ),
  
  se = list(
    pois_all_1$mfxest[,2],
    pois_all_2$mfxest[,2],
    se_ols_all_1,
    se_ols_all_2
  ),
  
  digits = 3,
  align = TRUE,
  no.space = TRUE
)


library(car)
library(sandwich)
library(lmtest)
library(emmeans)


model_wald <- lm(
  Clicks ~ Condition_SRP * Condition_Uncertainty,
  data = analysis_data
)
vc <- vcovHC(model_wald, type = "HC1")
#Ambiguity High vs Risk High
linearHypothesis(
  model_wald,
  "Condition_UncertaintyRisk = 0",
  vcov = vc
)

# A low vs R low 
linearHypothesis(
  model_wald,
  "Condition_UncertaintyRisk +
   Condition_SRPLow:Condition_UncertaintyRisk = 0",
  vcov = vc
)

emmeans(model_wald,
        pairwise ~ Condition_Uncertainty | Condition_SRP)


# Robustness 

amb_data <- analysis_data %>%
  filter(Condition_Uncertainty == "Ambiguity")

# Wilcoxon rank-sum (Mann–Whitney)
wilcox_amb <- wilcox.test(
  Clicks ~ Condition_SRP,
  data = amb_data,
  alternative = "greater",
  exact = FALSE,
  conf.int = TRUE
)

print(wilcox_amb)

# Permutation Wilcoxon (recommended robustness)
perm_amb <- wilcox_test(
  Clicks ~ Condition_SRP,
  data = amb_data,
  alternative = "greater",
  distribution = approximate(B = 10000)
)

print(perm_amb)

# Extract statistics
W_amb  <- wilcox_amb$statistic
p_amb  <- wilcox_amb$p.value
Z_amb  <- statistic(perm_amb, type = "standardized")
p_perm_amb <- pvalue(perm_amb)

# -------------------------------------------------------------------
# 2. Risk condition
# High SRP vs Low SRP
# -------------------------------------------------------------------

risk_data <- analysis_data %>%
  filter(Condition_Uncertainty == "Risk")

wilcox_risk <- wilcox.test(
  Clicks ~ Condition_SRP,
  data = risk_data,
  alternative = "greater",
  exact = FALSE,
  conf.int = TRUE
)

print(wilcox_risk)

perm_risk <- wilcox_test(
  Clicks ~ Condition_SRP,
  data = risk_data,
  alternative = "greater",
  distribution = approximate(B = 10000)
)

print(perm_risk)

W_risk  <- wilcox_risk$statistic
p_risk  <- wilcox_risk$p.value
Z_risk  <- statistic(perm_risk, type = "standardized")
p_perm_risk <- pvalue(perm_risk)


######### Mechanisms
# ---- Prepare data ----
  ambiguity_df <- analysis_data %>%
    filter(Condition_Uncertainty == "Ambiguity") %>%
    mutate(
      average_belief =
        (0 * Belief_0 + 1 * Belief_1 + 2 * Belief_2) / 100
    )

#------------------------------------------------------------
# Models
#------------------------------------------------------------

belief_reg_overall <- lm(
  average_belief ~ Condition_SRP  + Feedback+
    Age + Female + Round + Risk_attitude,
  data = ambiguity_df
)

belief_reg_zero <- lm(
  Belief_0 ~ Condition_SRP  + Feedback+
    Age + Female + Round + Risk_attitude,
  data = ambiguity_df
)

belief_reg_one <- lm(
  Belief_1 ~ Condition_SRP + Feedback +
    Age + Female + Round + Risk_attitude,
  data = ambiguity_df
)

belief_reg_two <- lm(
  Belief_2 ~ Condition_SRP  + Feedback+
    Age + Female + Round + Risk_attitude,
  data = ambiguity_df
)

#------------------------------------------------------------
# HC1 robust standard errors (JoEP style)
#------------------------------------------------------------

se_overall <- sqrt(diag(vcovHC(belief_reg_overall, type = "HC1")))
se_zero    <- sqrt(diag(vcovHC(belief_reg_zero, type = "HC1")))
se_one     <- sqrt(diag(vcovHC(belief_reg_one, type = "HC1")))
se_two     <- sqrt(diag(vcovHC(belief_reg_two, type = "HC1")))

#------------------------------------------------------------
# Stargazer table
#------------------------------------------------------------

stargazer(
  belief_reg_overall,
  belief_reg_zero,
  belief_reg_one,
  belief_reg_two,
  
  type = "text",   # change to "latex" for paper
  
  title = "Belief Updating Regressions (Ambiguity Condition)",
  
  column.labels = c(
    "Total Bombs",
    "Belief Zero",
    "Belief One",
    "Belief Two"
  ),
  
  dep.var.labels = "",
  
  se = list(
    se_overall,
    se_zero,
    se_one,
    se_two
  ),
  
 
  digits = 3,
  align = TRUE,
  no.space = TRUE
)


################################################################################



### Appendix 



data <- readRDS(
  file.path(paths$data_processed, "analysis_data_full.rds")
)


r1 <- filter(data, Round == 1)
r2 <- filter(data, Round == 2)
r3 <- filter(data, Round == 3)
r4 <- filter(data, Round == 4)
r5 <- filter(data, Round == 5)

model_formula1 <- Clicks ~ Condition_SRP +
  Condition_Uncertainty +
  Risk_attitude +
  Female +
  Age +
  Income_1

model_formula <- Clicks ~ Condition_SRP +
  Condition_Uncertainty +
  Risk_attitude +
  Female +
  Age +
  Income_1 +
  Feedback

# Poisson marginal effects (paper specification)

reg1.1 <- poissonmfx(model_formula1, data = r1, atmean = TRUE, robust = TRUE)
reg1.2 <- poissonmfx(model_formula,  data = r2, atmean = TRUE, robust = TRUE)
reg1.3 <- poissonmfx(model_formula,  data = r3, atmean = TRUE, robust = TRUE)
reg1.4 <- poissonmfx(model_formula,  data = r4, atmean = TRUE, robust = TRUE)
reg1.5 <- poissonmfx(model_formula,  data = r5, atmean = TRUE, robust = TRUE)

# ---------------------------------------------------------------------------
# OLS robustness (HC1 SE)
# ---------------------------------------------------------------------------

ols1 <- lm(model_formula1, data = r1)
ols2 <- lm(model_formula,  data = r2)
ols3 <- lm(model_formula,  data = r3)
ols4 <- lm(model_formula,  data = r4)
ols5 <- lm(model_formula,  data = r5)

se1 <- sqrt(diag(vcovHC(ols1, type="HC1")))
se2 <- sqrt(diag(vcovHC(ols2, type="HC1")))
se3 <- sqrt(diag(vcovHC(ols3, type="HC1")))
se4 <- sqrt(diag(vcovHC(ols4, type="HC1")))
se5 <- sqrt(diag(vcovHC(ols5, type="HC1")))

# Print regression table
stargazer(
  ols1, ols2, ols3, ols4, ols5,
  type = "text",
  se = list(se1,se2,se3,se4,se5),
  title = "Round-specific OLS regressions"
)


stargazer(
  reg1.1$fit,
  reg1.2$fit,
  reg1.3$fit,
  reg1.4$fit,
  reg1.5$fit,
  type = "text",
  title = "Round-specific Poisson regressions (Marginal Effects)",
  
  coef = list(
    reg1.1$mfxest[,1],
    reg1.2$mfxest[,1],
    reg1.3$mfxest[,1],
    reg1.4$mfxest[,1],
    reg1.5$mfxest[,1]
  ),
  
  se = list(
    reg1.1$mfxest[,2],
    reg1.2$mfxest[,2],
    reg1.3$mfxest[,2],
    reg1.4$mfxest[,2],
    reg1.5$mfxest[,2]
  ),
  
  column.labels = c("R1","R2","R3","R4","R5"),
  dep.var.labels = "Clicks",
  digits = 3
)


#### Joint analysis 

model_1 <- Clicks ~ Condition_SRP + Condition_Uncertainty
model_2 <- Clicks ~ Condition_SRP:Condition_Uncertainty

# Poisson marginal effects 
pois_all_1 <- poissonmfx(data = data, formula = model_1, atmean = TRUE, robust = TRUE)
pois_all_2 <- poissonmfx(data = data, formula = model_2, atmean = TRUE, robust = TRUE)

# OLS robustness 
ols_all_1 <- lm(model_1, data = data)
ols_all_2 <- lm(model_2, data = data)

se_ols_all_1 <- sqrt(diag(vcovHC(ols_all_1, type = "HC1")))
se_ols_all_2 <- sqrt(diag(vcovHC(ols_all_2, type = "HC1")))

stargazer(

  pois_all_1$fit,
  pois_all_2$fit,
  ols_all_1,
  ols_all_2,
  
  type = "text",   
  title = "Pooled Regression Results (All Rounds)",
  
  dep.var.labels = "Clicks",
  
  column.labels = c(
    "Poisson: Main",
    "Poisson: Interaction",
    "OLS: Main",
    "OLS: Interaction"
  ),
  
  # ---- IMPORTANT PART ----
  coef = list(
    pois_all_1$mfxest[,1],
    pois_all_2$mfxest[,1],
    coef(ols_all_1),
    coef(ols_all_2)
  ),
  
  se = list(
    pois_all_1$mfxest[,2],
    pois_all_2$mfxest[,2],
    se_ols_all_1,
    se_ols_all_2
  ),
  
  digits = 3,
  align = TRUE,
  no.space = TRUE
)



############################################################
### BAYESIAN UPDATING ANALYSIS — AMBIGUITY CONDITION
############################################################

library(dplyr)
library(purrr)
library(lmtest)
library(sandwich)

# ----------------------------------------------------------
# 1. Prepare data
# ----------------------------------------------------------

df_amb <- analysis_data %>%
  filter(Condition_Uncertainty == "Ambiguity") %>%
  arrange(Subject, Round) %>%
  mutate(
    # beliefs as probabilities
    pi0 = Belief_0 / 100,
    pi1 = Belief_1 / 100,
    pi2 = Belief_2 / 100,
    
    # expected bombs
    exp_bombs = (Belief_1 + 2*Belief_2)/100,
    
    # realized outcome
    bomb = as.integer(Feedback == "Feedback_bomb")
  )

# ----------------------------------------------------------
# 2. Likelihood of surviving k clicks
# ----------------------------------------------------------

safe_prob <- function(s, k, N = 100){
  ifelse(k > (N - s), 0,
         choose(N - s, k) / choose(N, k))
}

# vectorized expected bomb probability
expected_bomb_prob <- function(pi0, pi1, pi2, k){
  
  p_safe =
    pi0 * safe_prob(0, k) +
    pi1 * safe_prob(1, k) +
    pi2 * safe_prob(2, k)
  
  1 - p_safe
}

# ----------------------------------------------------------
# 3. Prediction error ("surprise")
# ----------------------------------------------------------

df_amb <- df_amb %>%
  mutate(
    exp_p_bomb =
      expected_bomb_prob(pi0, pi1, pi2, Clicks),
    
    surprise = bomb - exp_p_bomb
  )

# interpretation:
# surprise > 0 → worse than expected
# surprise < 0 → safer than expected

# ----------------------------------------------------------
# 4. Create panel changes (t → t+1)
# ----------------------------------------------------------

panel <- df_amb %>%
  group_by(Subject) %>%
  mutate(
    d_belief = lead(exp_bombs) - exp_bombs,
    d_clicks = lead(Clicks) - Clicks
  ) %>%
  ungroup() %>%
  filter(!is.na(d_belief), !is.na(d_clicks))


behavior_model <- lm(
  d_clicks ~ surprise * Condition_SRP + Clicks + factor(Round),
  data = panel
)

coeftest(
  behavior_model,
  vcov = vcovCL(behavior_model, cluster = ~Subject)
)




panel <- panel %>%
  mutate(
    surprise_type = ifelse(
      surprise > 0,
      "Unexpected bomb",
      "Safer than expected"
    )
  )

model_safer <- lm(
  d_clicks ~ Condition_SRP + Clicks,
  data = subset(panel, surprise_type == "Safer than expected")
)

model_worse <- lm(
  d_clicks ~ Condition_SRP + Clicks,
  data = subset(panel, surprise_type == "Unexpected bomb")
)

summary(model_safer)
summary(model_worse)


message("Analysis finished successfully.")



table_stats <- r1 %>%
  group_by(Condition_Uncertainty, Condition_SRP) %>%
  summarise(
    N = n(),
    Mean = mean(Clicks, na.rm = TRUE),
    SD = sd(Clicks, na.rm = TRUE),
    SE = SD / sqrt(N),
    CI_low  = Mean - qt(0.975, df = N - 1) * SE,
    CI_high = Mean + qt(0.975, df = N - 1) * SE,
    .groups = "drop"
  )
table_stats


lm_all <- lm(
  Clicks ~ Condition_SRP * Condition_Uncertainty,
  data = analysis_data
)

summary(lm_all)
anova(lm_all)

library(emmeans)

pairs(
  emmeans(lm_all, ~ Condition_SRP * Condition_Uncertainty),
  adjust = "tukey"
)
