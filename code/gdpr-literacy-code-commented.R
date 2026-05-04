#Set up

## Download the libraries
library(tidyverse)
library(haven)
library(mediation)
library(marginaleffects)
library(MASS)
library(dplyr)
library(brant)

## Rename the data (imported manually to R available in the Github repo)
eb2019 <- X2019_ZA7562_v2_0_0
eb2015 <- X2015_ZA5964_v2_0_0

# Variable selection for the analysis
df_2019 <- eb2019 %>% 
  dplyr::select (
    isocntry, 
    w1, 
    qb7,
    qb9,
    qb10,
    qb11,
    qb17,
    qb18_1,
    qb18_2,
    qb18_3,
    qb18_4,
    qb18_5,
    qb18_6
  ) %>% mutate(
    
    changed_settings = case_when( #DV
      qb11 == 1 ~ 1,
      qb11 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    perceived_control = 5 - qb9, # 5 is the minimum perceived, 1 is the maximum
    
    control_binary = case_when(
      perceived_control >= 3 ~ 1L,  # some or a lot of control (71.5%)
      perceived_control <= 2 ~ 0L,  # not much or no control   (28.5%)
      TRUE ~ NA_integer_
    ),
    
    gdpr_heard = if_else(qb17 == 1, 1L, 0L),
    across(
      qb18_1:qb18_6,
        ~if_else(. == 1, 1L, 0L),
      .names = "{.col}_r"
    ),
    gdpr_knows_score = qb18_1_r + qb18_2_r + qb18_3_r + qb18_4_r + qb18_5_r + qb18_6_r,
    
    gdpr_knows_any = if_else(gdpr_knows_score > 0, 1L, 0L),
    
    concern_use = qb10,
    concern_track = qb7
    
  ) %>%
  filter(
    !is.na(changed_settings),
    !is.na(perceived_control),
    !is.na(gdpr_heard),
    !is.na(gdpr_knows_score)
  )

#Creating the country-level baseline from eb83.1 (2015)
country_baseline <- eb2015 %>%
  group_by(isocntry) %>%
  summarise(
    baseline_control_2015 = mean(qb4, na.rm = TRUE),
    baaseline_concern_2015 = mean(qb5, na.rm = TRUE),
    
    n_country_2015 = n()
  )

#Check the country coverage (should be all EU member states)
#cat("Countries in 2015 baseline:\n")
#print(nrow(country_baseline))
#print(country_baseline)

#Merge the country baseline into the final dataset
df_2019 <- df_2019 %>%
  left_join(country_baseline, by = "isocntry")

#Verify the merge
cat("\nMissing baseline values after merge:\n")
cat(sum(is.na(df$baseline_control_2015)), "\n")

##Collinearity check between q17 and q18
cat("Correlation check qb17 / qb18: \n")
cat(cor(df_2019$gdpr_heard, df_2019$gdpr_knows_score, use = "complete.obs"), "\n\n")

##Check the distribution score (awareness of GDPR rights qb18_1-qb18_6)
cat("Distribution of knowledge (0-6):\n")
print(table(df_2019$gdpr_knows_score))

#Check knowledge GDPR but not heard of any GDPR rights
cat("/nHeard about DPR but doesn't know rights")
print(table(heard = df_2019$gdpr_heard,
            knows = df_2019$gdpr_knows_any))

# Visualisations (Figure 1)
data.frame(score = 0:6,
           n = c(6642, 2086, 1489, 1033, 680, 397, 721)) %>%
  ggplot(aes(x = score, y = n)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = 0:6) +
  labs(title = "Distribution of GDPR rights knowledge score",
       subtitle = "EB 91.2, 2019 (N ≈ 13,000 with non-missing values)",
       x = "Number of rights known (QB18, 0–6)",
       y = "Number of respondents")

# Brant test (before running ordered logit)
df_2019$perceived_control_ord <- factor(
  df_2019$perceived_control, ordered = TRUE
)

m_brant <- polr(
  perceived_control_ord ~ gdpr_heard + gdpr_knows_score + concern_use + concern_track,
  data = df_2019,
  Hess = TRUE
)

brant(m_brant)
##Ordered logit is not appropriate here, the omnibus test is failed.

# Check QB9 distribution before collapsing
table(df_2019$perceived_control)  # already reversed, so 4=most control

# Also check original QB9
table(df_2019$qb9)

#Model estimation
##M1: Does GDPR awareness predict privacy-protective behavior?
m1_total <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_score +
    concern_use + concern_track + baseline_control_2015,
  data   = df_2019,
  weights = w1,
  family = binomial(link = "logit")
)
summary(m1_total)

##M2: Does GDPR awareness predict perceived control?
m2_mediator <- glm(
  control_binary ~ gdpr_heard + gdpr_knows_score +
    concern_use + concern_track + baseline_control_2015,
  data   = df_2019,
  weights = w1,
  family = binomial(link = "logit")
)
summary(m2_mediator)

##M3: Direct effect of awareness net of perceived control
m3_full <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_score +
    control_binary +
    concern_use + concern_track + baseline_control_2015,
  data   = df_2019,
  weights = w1,
  family = binomial(link = "logit")
)
summary(m3_full)

# Mediation test (first run with sims = 1000 then final run with sims = 5000)
set.seed(42)
med_heard <- mediate(
  model.m  = m2_mediator,
  model.y  = m3_full,
  treat    = "gdpr_heard",
  mediator = "control_binary",
  boot     = TRUE,
  sims     = 5000,
  boot.ci.type = "perc"
)
summary(med_heard)

set.seed(42)
med_knows <- mediate(
  model.m  = m2_mediator,
  model.y  = m3_full,
  treat    = "gdpr_knows_score",
  mediator = "control_binary",
  treat.value   = 6,
  control.value = 0,
  boot     = TRUE,
  sims     = 5000,
  boot.ci.type = "perc"
)
summary(med_knows)

# Check for extreme predicted probabilities in M2
hist(fitted(m2_mediator), 
     main = "Fitted probabilities M2",
     xlab = "P(control_binary = 1)")

#Average marginal effects
cat("\nAMEs: M1 Total Effect\n")
avg_slopes(m1_total)

cat("\nAMEs: M2 Mediator Model\n")
avg_slopes(m2_mediator)

cat("\nAMEs: M3 Full Model\n")
avg_slopes(m3_full)


# Robustness check

##R1: Replace knowledge score with the gdpr_knows_any
m1_robust_binary <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_any +
    concern_use + concern_track + baseline_control_2015,
  data   = df_2019,
  weights = w1,
  family = binomial(link = "logit")
)
summary(m1_robust_binary)

##R2: Swap baseline variable (concern instead of control)
m1_robust_baseline <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_score +
    concern_use + concern_track + baaseline_concern_2015,
  data   = df_2019,
  weights = w1,
  family = binomial(link = "logit")
)
summary(m1_robust_baseline)

##R3: Exclude knowledge score = 6 (sensitivity check)
###Tests whether "expert" cluster drives results
m1_no6 <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_score +
    concern_use + concern_track + baseline_control_2015,
  data   = df_2019 %>% filter(gdpr_knows_score < 6),
  weights = w1,
  family = binomial(link = "logit")
)
summary(m1_no6)

# Compare key coefficients across robustness checks
##gdpr_heard and gdpr_knows coefficients should be stable
cat("\nCoefficient stability across specifications\n")
rbind(
  M1_main    = coef(m1_total)[c("gdpr_heard","gdpr_knows_score")],
  M1_binary  = coef(m1_robust_binary)[c("gdpr_heard","gdpr_knows_any")],
  M1_no6     = coef(m1_no6)[c("gdpr_heard","gdpr_knows_score")]
)