library(tidyverse)
library(ggplot2)
library(marginaleffects)
library(broom)
library(patchwork)
library(haven)

# Color theme
theme_paper <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      axis.text         = element_text(color = "black"),
      axis.title        = element_text(size = 9),
      plot.title        = element_text(size = 10, face = "bold"),
      plot.subtitle     = element_text(size = 8, color = "grey40"),
      legend.position   = "bottom",
      legend.text       = element_text(size = 8),
      strip.text        = element_text(face = "bold", size = 9)
    )
}

# Figure 1 - Coefficient plot: M1, M2, M3

## Tidy all three models
tidy_m1 <- tidy(m1_total, conf.int = TRUE) %>% 
  mutate(model = "M1: Total Effect\n(DV: Changed Settings)")
tidy_m2 <- tidy(m2_mediator, conf.int = TRUE) %>% 
  mutate(model = "M2: Mediator Model\n(DV: Perceived Control)")
tidy_m3 <- tidy(m3_full, conf.int = TRUE) %>% 
  mutate(model = "M3: Full Model\n(DV: Changed Settings)")

# Combine and filter out intercept
coef_all <- bind_rows(tidy_m1, tidy_m2, tidy_m3) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "gdpr_heard"            ~ "GDPR heard (QB17)",
      term == "gdpr_knows_score"      ~ "Knowledge score (QB18)",
      term == "control_binary"        ~ "Perceived control (QB9)",
      term == "concern_use"           ~ "Concern: data use (QB10)",
      term == "concern_track"         ~ "Concern: tracking (QB7)",
      term == "baseline_control_2015" ~ "Baseline control 2015",
      TRUE ~ term
    ),
    # Order variables
    term = factor(term, levels = c(
      "Baseline control 2015",
      "Concern: tracking (QB7)",
      "Concern: data use (QB10)",
      "Perceived control (QB9)",
      "Knowledge score (QB18)",
      "GDPR heard (QB17)"
    )),
    significant = p.value < 0.05
  )

fig1 <- ggplot(coef_all, 
               aes(x = estimate, y = term, 
                   color = significant, shape = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")
  ) +
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")
  ) +
  facet_wrap(~model, ncol = 3) +
  labs(
    title    = "Figure 1: Logistic Regression Coefficients (Log-Odds)",
    subtitle = "95% confidence intervals. Dashed line at zero.",
    x        = "Log-odds coefficient",
    y        = NULL,
    color    = "Significance",
    shape    = "Significance"
  ) +
  theme_paper()

ggsave("fig1_coefficients.pdf", fig1, 
       width = 7, height = 3.5, device = "pdf")

# Figure 2 — AME plot: M1 and M3 compared
ame_m1 <- avg_slopes(m1_total) %>%
  as.data.frame() %>%
  mutate(model = "M1: Total Effect")

ame_m3 <- avg_slopes(m3_full) %>%
  as.data.frame() %>%
  mutate(model = "M3: Direct Effect")

ame_both <- bind_rows(ame_m1, ame_m3) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "gdpr_heard"            ~ "GDPR heard (QB17)",
      term == "gdpr_knows_score"      ~ "Knowledge score (QB18)",
      term == "control_binary"        ~ "Perceived control (QB9)",
      term == "concern_use"           ~ "Concern: data use (QB10)",
      term == "concern_track"         ~ "Concern: tracking (QB7)",
      term == "baseline_control_2015" ~ "Baseline control 2015",
      TRUE ~ term
    ),
    term = factor(term, levels = c(
      "Baseline control 2015",
      "Concern: tracking (QB7)",
      "Concern: data use (QB10)",
      "Perceived control (QB9)",
      "Knowledge score (QB18)",
      "GDPR heard (QB17)"
    ))
  )

fig2 <- ggplot(ame_both,
               aes(x = estimate, y = term,
                   color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.5,
                 position = position_dodge(width = 0.4)) +
  geom_point(size = 2.5,
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(
    "M1: Total Effect"  = "black",
    "M3: Direct Effect" = "grey50"
  )) +
  scale_shape_manual(values = c(
    "M1: Total Effect"  = 16,
    "M3: Direct Effect" = 17
  )) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Figure 2: Average Marginal Effects on P(Changed Settings)",
    subtitle = "M1 (total) vs M3 (direct, controlling for perceived control). 95% CIs.",
    x        = "Change in predicted probability",
    y        = NULL,
    color    = NULL,
    shape    = NULL
  ) +
  theme_paper()

ggsave("fig2_AMEs.pdf", fig2,
       width = 7, height = 3.2, device = "pdf")

# Figure 3 — Mediation decomposition bar chart
mediation_df <- data.frame(
  Treatment = c("GDPR heard (QB17)", "GDPR heard (QB17)",
                "Knowledge score (QB18, 0→6)", 
                "Knowledge score (QB18, 0→6)"),
  Effect    = c("Direct (ADE)", "Indirect via\nperceived control (ACME)",
                "Direct (ADE)", "Indirect via\nperceived control (ACME)"),
  Estimate  = c(0.137, 0.003, 0.303, 0.010),
  Lower     = c(0.116, -0.001, 0.270, 0.005),
  Upper     = c(0.158, 0.006, 0.332, 0.014),
  Significant = c(TRUE, FALSE, TRUE, TRUE)
)

fig3 <- ggplot(mediation_df,
               aes(x = Estimate, y = Effect,
                   color = Significant)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = 0.2, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  facet_wrap(~Treatment, ncol = 1, scales = "free_y") +
  labs(
    title    = "Figure 3: Mediation Decomposition",
    subtitle = "ACME = indirect effect via perceived control. ADE = direct effect.\n5,000 bootstrap simulations. 95% CIs.",
    x        = "Effect on P(Changed Settings)",
    y        = NULL,
    color    = "Significance"
  ) +
  theme_paper()

ggsave("fig3_mediation.pdf", fig3,
       width = 7, height = 3.5, device = "pdf")

# Figure 4 — Predicted probability by knowledge score
pred_df <- predictions(
  m1_total,
  newdata = datagrid(
    gdpr_knows_score      = 0:6,
    gdpr_heard            = c(0, 1),
    concern_use           = mean(df_2019$concern_use, na.rm=TRUE),
    concern_track         = mean(df_2019$concern_track, na.rm=TRUE),
    baseline_control_2015 = mean(df_2019$baseline_control_2015, 
                                 na.rm=TRUE)
  )
) %>%
  as.data.frame() %>%
  mutate(
    `GDPR heard` = if_else(gdpr_heard == 1, 
                           "Heard of GDPR", 
                           "Not heard of GDPR")
  )

fig4 <- ggplot(pred_df,
               aes(x = gdpr_knows_score, y = estimate,
                   color = `GDPR heard`,
                   fill  = `GDPR heard`)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_color_manual(values = c(
    "Heard of GDPR"     = "black",
    "Not heard of GDPR" = "grey55"
  )) +
  scale_fill_manual(values = c(
    "Heard of GDPR"     = "black",
    "Not heard of GDPR" = "grey55"
  )) +
  labs(
    title    = "Figure 4: Predicted Probability of Changing Privacy Settings",
    subtitle = "By rights knowledge score and GDPR exposure. Controls at means. 95% CIs.",
    x        = "Number of GDPR rights known (QB18, 0–6)",
    y        = "P(Changed privacy settings)",
    color    = NULL,
    fill     = NULL
  ) +
  theme_paper()

ggsave("fig4_predicted.pdf", fig4,
       width = 7, height = 3.2, device = "pdf")

library(stargazer)

stargazer(
  m1_total, m2_mediator, m3_full,
  type  = "latex",
  title = "Logistic Regression Results",
  label = "tab:models",
  dep.var.labels   = c("Changed Settings", 
                       "Perceived Control", 
                       "Changed Settings"),
  covariate.labels = c("GDPR heard (QB17)",
                       "Knowledge score (QB18)",
                       "Perceived control (QB9)",
                       "Concern: data use (QB10)",
                       "Concern: tracking (QB7)",
                       "Baseline control (2015)"),
  column.labels = c("M1: Total", 
                    "M2: Mediator", 
                    "M3: Full"),
  single.row    = FALSE,
  star.cutoffs  = c(0.05, 0.01, 0.001),
  notes         = "Post-stratification weights applied. 
                   $^{*}$p$<$0.05; $^{**}$p$<$0.01; 
                   $^{***}$p$<$0.001.",
  notes.append  = FALSE,
  out = "models.tex"
)

lapply(list(m1_total, m2_mediator, m3_full), function(m) {
  cbind(coef = coef(m), se = sqrt(diag(vcov(m))))
})

modelsummary(
  list(
    "M1: Total" = m1_total,
    "M2: Mediator" = m2_mediator,
    "M3: Full" = m3_full
  ),
  output = "models.tex"
)

#Diagnostics figures and tables

library(car)        # for vif()
library(tidyverse)
library(ggplot2)

# 1. MULTICOLLINEARITY — VIF
vif_m1 <- vif(m1_total)
vif_m2 <- vif(m2_mediator)
vif_m3 <- vif(m3_full)

cat("VIF — M1:\n"); print(round(vif_m1, 3))
cat("VIF — M2:\n"); print(round(vif_m2, 3))
cat("VIF — M3:\n"); print(round(vif_m3, 3))

# Export VIF table
vif_df <- data.frame(
  Variable = c("GDPR heard (QB17)", 
               "Knowledge score (QB18)",
               "Concern: data use (QB10)",
               "Concern: tracking (QB7)",
               "Baseline control (2015)"),
  `M1` = round(vif_m1[c("gdpr_heard","gdpr_knows_score",
                        "concern_use","concern_track",
                        "baseline_control_2015")], 3),
  `M2` = round(vif_m2[c("gdpr_heard","gdpr_knows_score",
                        "concern_use","concern_track",
                        "baseline_control_2015")], 3),
  `M3` = round(vif_m3[c("gdpr_heard","gdpr_knows_score",
                        "concern_use","concern_track",
                        "baseline_control_2015")], 3),
  check.names = FALSE
)

vif_df %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Table A6: Variance Inflation Factors (VIF)",
    label     = "tab:vif",
    align     = c("l","r","r","r"),
    digits    = 3
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = "VIF > 10 indicates severe multicollinearity. 
               VIF > 5 warrants concern. All values here 
               are well below 2.0.",
    general_title = "Note:"
  ) %>%
  save_kable("tableA6.tex")

# 2. SEPARATION CHECK
# Cross-tabs of binary predictors against DV
cat("\nSeparation check — QB17 × Changed Settings:\n")
print(table(df_2019$gdpr_heard, df_2019$changed_settings))

cat("\nSeparation check — QB18 any × Changed Settings:\n")
print(table(df_2019$gdpr_knows_any, df_2019$changed_settings))

# Both cells in each row should be non-zero
# If any cell = 0, you have perfect separation

# 3. INFLUENTIAL OBSERVATIONS — Cook's Distance
cooksd_m1 <- cooks.distance(m1_total)

# Figure A1 — Cook's distance plot
cook_df <- data.frame(
  obs    = seq_along(cooksd_m1),
  cooksd = cooksd_m1
)

# Common threshold: 4/n
threshold <- 4 / nrow(df_2019)

figA1 <- ggplot(cook_df, aes(x = obs, y = cooksd)) +
  geom_point(
    aes(color = cooksd > threshold),
    size = 0.8, alpha = 0.6
  ) +
  geom_hline(yintercept = threshold, 
             linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("FALSE" = "grey60", "TRUE" = "black"),
    labels = c("FALSE" = "Normal", 
               "TRUE"  = paste0("Influential (> 4/n = ",
                                round(threshold, 4), ")"))
  ) +
  labs(
    title    = "Figure A1: Cook's Distance — M1",
    subtitle = paste0("Dashed line = 4/n threshold (", 
                      round(threshold, 4), "). N = ", 
                      nrow(df_2019)),
    x        = "Observation index",
    y        = "Cook's distance",
    color    = NULL
  ) +
  theme_paper()

ggsave("figA1_cooks.pdf", figA1, 
       width = 7, height = 3, device = "pdf")

# Count influential observations
n_influential <- sum(cooksd_m1 > threshold)
cat("\nInfluential observations (Cook's D > 4/n):", 
    n_influential, "\n")
cat("As % of sample:", 
    round(n_influential/nrow(df_2019)*100, 1), "%\n")

# Sensitivity check: rerun M1 excluding influential obs
df_no_influential <- df_2019[cooksd_m1 <= threshold, ]

m1_no_influential <- glm(
  changed_settings ~ gdpr_heard + gdpr_knows_score +
    concern_use + concern_track + baseline_control_2015,
  data   = df_no_influential,
  weights = w1,
  family = binomial(link = "logit")
)

cat("\nCoefficient comparison — with vs without influential obs:\n")
round(cbind(
  Full    = coef(m1_total),
  No_Inf  = coef(m1_no_influential)
), 4)

#Model for — pseudo-R2
# Null log-likelihood
loglik_null_m1 <- logLik(
  glm(changed_settings ~ 1, data = df_2019, 
      weights = w1, family = binomial)
)
loglik_null_m2 <- logLik(
  glm(control_binary ~ 1, data = df_2019,
      weights = w1, family = binomial)
)

loglik_m1 <- logLik(m1_total)
loglik_m2 <- logLik(m2_mediator)
loglik_m3 <- logLik(m3_full)

mcfadden <- function(model, null_loglik) {
  1 - (as.numeric(logLik(model)) / as.numeric(null_loglik))
}

cat("\nMcFadden pseudo-R2:\n")
cat("M1:", round(mcfadden(m1_total, loglik_null_m1), 4), "\n")
cat("M2:", round(mcfadden(m2_mediator, loglik_null_m2), 4), "\n")
cat("M3:", round(mcfadden(m3_full, loglik_null_m1), 4), "\n")

# AIC comparison
cat("\nAIC:\n")
cat("M1:", AIC(m1_total), "\n")
cat("M2:", AIC(m2_mediator), "\n")
cat("M3:", AIC(m3_full), "\n")

# 5. HETEROSCEDASTICITY NOTE
cat("\nDispersion check M1 (should be ~1.0 for binomial):\n")
cat(sum(residuals(m1_total, type = "pearson")^2) / 
      m1_total$df.residual, "\n")