# =============================================================================
# VWAZA MARSH WILDLIFE RESERVE — CO-MANAGEMENT ANALYSIS
# Lameck Nyondo MSc Thesis | Research Path Malawi
# =============================================================================
# Design: Exploratory Sequential Mixed Methods
# Quantitative role: directional/exploratory support only (N = 41)
# Primary analysis: Spearman correlations (ordinal Likert data)
# Secondary check: OLS regression with full assumption diagnostics
#   → keep only if assumptions hold; otherwise report Spearman only
# =============================================================================

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(psych)         # alpha(), describe()
library(ggplot2)
library(ggcorrplot)    # correlation heatmap
library(lmtest)        # bptest() — Breusch-Pagan
library(car)           # vif(), qqPlot()
library(olsrr)         # ols_plot_cooksd_bar(), ols_vif_tol()
library(broom)         # tidy(), glance(), augment()
library(gtsummary)     # tbl_summary() — full frequency tables, all levels
library(gt)            # gt tables → .docx via gtsave()
# library(flextable) — replaced by gt/gtsummary throughout
# library(officer)   — replaced by gt/gtsummary throughout
library(glue)          # glue() for inline string interpolation
library(patchwork)     # combine ggplots

# ── Output directories ──────────────────────────────────────────────────────
dir.create(here("tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("figures"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. IMPORT & RECODE
# =============================================================================

raw <- read_excel(
  here("data", "lameck.xlsx"),
  sheet    = "data",
  skip     = 1,          # row 1 is a duplicate header row
  col_names = TRUE
)

# Short, clean variable names
vwaza <- raw |>
  clean_names() |>
  rename(
    age             = age_of_respondent,
    education       = level_of_education,
    gender          = gender_of_respondent,
    role            = role_of_respondent_in_co_management,
    part_freq       = frequency_of_participation_in_co_management,
    part_type       = type_of_participation_in_co_management,
    dec_making      = participation_in_decision_making,
    incentives      = incentives_recieved_during_participation,
    # Empowerment items (q9a–q9e)
    emp_informed    = our_community_is_consistently_informed_about_management_decisions_before_they_are_implemented,
    emp_consulted   = our_community_is_consulted_for_our_opinion_and_our_feedback_is_seriously_considered_in_the_decision_making_process,
    emp_agreements  = management_actions_are_based_on_co_management_formal_agreements_that_have_been_mutually_developed_between_our_community_and_the_managing_authorities,
    emp_authority   = our_community_shares_formal_authority_and_responsibility_in_co_management_structures_e_g_planning_rule_setting_budgeting,
    emp_full_auth   = our_community_has_the_full_authority_and_responsibility_to_make_and_implement_final_decisions_on_co_management_issues,
    # Performance composites
    perf_ecological = perceived_impact_of_participation_and_empowerment_on_ecological_outcomes,
    perf_economic   = perceived_impact_of_participation_and_empowerment_on_economic_outcomes,
    perf_social     = perceived_impact_of_participation_and_empowerment_on_social_outcomes,
    # Performance items (q13a–q13d)
    perf_trust      = the_co_management_process_has_improved_trust_and_reduced_conflict_between_the_community_and_managing_authorities,
    perf_compliance = the_current_level_of_community_involvement_increases_the_long_term_acceptance_and_compliance_with_management_rules,
    perf_fairness   = the_benefits_economic_social_ecological_from_the_managed_resource_are_distributed_fairly_as_a_result_of_this_co_management_arrangement,
    perf_overall    = overall_the_co_management_arrangement_has_been_successful_in_achieving_its_stated_goals
  ) |>
  select(-starts_with("unnamed")) |>   # drop the stray ID column
  mutate(id = row_number())

# ── Ordinal encoding ─────────────────────────────────────────────────────────
# 5-point Likert: Strongly Disagree=1 … Strongly Agree=5
agree_levels <- c(
  "Strongly Disagree" = 1, "Disagree" = 2, "Neutral" = 3,
  "Agree" = 4, "Strongly Agree" = 5
)

# 5-point impact: Very negative=1 … Very positive=5
impact_levels <- c(
  "Very negative" = 1, "Negative" = 2, "Neutral" = 3,
  "Positive" = 4, "Very positive" = 5
)

# Participation frequency: Never=1 … Always=5
freq_levels <- c(
  "Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Always" = 5
)

# Participation type (Arnstein/Pretty ladder): Passive=1 … Self-mobilisation=5
type_levels <- c(
  "Passive (just informed,little actual involvement)"              = 1,
  "Consultative (giving input when asked)"                         = 2,
  "Functional (taking part in specific activities)"                = 3,
  "Interactive (actively involved in planning and decision making)"= 4,
  "Self-mobilization (taking initiative independently of the authorities)" = 5
)

# Decision making: No=0, Yes=1
# Incentives: None=0, any type=1 (binary presence)

vwaza <- vwaza |>
  mutate(
    # Empowerment items → numeric
    across(c(emp_informed, emp_consulted, emp_agreements,
             emp_authority, emp_full_auth),
           ~ agree_levels[.x]),
    # Performance composite perceptions → numeric
    across(c(perf_ecological, perf_economic, perf_social),
           ~ impact_levels[.x]),
    # Performance Likert items → numeric
    across(c(perf_trust, perf_compliance, perf_fairness, perf_overall),
           ~ agree_levels[.x]),
    # Participation
    part_freq_n  = freq_levels[part_freq],
    part_type_n  = type_levels[part_type],
    dec_making_n = if_else(dec_making == "Yes", 1L, 0L),
    incentive_n  = if_else(is.na(incentives), 0L, 1L),
    # Demographics → factor
    age       = factor(age,       levels = c("18-35 years","36-54 years","55 years and above")),
    education = factor(education, levels = c("No formal education","Primary","Secondary","Tertiary")),
    gender    = factor(gender),
    role      = factor(role)
  )

# ── Composite scores (mean of items) ─────────────────────────────────────────
vwaza <- vwaza |>
  rowwise() |>
  mutate(
    empowerment_score = mean(c(emp_informed, emp_consulted, emp_agreements,
                               emp_authority, emp_full_auth), na.rm = TRUE),
    performance_score = mean(c(perf_ecological, perf_economic, perf_social,
                               perf_trust, perf_compliance, perf_fairness,
                               perf_overall), na.rm = TRUE)
  ) |>
  ungroup()

cat("✔  Data loaded and recoded. N =", nrow(vwaza), "\n")
cat("   Empowerment score range:", range(vwaza$empowerment_score, na.rm=TRUE), "\n")
cat("   Performance score range:", range(vwaza$performance_score, na.rm=TRUE), "\n\n")

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================

cat("── DESCRIPTIVE STATISTICS ──────────────────────────────────────────────\n")

# ── Table 1: Socio-demographic characteristics (all levels, no selection) ────
# gtsummary::tbl_summary() enumerates every level of every factor automatically.
# No hand-picking of categories — the full distribution is shown.

demo_gt <- vwaza |>
  select(
    Age            = age,
    Education      = education,
    Gender         = gender,
    Role           = role,
    `Participation frequency` = part_freq,
    `Participation type`      = part_type,
    `Decision-making role`    = dec_making,
    Incentives                = incentives
  ) |>
  tbl_summary(
    type        = everything() ~ "categorical",
    statistic   = all_categorical() ~ "{n} ({p}%)",
    digits      = all_categorical() ~ c(0, 1),
    missing     = "no"
  ) |>
  modify_header(label ~ "**Variable**", stat_0 ~ "**n (%)**") |>
  modify_caption("**Table 1. Socio-demographic and participation characteristics (N = 41)**") |>
  bold_labels()

# Print to console
print(demo_gt)

# Save as Word
demo_gt |>
  as_gt() |>
  gtsave(here("tables", "table1_demographics.docx"))

cat("\nTable 1 saved → tables/table1_demographics.docx\n\n")

# ── Table 2: Empowerment scale item means ────────────────────────────────────
emp_items_df <- tibble(
  Item = c(
    "Informed about decisions",
    "Consulted, feedback considered",
    "Jointly developed agreements",
    "Shared formal authority",
    "Full decision-making authority"
  ),
  Mean = c(
    mean(vwaza$emp_informed,   na.rm = TRUE),
    mean(vwaza$emp_consulted,  na.rm = TRUE),
    mean(vwaza$emp_agreements, na.rm = TRUE),
    mean(vwaza$emp_authority,  na.rm = TRUE),
    mean(vwaza$emp_full_auth,  na.rm = TRUE)
  ),
  SD = c(
    sd(vwaza$emp_informed,   na.rm = TRUE),
    sd(vwaza$emp_consulted,  na.rm = TRUE),
    sd(vwaza$emp_agreements, na.rm = TRUE),
    sd(vwaza$emp_authority,  na.rm = TRUE),
    sd(vwaza$emp_full_auth,  na.rm = TRUE)
  )
) |>
  mutate(across(c(Mean, SD), \(x) round(x, 2)))

emp_gt <- emp_items_df |>
  gt() |>
  tab_header(
    title    = "Table 2. Empowerment scale item descriptives",
    subtitle = "N = 41; scale: 1 = Strongly Disagree, 5 = Strongly Agree"
  ) |>
  cols_label(Item = "Item", Mean = "Mean", SD = "SD") |>
  fmt_number(columns = c(Mean, SD), decimals = 2) |>
  tab_source_note("Items scored 1–5 (Likert). Composite mean used in analysis.")

print(emp_gt)

gtsave(emp_gt, here("tables", "table2_empowerment_items.docx"))
cat("Table 2 saved → tables/table2_empowerment_items.docx\n\n")

# ── Table 3: Performance scale item means ────────────────────────────────────
perf_items_df <- tibble(
  Item = c(
    "Ecological impact",
    "Economic impact",
    "Social impact",
    "Trust / conflict management",
    "Compliance with rules",
    "Benefit fairness",
    "Overall perceived success"
  ),
  Mean = c(
    mean(vwaza$perf_ecological, na.rm = TRUE),
    mean(vwaza$perf_economic,   na.rm = TRUE),
    mean(vwaza$perf_social,     na.rm = TRUE),
    mean(vwaza$perf_trust,      na.rm = TRUE),
    mean(vwaza$perf_compliance, na.rm = TRUE),
    mean(vwaza$perf_fairness,   na.rm = TRUE),
    mean(vwaza$perf_overall,    na.rm = TRUE)
  ),
  SD = c(
    sd(vwaza$perf_ecological, na.rm = TRUE),
    sd(vwaza$perf_economic,   na.rm = TRUE),
    sd(vwaza$perf_social,     na.rm = TRUE),
    sd(vwaza$perf_trust,      na.rm = TRUE),
    sd(vwaza$perf_compliance, na.rm = TRUE),
    sd(vwaza$perf_fairness,   na.rm = TRUE),
    sd(vwaza$perf_overall,    na.rm = TRUE)
  )
) |>
  mutate(across(c(Mean, SD), \(x) round(x, 2)))

perf_gt <- perf_items_df |>
  gt() |>
  tab_header(
    title    = "Table 3. Performance scale item descriptives",
    subtitle = "N = 41; ecological/economic/social: 1 = Very negative, 5 = Very positive; others: 1–5 Likert"
  ) |>
  cols_label(Item = "Item", Mean = "Mean", SD = "SD") |>
  fmt_number(columns = c(Mean, SD), decimals = 2) |>
  tab_source_note("Items scored 1–5. Composite mean used in analysis.")

print(perf_gt)

gtsave(perf_gt, here("tables", "table3_performance_items.docx"))
cat("Table 3 saved → tables/table3_performance_items.docx\n\n")

# =============================================================================
# 3. RELIABILITY (CRONBACH'S ALPHA)
# =============================================================================

cat("\n── RELIABILITY ─────────────────────────────────────────────────────────\n")

emp_matrix  <- vwaza |> select(emp_informed, emp_consulted, emp_agreements, emp_authority, emp_full_auth)
perf_matrix <- vwaza |> select(perf_ecological, perf_economic, perf_social, perf_trust, perf_compliance, perf_fairness, perf_overall)

alpha_emp  <- psych::alpha(emp_matrix)
alpha_perf <- psych::alpha(perf_matrix)

cat(sprintf("Empowerment scale  — Cronbach's α = %.3f\n", alpha_emp$total$raw_alpha))
cat(sprintf("Performance scale  — Cronbach's α = %.3f\n", alpha_perf$total$raw_alpha))
cat("(α ≥ 0.70 = acceptable; α ≥ 0.60 acceptable for exploratory scale)\n\n")

# =============================================================================
# 4. NORMALITY CHECKS ON COMPOSITE SCORES
# =============================================================================

cat("── NORMALITY CHECKS ────────────────────────────────────────────────────\n")

sw_emp  <- shapiro.test(vwaza$empowerment_score)
sw_perf <- shapiro.test(vwaza$performance_score)

cat(sprintf("Shapiro-Wilk — Empowerment:  W = %.4f, p = %.4f\n", sw_emp$statistic,  sw_emp$p.value))
cat(sprintf("Shapiro-Wilk — Performance:  W = %.4f, p = %.4f\n", sw_perf$statistic, sw_perf$p.value))
cat("H0: data are normally distributed. p < .05 → reject normality.\n\n")

# QQ plots
p_qq_emp <- ggplot(vwaza, aes(sample = empowerment_score)) +
  stat_qq(colour = "#2C7BB6", size = 2) +
  stat_qq_line(colour = "#D7191C", linewidth = 0.8) +
  labs(title = "QQ Plot — Empowerment Score",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_classic(base_size = 12)

p_qq_perf <- ggplot(vwaza, aes(sample = performance_score)) +
  stat_qq(colour = "#2C7BB6", size = 2) +
  stat_qq_line(colour = "#D7191C", linewidth = 0.8) +
  labs(title = "QQ Plot — Performance Score",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_classic(base_size = 12)

ggsave(here("figures", "qq_plots.png"),
       plot = p_qq_emp + p_qq_perf,
       width = 10, height = 5, dpi = 300)
cat("✔  QQ plots saved → figures/qq_plots.png\n\n")

# =============================================================================
# 5. SPEARMAN CORRELATIONS (PRIMARY — appropriate for ordinal data)
# =============================================================================

cat("── SPEARMAN CORRELATIONS ───────────────────────────────────────────────\n")

# Key bivariate correlations
# Note: incentive_n excluded — constant (all 41 respondents had incentives),
# zero variance prevents meaningful correlation or regression use.
cor_vars <- vwaza |>
  select(empowerment_score, part_freq_n, part_type_n,
         dec_making_n, performance_score)

spearman_mat <- cor(cor_vars, method = "spearman", use = "complete.obs")

print(round(spearman_mat, 3))

# Statistical significance for empowerment ↔ performance (primary relationship)
cor_test_main <- cor.test(vwaza$empowerment_score, vwaza$performance_score,
                          method = "spearman", exact = FALSE)
cat(sprintf(
  "\nSpearman ρ (Empowerment × Performance): ρ = %.3f, S = %.1f, p = %.4f\n",
  cor_test_main$estimate, cor_test_main$statistic, cor_test_main$p.value
))

# All pairwise with p-values
cor_with_p <- function(x, y, xname, yname) {
  ct <- cor.test(x, y, method = "spearman", exact = FALSE)
  tibble(
    Variable1 = xname, Variable2 = yname,
    rho = round(ct$estimate, 3),
    p   = round(ct$p.value, 4),
    sig = case_when(ct$p.value < .001 ~ "***",
                    ct$p.value < .01  ~ "**",
                    ct$p.value < .05  ~ "*",
                    ct$p.value < .10  ~ "†",
                    TRUE              ~ "")
  )
}

cor_table <- bind_rows(
  cor_with_p(vwaza$empowerment_score, vwaza$performance_score,
             "Empowerment",         "Performance"),
  cor_with_p(vwaza$part_freq_n,      vwaza$performance_score,
             "Participation Freq",  "Performance"),
  cor_with_p(vwaza$part_type_n,      vwaza$performance_score,
             "Participation Type",  "Performance"),
  cor_with_p(vwaza$dec_making_n,     vwaza$performance_score,
             "Decision Making",     "Performance"),
  # incentive_n omitted — constant predictor, all 41 respondents had incentives
  cor_with_p(vwaza$empowerment_score, vwaza$part_freq_n,
             "Empowerment",         "Participation Freq"),
  cor_with_p(vwaza$empowerment_score, vwaza$dec_making_n,
             "Empowerment",         "Decision Making")
)

# ── Table 4: Spearman correlation table (gt → .docx) ─────────────────────────
cor_gt <- cor_table |>
  gt() |>
  tab_header(
    title    = "Table 4. Spearman rank-order correlations",
    subtitle = "N = 41; † p < .10, * p < .05, ** p < .01, *** p < .001"
  ) |>
  cols_label(
    Variable1 = "Variable 1",
    Variable2 = "Variable 2",
    rho       = md("*ρ*"),
    p         = md("*p*-value"),
    sig       = ""
  ) |>
  fmt_number(columns = c(rho, p), decimals = 3) |>
  tab_source_note("Spearman's ρ; two-tailed; exact = FALSE (ties present).")

print(cor_gt)

gtsave(cor_gt, here("tables", "table4_spearman_correlations.docx"))
cat("\nTable 4 saved → tables/table4_spearman_correlations.docx\n\n")


# =============================================================================
# 6. OLS REGRESSION — ASSUMPTION DIAGNOSTICS
#    (Run it; decide whether to report based on assumption results)
# =============================================================================

cat("── OLS REGRESSION ASSUMPTION DIAGNOSTICS ───────────────────────────────\n")
cat("NOTE: OLS is secondary/exploratory given N=41 and ordinal predictors.\n")
cat("      All assumptions must hold before results are reported.\n\n")

# incentive_n excluded from model — constant predictor (all respondents had
# incentives), causing aliased coefficients and VIF crash.
model <- lm(performance_score ~ empowerment_score + dec_making_n,
            data = vwaza)

# ── Table 5: OLS regression coefficients (gt → .docx) ────────────────────────
reg_gt <- tidy(model, conf.int = TRUE) |>
  mutate(
    term = case_match(term,
                      "(Intercept)"       ~ "Intercept",
                      "empowerment_score" ~ "Empowerment score",
                      "dec_making_n"      ~ "Decision-making role (yes = 1)",
                      .default = term
    ),
    sig = case_when(
      p.value < .001 ~ "***",
      p.value < .01  ~ "**",
      p.value < .05  ~ "*",
      p.value < .10  ~ "†",
      TRUE           ~ ""
    )
  ) |>
  select(Term = term, B = estimate, SE = std.error,
         `95% CI low` = conf.low, `95% CI high` = conf.high,
         t = statistic, p = p.value, sig) |>
  gt() |>
  tab_header(
    title    = "Table 5. OLS regression — performance score on empowerment predictors",
    subtitle = glue::glue(
      "R² = {round(glance(model)$r.squared, 3)}, ",
      "Adj. R² = {round(glance(model)$adj.r.squared, 3)}, ",
      "F({glance(model)$df}, {glance(model)$df.residual}) = ",
      "{round(glance(model)$statistic, 2)}, p = {round(glance(model)$p.value, 4)}"
    )
  ) |>
  fmt_number(columns = c(B, SE, `95% CI low`, `95% CI high`, t, p), decimals = 3) |>
  cols_label(sig = "") |>
  tab_source_note("† p<.10  * p<.05  ** p<.01  *** p<.001") |>
  tab_source_note("NOTE: Report only if all assumption checks pass (see diagnostics below).")



print(reg_gt)
gtsave(reg_gt, here("tables", "table5_regression.docx"))
cat("Table 5 saved → tables/table5_regression.docx\n\n")


# ── 6a. Normality of residuals ─────────────────────────────────────────────
resid_sw <- shapiro.test(residuals(model))
cat(sprintf("\nShapiro-Wilk on residuals: W = %.4f, p = %.4f\n",
            resid_sw$statistic, resid_sw$p.value))
if (resid_sw$p.value < .05) {
  cat("⚠  Residuals are NOT normally distributed (p < .05).\n")
} else {
  cat("✔  Residuals appear normally distributed (p ≥ .05).\n")
}

# ── 6b. Homoscedasticity (Breusch-Pagan) ───────────────────────────────────
bp_test <- bptest(model)
cat(sprintf("\nBreusch-Pagan test: BP = %.4f, df = %d, p = %.4f\n",
            bp_test$statistic, bp_test$parameter, bp_test$p.value))
if (bp_test$p.value < .05) {
  cat("⚠  Heteroscedasticity detected (p < .05). Assumption violated.\n")
} else {
  cat("✔  No significant heteroscedasticity (p ≥ .05).\n")
}

# ── 6c. Multicollinearity (VIF) ────────────────────────────────────────────
# car::vif() crashes on aliased (dropped) coefficients; tryCatch handles this.
vif_ok <- tryCatch({
  vif_vals <- car::vif(model)
  cat("\nVariance Inflation Factors:\n")
  print(round(vif_vals, 3))
  if (any(vif_vals > 5)) {
    cat("⚠  VIF > 5 detected. Multicollinearity concern.\n")
    FALSE
  } else {
    cat("✔  All VIFs ≤ 5. No serious multicollinearity.\n")
    TRUE
  }
}, error = function(e) {
  cat("\nVIF check skipped — one or more predictors were dropped from the model\n")
  cat("(likely due to zero variance / perfect singularity in the data).\n")
  cat("Check: which predictors are constant or perfectly collinear?\n")
  cat(sprintf("  → Dropped predictor(s): %s\n",
              paste(names(which(is.na(coef(model)))), collapse = ", ")))
  TRUE   # treat as passing since the dropped variable cannot cause collinearity
})

# ── 6d. Influential cases — Cook's Distance ────────────────────────────────
augmented  <- augment(model)
augmented  <- augmented |> mutate(.row_id = seq_len(n()))
cooks_cut  <- 4 / nrow(vwaza)   # conservative threshold: 4/n
influential <- augmented |> filter(.cooksd > cooks_cut)

cat(sprintf("\nCook's distance threshold (4/n): %.4f\n", cooks_cut))
cat(sprintf("Cases exceeding threshold: %d\n", nrow(influential)))
if (nrow(influential) > 0) {
  cat("Row indices of influential cases:\n")
  print(influential |> select(.row_id, .cooksd, .hat, .std.resid))
}


# ── 6e. Decision: report regression or not? ────────────────────────────────
cat("── ASSUMPTION SUMMARY & REPORTING DECISION ─────────────────────────────\n")
resid_ok  <- resid_sw$p.value >= .05
hetero_ok <- bp_test$p.value  >= .05
vif_ok    <- all(vif_vals <= 5)
infl_ok   <- nrow(influential) <= 3   # allow up to 3 borderline cases for N=41

cat(sprintf("  Normality of residuals:  %s\n", ifelse(resid_ok,  "✔ PASS", "✘ FAIL")))
cat(sprintf("  Homoscedasticity:        %s\n", ifelse(hetero_ok, "✔ PASS", "✘ FAIL")))
cat(sprintf("  Multicollinearity (VIF): %s\n", ifelse(vif_ok,   "✔ PASS", "✘ FAIL")))
cat(sprintf("  Influential cases:       %s (%d cases > 4/n)\n",
            ifelse(infl_ok, "✔ PASS", "✘ CONCERN"), nrow(influential)))

all_pass <- resid_ok & hetero_ok & vif_ok

if (all_pass) {
  cat("\n✔  All key assumptions met. Report regression results (with exploratory framing).\n")
  cat("   Use: R² = ", round(summary(model)$r.squared, 3),
      ", F(3,37) = ", round(summary(model)$fstatistic[1], 3),
      ", p < .001\n")
  tidy_model <- tidy(model) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  print(tidy_model)
} else {
  cat("\n⚠  One or more assumptions violated.\n")
  cat("   RECOMMENDATION: Do NOT report OLS regression results.\n")
  cat("   Report Spearman correlations only as the quantitative finding.\n")
  cat("   This is more defensible for N=41 ordinal Likert data regardless.\n")
}


# =============================================================================
# 8. SESSION INFO
# =============================================================================
cat("── SESSION INFO ────────────────────────────────────────────────────────\n")
sessionInfo()
