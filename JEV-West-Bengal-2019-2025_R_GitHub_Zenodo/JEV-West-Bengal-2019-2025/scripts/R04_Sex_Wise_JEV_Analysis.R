#===============================================================================

# SEX-WISE ANALYSIS OF JEV POSITIVITY
# Five districts of North Bengal, India
#
# Analyses:
#   1. Overall sex-wise JEV positivity
#   2. Pearson chi-square test
#   3. Fisher's exact test
#   4. Risk Ratio (RR) with 95% CI
#   5. District-specific positivity
#   6. District-specific RR and OR with 95% CI
#   7. District-specific chi-square/Fisher tests
#   8. Binomial logistic regression
#   9. Likelihood-ratio test for Sex
#  10. Cochran-Mantel-Haenszel analysis
#  11. Sex x District interaction
#  12. Likelihood-ratio test for interaction
#  13. Publication-quality figures
#  14. Export results
#
# Reference categories:
#   Sex      = Female
#   District = DRJ
#
# IMPORTANT:
# All counts are from the final supplied dataset.
#
# FIX NOTE (this version):
#   Section 25 (RR forest plot) previously used a hard-coded linear axis
#   scale_y_continuous(limits = c(0, 3)). Because APD's 95% CI upper bound
#   (~3.98) exceeds 3, ggplot's limits= argument silently DROPPED that data
#   point/whisker instead of just zooming the view. Fixed by:
#     (a) computing axis limits dynamically from the data, and
#     (b) plotting RR on a log10 scale, which is the statistically correct
#         scale for ratio measures (RR/OR) since it makes reciprocal effects
#         (e.g. RR = 0.5 vs RR = 2) visually symmetric around the null (1).
#===============================================================================


#===============================================================================
# 1. PACKAGES
#===============================================================================

library(ggplot2)
library(scales)


#===============================================================================
# 2. REPRODUCIBILITY
#===============================================================================

set.seed(12345)


#===============================================================================
# 3. FINAL VERIFIED DATA
#===============================================================================

data_raw <- data.frame(
  
  District = rep(
    c("DRJ", "JAL", "UDJ", "APD", "CRB"),
    each = 2
  ),
  
  Sex = rep(
    c("Male", "Female"),
    times = 5
  ),
  
  Negative = c(
    318, 281,     # DRJ
    364, 265,     # JAL
    134, 113,     # UDJ
    100, 87,      # APD
    70, 61        # CRB
  ),
  
  Positive = c(
    50, 35,       # DRJ
    45, 26,       # JAL
    16, 15,       # UDJ
    22, 9,        # APD
    21, 12        # CRB
  )
)


#===============================================================================
# 4. DATA PREPARATION
#===============================================================================

data_raw$Total <-
  data_raw$Negative +
  data_raw$Positive

data_raw$Positivity <-
  data_raw$Positive /
  data_raw$Total * 100

data_raw$Sex <- factor(
  data_raw$Sex,
  levels = c("Female", "Male")
)

data_raw$District <- factor(
  data_raw$District,
  levels = c(
    "DRJ",
    "JAL",
    "UDJ",
    "APD",
    "CRB"
  )
)


#===============================================================================
# 5. DISPLAY DATA
#===============================================================================

cat("\n============================================================\n")
cat("FINAL SEX-WISE DATA\n")
cat("============================================================\n")

print(data_raw)


#===============================================================================
# 6. OVERALL SEX-WISE SUMMARY
#===============================================================================

overall <- aggregate(
  cbind(
    Negative,
    Positive
  ) ~ Sex,
  data = data_raw,
  FUN = sum
)

overall$Total <-
  overall$Negative +
  overall$Positive

overall$Positivity <-
  overall$Positive /
  overall$Total * 100


cat("\n============================================================\n")
cat("OVERALL SEX-WISE JEV POSITIVITY\n")
cat("============================================================\n")

print(overall)


#===============================================================================
# 7. EXACT OVERALL VALUES
#===============================================================================

male_pos <- overall$Positive[
  overall$Sex == "Male"
]

male_total <- overall$Total[
  overall$Sex == "Male"
]

female_pos <- overall$Positive[
  overall$Sex == "Female"
]

female_total <- overall$Total[
  overall$Sex == "Female"
]

male_risk <-
  male_pos / male_total

female_risk <-
  female_pos / female_total


cat("\nMale positivity = ",
    sprintf("%.2f", male_risk * 100),
    "% (",
    male_pos,
    "/",
    male_total,
    ")\n",
    sep = "")

cat("Female positivity = ",
    sprintf("%.2f", female_risk * 100),
    "% (",
    female_pos,
    "/",
    female_total,
    ")\n",
    sep = "")


#===============================================================================
# 8. OVERALL RISK RATIO
#===============================================================================

overall_RR <-
  male_risk / female_risk

SE_log_RR <- sqrt(
  (1 / male_pos) -
    (1 / male_total) +
    (1 / female_pos) -
    (1 / female_total)
)

RR_lower <-
  exp(
    log(overall_RR) -
      1.96 * SE_log_RR
  )

RR_upper <-
  exp(
    log(overall_RR) +
      1.96 * SE_log_RR
  )


cat("\n============================================================\n")
cat("OVERALL RISK RATIO\n")
cat("============================================================\n")

cat(
  "RR (Male vs Female) = ",
  round(overall_RR, 3),
  "\n",
  sep = ""
)

cat(
  "95% CI = ",
  round(RR_lower, 3),
  "-",
  round(RR_upper, 3),
  "\n",
  sep = ""
)


#===============================================================================
# 9. OVERALL CONTINGENCY TABLE
#===============================================================================

overall_table <- matrix(
  c(
    male_pos,
    male_total - male_pos,
    female_pos,
    female_total - female_pos
  ),
  nrow = 2,
  byrow = TRUE
)

rownames(overall_table) <- c(
  "Male",
  "Female"
)

colnames(overall_table) <- c(
  "Positive",
  "Negative"
)


cat("\n============================================================\n")
cat("OVERALL CONTINGENCY TABLE\n")
cat("============================================================\n")

print(overall_table)


#===============================================================================
# 10. PEARSON CHI-SQUARE TEST
#===============================================================================

chi_overall <- chisq.test(
  overall_table,
  correct = FALSE
)

cat("\n============================================================\n")
cat("OVERALL PEARSON CHI-SQUARE TEST\n")
cat("============================================================\n")

print(chi_overall)


#===============================================================================
# 11. FISHER'S EXACT TEST
#===============================================================================

fisher_overall <- fisher.test(
  overall_table
)

cat("\n============================================================\n")
cat("OVERALL FISHER'S EXACT TEST\n")
cat("============================================================\n")

print(fisher_overall)


#===============================================================================
# 12. DISTRICT-SPECIFIC ANALYSIS
#===============================================================================

district_results <- data.frame()

for (dist in levels(data_raw$District)) {
  
  sub <- data_raw[
    data_raw$District == dist,
  ]
  
  male <- sub[
    sub$Sex == "Male",
  ]
  
  female <- sub[
    sub$Sex == "Female",
  ]
  
  
  #---------------------------------------------------------------------------
  # COUNTS
  #---------------------------------------------------------------------------
  
  a <- male$Positive
  b <- male$Negative
  
  c <- female$Positive
  d <- female$Negative
  
  
  #---------------------------------------------------------------------------
  # POSITIVITY
  #---------------------------------------------------------------------------
  
  male_risk_d <-
    a / (a + b)
  
  female_risk_d <-
    c / (c + d)
  
  
  male_pct_d <-
    male_risk_d * 100
  
  female_pct_d <-
    female_risk_d * 100
  
  
  #---------------------------------------------------------------------------
  # RISK RATIO
  #---------------------------------------------------------------------------
  
  RR <- male_risk_d /
    female_risk_d
  
  SE_RR <- sqrt(
    (1 / a) -
      (1 / (a + b)) +
      (1 / c) -
      (1 / (c + d))
  )
  
  RR_Lower <- exp(
    log(RR) -
      1.96 * SE_RR
  )
  
  RR_Upper <- exp(
    log(RR) +
      1.96 * SE_RR
  )
  
  
  #---------------------------------------------------------------------------
  # ODDS RATIO
  #---------------------------------------------------------------------------
  
  OR <- (
    a * d
  ) / (
    b * c
  )
  
  SE_OR <- sqrt(
    1 / a +
      1 / b +
      1 / c +
      1 / d
  )
  
  OR_Lower <- exp(
    log(OR) -
      1.96 * SE_OR
  )
  
  OR_Upper <- exp(
    log(OR) +
      1.96 * SE_OR
  )
  
  
  #---------------------------------------------------------------------------
  # CONTINGENCY TABLE
  #---------------------------------------------------------------------------
  
  tab <- matrix(
    c(
      a, b,
      c, d
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  rownames(tab) <- c(
    "Male",
    "Female"
  )
  
  colnames(tab) <- c(
    "Positive",
    "Negative"
  )
  
  
  #---------------------------------------------------------------------------
  # PEARSON CHI-SQUARE
  #---------------------------------------------------------------------------
  
  chi <- chisq.test(
    tab,
    correct = FALSE
  )
  
  
  #---------------------------------------------------------------------------
  # FISHER EXACT TEST
  #---------------------------------------------------------------------------
  
  fisher <- fisher.test(tab)
  
  
  #---------------------------------------------------------------------------
  # SAVE
  #---------------------------------------------------------------------------
  
  district_results <- rbind(
    district_results,
    data.frame(
      
      District = dist,
      
      Male_Positive = a,
      Male_Total = a + b,
      Male_Positivity = male_pct_d,
      
      Female_Positive = c,
      Female_Total = c + d,
      Female_Positivity = female_pct_d,
      
      RR = RR,
      RR_Lower = RR_Lower,
      RR_Upper = RR_Upper,
      
      OR = OR,
      OR_Lower = OR_Lower,
      OR_Upper = OR_Upper,
      
      Chi_square = as.numeric(
        chi$statistic
      ),
      
      Chi_P = chi$p.value,
      
      Fisher_P = fisher$p.value
    )
  )
}


#===============================================================================
# 13. DISTRICT RESULTS
#===============================================================================

cat("\n============================================================\n")
cat("DISTRICT-SPECIFIC RESULTS\n")
cat("============================================================\n")

print(
  district_results,
  row.names = FALSE
)


#===============================================================================
# 14. COCHRAN-MANTEL-HAENSZEL ANALYSIS
#===============================================================================

cmh_array <- array(
  
  c(
    
    # DRJ
    50, 318,
    35, 281,
    
    # JAL
    45, 364,
    26, 265,
    
    # UDJ
    16, 134,
    15, 113,
    
    # APD
    22, 100,
    9, 87,
    
    # CRB
    21, 70,
    12, 61
    
  ),
  
  dim = c(
    2,
    2,
    5
  ),
  
  dimnames = list(
    
    Outcome = c(
      "Positive",
      "Negative"
    ),
    
    Sex = c(
      "Male",
      "Female"
    ),
    
    District = levels(
      data_raw$District
    )
  )
)


#===============================================================================
# 15. CMH TEST
#===============================================================================

cmh_test <- mantelhaen.test(
  cmh_array,
  correct = FALSE
)

cat("\n============================================================\n")
cat("COCHRAN-MANTEL-HAENSZEL TEST\n")
cat("============================================================\n")

print(cmh_test)


#===============================================================================
# 16. MANTEL-HAENSZEL COMMON OR
#===============================================================================

mh_num <- 0
mh_den <- 0

for (dist in levels(data_raw$District)) {
  
  sub <- data_raw[
    data_raw$District == dist,
  ]
  
  male <- sub[
    sub$Sex == "Male",
  ]
  
  female <- sub[
    sub$Sex == "Female",
  ]
  
  a <- male$Positive
  b <- male$Negative
  c <- female$Positive
  d <- female$Negative
  
  n <- a + b + c + d
  
  mh_num <-
    mh_num +
    (a * d) / n
  
  mh_den <-
    mh_den +
    (b * c) / n
}

MH_OR <-
  mh_num / mh_den


cat("\n============================================================\n")
cat("MANTEL-HAENSZEL COMMON OR\n")
cat("============================================================\n")

cat(
  "Common OR (Male vs Female) = ",
  round(MH_OR, 3),
  "\n",
  sep = ""
)


#===============================================================================
# 17. BINOMIAL LOGISTIC REGRESSION
#===============================================================================

model_district <- glm(
  
  cbind(
    Positive,
    Negative
  ) ~ District,
  
  data = data_raw,
  
  family = binomial(
    link = "logit"
  )
)


model_sex_district <- glm(
  
  cbind(
    Positive,
    Negative
  ) ~ Sex + District,
  
  data = data_raw,
  
  family = binomial(
    link = "logit"
  )
)


cat("\n============================================================\n")
cat("BINOMIAL LOGISTIC REGRESSION\n")
cat("============================================================\n")

print(
  summary(model_sex_district)
)


#===============================================================================
# 18. LOGISTIC REGRESSION OR AND 95% CI
#===============================================================================

model_coef <- coef(
  model_sex_district
)

model_OR <- exp(
  model_coef
)

model_CI <- exp(
  confint(
    model_sex_district
  )
)

model_p <- summary(
  model_sex_district
)$coefficients[, 4]


logistic_results <- data.frame(
  
  Variable = names(
    model_OR
  ),
  
  OR = model_OR,
  
  CI_Lower = model_CI[, 1],
  
  CI_Upper = model_CI[, 2],
  
  P_value = model_p
)


cat("\n============================================================\n")
cat("LOGISTIC REGRESSION RESULTS\n")
cat("============================================================\n")

print(
  logistic_results,
  row.names = FALSE
)


#===============================================================================
# 19. LIKELIHOOD-RATIO TEST FOR SEX
#
# Reduced model:
#   District
#
# Full model:
#   Sex + District
#
# This tests whether adding Sex improves the model after accounting for District.
#===============================================================================

LRT_sex <- anova(
  model_district,
  model_sex_district,
  test = "Chisq"
)


cat("\n============================================================\n")
cat("LIKELIHOOD-RATIO TEST FOR SEX\n")
cat("============================================================\n")

print(LRT_sex)


LRT_sex_chisq <-
  LRT_sex$Deviance[2]

LRT_sex_df <-
  LRT_sex$Df[2]

LRT_sex_p <-
  LRT_sex$`Pr(>Chi)`[2]


cat(
  "\nLikelihood-ratio chi-sq = ",
  round(LRT_sex_chisq, 3),
  "\n",
  sep = ""
)

cat(
  "df = ",
  LRT_sex_df,
  "\n",
  sep = ""
)

cat(
  "P = ",
  format.pval(
    LRT_sex_p,
    digits = 4
  ),
  "\n",
  sep = ""
)


#===============================================================================
# 20. SEX x DISTRICT INTERACTION MODEL
#===============================================================================

model_interaction <- glm(
  
  cbind(
    Positive,
    Negative
  ) ~ Sex * District,
  
  data = data_raw,
  
  family = binomial(
    link = "logit"
  )
)


cat("\n============================================================\n")
cat("SEX x DISTRICT INTERACTION MODEL\n")
cat("============================================================\n")

print(
  summary(model_interaction)
)


#===============================================================================
# 21. LIKELIHOOD-RATIO TEST FOR SEX x DISTRICT INTERACTION
#===============================================================================

LRT_interaction <- anova(
  model_sex_district,
  model_interaction,
  test = "Chisq"
)


cat("\n============================================================\n")
cat("LIKELIHOOD-RATIO TEST: SEX x DISTRICT\n")
cat("============================================================\n")

print(LRT_interaction)


interaction_chisq <-
  LRT_interaction$Deviance[2]

interaction_df <-
  LRT_interaction$Df[2]

interaction_p <-
  LRT_interaction$`Pr(>Chi)`[2]


cat(
  "\nLikelihood-ratio chi-sq = ",
  round(interaction_chisq, 3),
  "\n",
  sep = ""
)

cat(
  "df = ",
  interaction_df,
  "\n",
  sep = ""
)

cat(
  "P = ",
  format.pval(
    interaction_p,
    digits = 4
  ),
  "\n",
  sep = ""
)


#===============================================================================
# 22. MODEL FIT STATISTICS
#===============================================================================

null_model <- glm(
  
  cbind(
    Positive,
    Negative
  ) ~ 1,
  
  data = data_raw,
  
  family = binomial(
    link = "logit"
  )
)


null_deviance <-
  deviance(null_model)

residual_deviance <-
  deviance(model_sex_district)

AIC_value <-
  AIC(model_sex_district)

McFadden_R2 <-
  1 -
  (
    residual_deviance /
      null_deviance
  )


cat("\n============================================================\n")
cat("MODEL FIT\n")
cat("============================================================\n")

cat(
  "Null deviance = ",
  round(null_deviance, 3),
  "\n",
  sep = ""
)

cat(
  "Residual deviance = ",
  round(residual_deviance, 3),
  "\n",
  sep = ""
)

cat(
  "AIC = ",
  round(AIC_value, 3),
  "\n",
  sep = ""
)

cat(
  "McFadden pseudo-R2 = ",
  round(McFadden_R2, 4),
  "\n",
  sep = ""
)


#===============================================================================
# 23. PUBLICATION-QUALITY SEX-WISE BAR PLOT
#===============================================================================

bar_plot <- ggplot(
  
  data_raw,
  
  aes(
    x = District,
    y = Positivity,
    fill = Sex
  )
  
) +
  
  geom_col(
    position = position_dodge(
      width = 0.75
    ),
    width = 0.65
  ) +
  
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(
      0,
      25,
      5
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "District",
    y = "JEV Positivity (%)",
    fill = "Sex"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      color = "black"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.position = "bottom"
  )


print(bar_plot)


#===============================================================================
# 24. SAVE BAR PLOT
#===============================================================================

ggsave(
  filename = "JEV_Sex_Wise_Positivity.png",
  plot = bar_plot,
  width = 8,
  height = 5.5,
  dpi = 300
)


#===============================================================================
# 25. RISK-RATIO FOREST PLOT  (FIXED)
#
# Previous version hard-coded scale_y_continuous(limits = c(0, 3)) on a
# LINEAR scale. APD's RR_Upper (~3.98) exceeded 3, so ggplot's limits=
# silently DROPPED that point/whisker rather than merely zooming the view
# (a "Removed row(s) containing missing values" warning would have fired).
#
# Fix:
#   1. Compute axis limits dynamically from min(RR_Lower)/max(RR_Upper),
#      with a small multiplicative pad, so no CI is ever clipped.
#   2. Use a log10 y-axis, which is the correct scale for ratio measures
#      (RR/OR): it makes RR = 0.5 and RR = 2 visually equidistant from the
#      null value of 1, rather than exaggerating RRs above 1.
#===============================================================================

forest_data <- district_results

forest_data$District <- factor(
  forest_data$District,
  levels = rev(
    c(
      "DRJ",
      "JAL",
      "UDJ",
      "APD",
      "CRB"
    )
  )
)

# Dynamic axis range (log scale), padded ~15% beyond the most extreme CI bound
y_min <- min(forest_data$RR_Lower) * 0.85
y_max <- max(forest_data$RR_Upper) * 1.15

# Build a clean set of log-scale breaks that spans the data range
log_breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16)
log_breaks <- log_breaks[log_breaks >= y_min / 1.5 & log_breaks <= y_max * 1.5]
if (!1 %in% log_breaks) log_breaks <- sort(c(log_breaks, 1))


forest_plot <- ggplot(
  
  forest_data,
  
  aes(
    x = District,
    y = RR
  )
  
) +
  
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  
  geom_errorbar(
    aes(
      ymin = RR_Lower,
      ymax = RR_Upper
    ),
    width = 0.12,
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 3.5
  ) +
  
  coord_flip() +
  
  scale_y_log10(
    limits = c(y_min, y_max),
    breaks = log_breaks,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  
  labs(
    x = "District",
    y = "Risk Ratio (95% CI), log scale"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      color = "black"
    )
  )


print(forest_plot)


#===============================================================================
# 26. SAVE FOREST PLOT
#===============================================================================

ggsave(
  filename = "JEV_Sex_Wise_RR_Forest_Plot.png",
  plot = forest_plot,
  width = 8,
  height = 5,
  dpi = 300
)


#===============================================================================
# 27. SAVE RESULTS
#===============================================================================

write.csv(
  data_raw,
  "JEV_Sex_Raw_Data.csv",
  row.names = FALSE
)

write.csv(
  overall,
  "JEV_Overall_Sex_Results.csv",
  row.names = FALSE
)

write.csv(
  district_results,
  "JEV_District_Sex_Results.csv",
  row.names = FALSE
)

write.csv(
  logistic_results,
  "JEV_Sex_Logistic_Regression.csv",
  row.names = FALSE
)


#===============================================================================
# 28. FINAL SUMMARY
#===============================================================================

cat("\n\n")
cat("============================================================\n")
cat("FINAL SEX ANALYSIS SUMMARY\n")
cat("============================================================\n")

cat(
  "\nMale: ",
  sprintf("%.2f", male_risk * 100),
  "% (",
  male_pos,
  "/",
  male_total,
  ")",
  sep = ""
)

cat(
  "\nFemale: ",
  sprintf("%.2f", female_risk * 100),
  "% (",
  female_pos,
  "/",
  female_total,
  ")",
  sep = ""
)

cat(
  "\nOverall Pearson chi-sq = ",
  round(
    chi_overall$statistic,
    3
  ),
  ", df = ",
  chi_overall$parameter,
  ", P = ",
  format.pval(
    chi_overall$p.value,
    digits = 4
  ),
  sep = ""
)

cat(
  "\nOverall RR = ",
  round(
    overall_RR,
    3
  ),
  " (95% CI ",
  round(RR_lower, 3),
  "-",
  round(RR_upper, 3),
  ")",
  sep = ""
)

cat(
  "\nCMH chi-sq = ",
  round(
    cmh_test$statistic,
    3
  ),
  ", P = ",
  format.pval(
    cmh_test$p.value,
    digits = 4
  ),
  sep = ""
)

cat(
  "\nMH common OR = ",
  round(
    MH_OR,
    3
  ),
  sep = ""
)

cat(
  "\nSex LRT chi-sq = ",
  round(
    LRT_sex_chisq,
    3
  ),
  ", df = ",
  LRT_sex_df,
  ", P = ",
  format.pval(
    LRT_sex_p,
    digits = 4
  ),
  sep = ""
)

cat(
  "\nSex x District interaction LRT chi-sq = ",
  round(
    interaction_chisq,
    3
  ),
  ", df = ",
  interaction_df,
  ", P = ",
  format.pval(
    interaction_p,
    digits = 4
  ),
  sep = ""
)

cat("\n\n============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================\n")


#===============================================================================
# END OF SCRIPT
#===============================================================================
