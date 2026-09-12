#===============================================================================
# JEV POSITIVITY BY AGE GROUP 
#===============================================================================

rm(list = ls())

#-------------------------------------------------------------------------------
# 1. AGE-GROUP DATA
#-------------------------------------------------------------------------------

overall <- matrix(
  c(
    499, 54,   # 0–9
    232, 55,   # 10–19
    271, 30,   # 20–29
    222, 28,   # 30–39
    196, 31,   # 40–49
    179, 19,   # 50–59
    194, 34    # >60
  ),
  nrow = 7,
  byrow = TRUE
)

rownames(overall) <- c(
  "0–9", "10–19", "20–29", "30–39",
  "40–49", "50–59", ">60"
)

colnames(overall) <- c("JEV Negative", "JEV Positive")

print(overall)


#-------------------------------------------------------------------------------
# 2. POSITIVITY BY AGE GROUP
#-------------------------------------------------------------------------------

age_props <- data.frame(
  Age_Group = rownames(overall),
  Total = rowSums(overall),
  JEV_Positive = overall[, "JEV Positive"],
  JEV_Negative = overall[, "JEV Negative"],
  Positivity_Rate =
    overall[, "JEV Positive"] /
    rowSums(overall) * 100
)

age_props$Positivity_Rate <- round(age_props$Positivity_Rate, 2)

print(age_props)


#-------------------------------------------------------------------------------
# 3. OVERALL CHI-SQUARE TEST
#-------------------------------------------------------------------------------

chi_age <- chisq.test(overall, correct = FALSE)

cat("\nOverall age-group association:\n")
cat("Chi-square =", round(chi_age$statistic, 3), "\n")
cat("df =", chi_age$parameter, "\n")
cat("P =", format.pval(chi_age$p.value, digits = 4), "\n")


#-------------------------------------------------------------------------------
# 4. CRAMÉR'S V
#-------------------------------------------------------------------------------

n <- sum(overall)
r <- nrow(overall)
k <- ncol(overall)

cramers_v <- sqrt(
  as.numeric(chi_age$statistic) /
    (n * min(r - 1, k - 1))
)

cat("Cramer's V =", round(cramers_v, 3), "\n")


#-------------------------------------------------------------------------------
# 5. STANDARDIZED RESIDUALS
#-------------------------------------------------------------------------------

residual_df <- data.frame(
  Age_Group = rownames(overall),
  Negative = round(chi_age$stdres[, "JEV Negative"], 2),
  Positive = round(chi_age$stdres[, "JEV Positive"], 2)
)

cat("\nStandardized residuals:\n")
print(residual_df)

cat("\nAge groups with positive residual > 2:\n")
print(residual_df$Age_Group[residual_df$Positive > 2])


#-------------------------------------------------------------------------------
# 6. ODDS RATIOS – 50–59 AS REFERENCE
#-------------------------------------------------------------------------------

reference_group <- "50–59"

ref_pos <- overall[reference_group, "JEV Positive"]
ref_neg <- overall[reference_group, "JEV Negative"]

results_or <- data.frame()

for (age in rownames(overall)) {

  if (age == reference_group) next

  pos <- overall[age, "JEV Positive"]
  neg <- overall[age, "JEV Negative"]

  OR <- (pos / neg) / (ref_pos / ref_neg)

  SE <- sqrt(
    1 / pos +
    1 / neg +
    1 / ref_pos +
    1 / ref_neg
  )

  log_OR <- log(OR)

  CI_lower <- exp(log_OR - 1.96 * SE)
  CI_upper <- exp(log_OR + 1.96 * SE)

  z <- log_OR / SE

  p <- 2 * (1 - pnorm(abs(z)))

  results_or <- rbind(
    results_or,
    data.frame(
      Age_Group = age,
      OR = round(OR, 2),
      CI_Lower = round(CI_lower, 2),
      CI_Upper = round(CI_upper, 2),
      P_value = round(p, 4)
    )
  )
}

cat("\nOdds ratios compared with 50–59 years:\n")
print(results_or)


#-------------------------------------------------------------------------------
# 7. BONFERRONI PAIRWISE COMPARISONS
#-------------------------------------------------------------------------------

age_groups <- rownames(overall)

pairwise_results <- data.frame()

for (i in 1:(length(age_groups) - 1)) {

  for (j in (i + 1):length(age_groups) {

    tab <- matrix(
      c(
        overall[i, "JEV Positive"],
        overall[i, "JEV Negative"],
        overall[j, "JEV Positive"],
        overall[j, "JEV Negative"]
      ),
      nrow = 2,
      byrow = TRUE
    )

    # Fisher's exact test for pairwise comparisons
    test <- fisher.test(tab)

    pairwise_results <- rbind(
      pairwise_results,
      data.frame(
        Group1 = age_groups[i],
        Group2 = age_groups[j],
        P_value = test$p.value
      )
    )
  }
}

pairwise_results$Adjusted_P <-
  p.adjust(pairwise_results$P_value, method = "bonferroni")

pairwise_results$P_value <-
  round(pairwise_results$P_value, 4)

pairwise_results$Adjusted_P <-
  round(pairwise_results$Adjusted_P, 4)

cat("\nBonferroni-adjusted pairwise comparisons:\n")
print(pairwise_results)


#-------------------------------------------------------------------------------
# 8. AGE-SPECIFIC SUMMARY
#-------------------------------------------------------------------------------

highest <- age_props[
  which.max(age_props$Positivity_Rate),
]

lowest <- age_props[
  which.min(age_props$Positivity_Rate),
]

cat("\n============================================\n")
cat("FINAL AGE-SPECIFIC SUMMARY\n")
cat("============================================\n")

cat(
  "Highest positivity:",
  highest$Age_Group,
  "=",
  highest$Positivity_Rate,
  "%\n"
)

cat(
  "Lowest positivity:",
  lowest$Age_Group,
  "=",
  lowest$Positivity_Rate,
  "%\n"
)


#-------------------------------------------------------------------------------
# 9. PUBLICATION-QUALITY AGE GROUP FIGURE
#-------------------------------------------------------------------------------

library(ggplot2)

plot_data <- age_props

plot_data$Age_Group <- factor(
  plot_data$Age_Group,
  levels = c(
    "0–9", "10–19", "20–29", "30–39",
    "40–49", "50–59", ">60"
  )
)

ggplot(
  plot_data,
  aes(x = Age_Group, y = Positivity_Rate)
) +
  geom_col(width = 0.7) +
  labs(
    x = "Age group (years)",
    y = "JEV positivity (%)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


#-------------------------------------------------------------------------------
# END
#-------------------------------------------------------------------------------
