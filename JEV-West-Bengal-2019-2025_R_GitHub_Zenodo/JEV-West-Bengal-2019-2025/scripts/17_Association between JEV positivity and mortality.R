# =========================================================
# Association between JEV positivity and mortality
# =========================================================

# Data
JEV_positive <- 251
JEV_negative <- 1793

Death_positive <- 58
Death_negative <- 51

# Non-death
Nondeath_positive <- JEV_positive - Death_positive
Nondeath_negative <- JEV_negative - Death_negative

# 2 x 2 table
death_table <- matrix(
  c(Death_positive, Nondeath_positive,
    Death_negative, Nondeath_negative),
  nrow = 2,
  byrow = TRUE
)

rownames(death_table) <- c("JEV-positive", "JEV-negative")
colnames(death_table) <- c("Death", "No_death")

print(death_table)

# ---------------------------------------------------------
# Fisher's exact test
# ---------------------------------------------------------

fisher_result <- fisher.test(death_table)

cat("\nFisher's exact test\n")
cat("p-value =", fisher_result$p.value, "\n")


# ---------------------------------------------------------
# Mortality risks
# ---------------------------------------------------------

risk_positive <- Death_positive / JEV_positive
risk_negative <- Death_negative / JEV_negative

cat("\nMortality risk among JEV-positive:",
    risk_positive * 100, "%\n")

cat("Mortality risk among JEV-negative:",
    risk_negative * 100, "%\n")


# ---------------------------------------------------------
# Risk Ratio
# ---------------------------------------------------------

RR <- risk_positive / risk_negative

cat("\nRisk Ratio =", RR, "\n")


# ---------------------------------------------------------
# 95% CI for Risk Ratio
# ---------------------------------------------------------

SE_log_RR <- sqrt(
  (1/Death_positive) - (1/JEV_positive) +
    (1/Death_negative) - (1/JEV_negative)
)

lower_CI <- exp(log(RR) - 1.96 * SE_log_RR)
upper_CI <- exp(log(RR) + 1.96 * SE_log_RR)

cat("95% CI:", lower_CI, "-", upper_CI, "\n")


# ---------------------------------------------------------
# Difference in mortality risk
# ---------------------------------------------------------

risk_difference <- risk_positive - risk_negative

cat("\nRisk difference =", risk_difference * 100, "percentage points\n")


# ---------------------------------------------------------
# Two-sample test of proportions
# ---------------------------------------------------------

prop_result <- prop.test(
  x = c(Death_positive, Death_negative),
  n = c(JEV_positive, JEV_negative),
  correct = FALSE
)

cat("\nTwo-sample proportion test\n")
cat("p-value =", prop_result$p.value, "\n")
