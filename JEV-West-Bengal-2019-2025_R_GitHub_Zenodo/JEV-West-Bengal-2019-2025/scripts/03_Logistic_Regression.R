# ============================================================
# JEV POSITIVITY: LINEAR LOGISTIC REGRESSION AND 
# MULTIVARIABLE BINOMIAL LOGISTIC REGRESSION
# (DATA CORRECTED TO MATCH SOURCE SPREADSHEET)
# ============================================================

# ------------------------------------------------------------
# 1. Enter the data
# ------------------------------------------------------------

data <- data.frame(
  Year = rep(2019:2025, each = 5),
  District = rep(c("DRJ", "JAL", "UDJ", "APD", "CRB"), times = 7),
  
  # JEV-positive cases
  Positive = c(
    # 2019
    9, 5, 5, 6, 5,
    # 2020
    13, 8, 1, 2, 3,
    # 2021
    13, 14, 5, 3, 2,
    # 2022
    12, 8, 2, 8, 7,
    # 2023
    17, 16, 4, 7, 5,
    # 2024
    9, 7, 6, 3, 4,
    # 2025
    12, 13, 8, 2, 7
  ),
  
  # Total tested cases (corrected to match source spreadsheet)
  Total = c(
    # 2019
    39, 50, 17, 20, 18,
    # 2020
    57, 52, 17, 17, 16,
    # 2021
    90, 98, 36, 18, 25,
    # 2022
    157, 161, 50, 48, 26,
    # 2023
    123, 119, 50, 44, 30,
    # 2024
    104, 102, 39, 33, 23,
    # 2025
    114, 118, 69, 38, 26
  )
)

# Calculate JEV-negative cases
data$Negative <- data$Total - data$Positive

# Calculate positivity percentage
data$Positivity_Percent <- 100 * data$Positive / data$Total

# Convert Year to numeric (for linear trend) and factor (for categorical)
data$Year_numeric <- data$Year
data$Year_factor <- as.factor(data$Year)

# Convert District to factor
data$District <- as.factor(data$District)

# View data
print(head(data))

# ============================================================
# 2. LINEAR REGRESSION (Using proportions)
# ============================================================

# Create response variable as proportion of positives
data$Proportion <- data$Positive / data$Total

# ------------------------------------------------------------------
# 2a. Simple linear regression: Positivity ~ Year (numeric)
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("LINEAR REGRESSION: Positivity ~ Year\n")
cat("====================================\n")

model_linear_year <- lm(Proportion ~ Year_numeric, data = data)
summary(model_linear_year)

# ------------------------------------------------------------------
# 2b. Simple linear regression: Positivity ~ District
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("LINEAR REGRESSION: Positivity ~ District\n")
cat("====================================\n")

model_linear_district <- lm(Proportion ~ District, data = data)
summary(model_linear_district)

# ------------------------------------------------------------------
# 2c. Multiple linear regression: Positivity ~ Year + District
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("MULTIPLE LINEAR REGRESSION: Positivity ~ Year + District\n")
cat("====================================\n")

model_linear_both <- lm(Proportion ~ Year_numeric + District, data = data)
summary(model_linear_both)

# ============================================================
# 3. BINOMIAL LOGISTIC REGRESSION
# ============================================================

# ------------------------------------------------------------------
# 3a. Simple logistic regression: Positive ~ Year (numeric)
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("LOGISTIC REGRESSION: Positive ~ Year (numeric)\n")
cat("====================================\n")

model_logit_year <- glm(
  cbind(Positive, Negative) ~ Year_numeric,
  data = data,
  family = binomial(link = "logit")
)

summary(model_logit_year)

# Calculate Odds Ratio for Year
OR_year <- exp(coef(model_logit_year)[2])
cat("\nOdds Ratio for Year (per 1-year increase):", round(OR_year, 3), "\n")
cat("95% CI:", round(exp(confint(model_logit_year)[2,]), 3), "\n")

# ------------------------------------------------------------------
# 3b. Simple logistic regression: Positive ~ District
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("LOGISTIC REGRESSION: Positive ~ District\n")
cat("====================================\n")

model_logit_district <- glm(
  cbind(Positive, Negative) ~ District,
  data = data,
  family = binomial(link = "logit")
)

summary(model_logit_district)

# Calculate Odds Ratios for Districts (using APD as reference)
# Relevel to set APD as reference (since it has lowest positivity)
data$District <- relevel(data$District, ref = "JAL")

# Refit model with new reference
model_logit_district <- glm(
  cbind(Positive, Negative) ~ District,
  data = data,
  family = binomial(link = "logit")
)

summary(model_logit_district)

# Calculate Odds Ratios for Districts
OR_district <- exp(coef(model_logit_district)[-1])
cat("\nOdds Ratios for Districts (reference = JAL):\n")
print(round(OR_district, 3))

# ------------------------------------------------------------------
# 3c. MULTIVARIABLE LOGISTIC REGRESSION: Positive ~ Year + District
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("MULTIVARIABLE LOGISTIC REGRESSION: Positive ~ Year + District\n")
cat("====================================\n")

model_logit_both <- glm(
  cbind(Positive, Negative) ~ Year_numeric + District,
  data = data,
  family = binomial(link = "logit")
)

summary(model_logit_both)

# ------------------------------------------------------------------
# 3d. Logistic regression with interaction: Year * District
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("LOGISTIC REGRESSION WITH INTERACTION: Year * District\n")
cat("====================================\n")

model_logit_interaction <- glm(
  cbind(Positive, Negative) ~ Year_numeric * District,
  data = data,
  family = binomial(link = "logit")
)

summary(model_logit_interaction)

# ============================================================
# 4. COMPARE MODELS USING ANOVA
# ============================================================

cat("\n====================================\n")
cat("MODEL COMPARISON (ANOVA)\n")
cat("====================================\n")

# Compare nested models
cat("\nComparing: Year vs Year + District\n")
anova(model_logit_year, model_logit_both, test = "Chisq")

cat("\nComparing: Year + District vs Year * District\n")
anova(model_logit_both, model_logit_interaction, test = "Chisq")

# ============================================================
# 5. MODEL DIAGNOSTICS FOR BEST MODEL
# ============================================================

# Using the multivariable model (Year + District) as best model
best_model <- model_logit_both

# ------------------------------------------------------------------
# 5a. Deviance residuals
# ------------------------------------------------------------------

cat("\n====================================\n")
cat("MODEL DIAGNOSTICS\n")
cat("====================================\n")

cat("\nDeviance residuals (first 10):\n")
print(round(residuals(best_model, type = "deviance")[1:10], 3))

# ------------------------------------------------------------------
# 5b. Cook's distance (influence measures)
# ------------------------------------------------------------------

cooks_d <- cooks.distance(best_model)
cat("\nCook's distance (first 10):\n")
print(round(cooks_d[1:10], 4))

# ------------------------------------------------------------------
# 5c. Summary statistics
# ------------------------------------------------------------------

cat("\nModel Summary Statistics:\n")
cat("Null deviance:", round(best_model$null.deviance, 2), "\n")
cat("Residual deviance:", round(best_model$deviance, 2), "\n")
cat("AIC:", round(AIC(best_model), 2), "\n")

# Pseudo R-squared (McFadden)
pseudo_r2 <- 1 - (best_model$deviance / best_model$null.deviance)
cat("McFadden's Pseudo R-squared:", round(pseudo_r2, 4), "\n")

# ============================================================
# 6. PREDICTED PROBABILITIES
# ============================================================

# Create prediction data frame for all combinations
pred_data <- expand.grid(
  Year_numeric = 2019:2025,
  District = levels(data$District)
)

# Add predictions
pred_data$Predicted_Prob <- predict(
  best_model,
  newdata = pred_data,
  type = "response"
)

pred_data$Predicted_Percent <- round(100 * pred_data$Predicted_Prob, 2)

# Reshape to show by District and Year
pred_wide <- reshape(
  pred_data,
  idvar = "Year_numeric",
  timevar = "District",
  direction = "wide"
)

cat("\n====================================\n")
cat("PREDICTED POSITIVITY (%)\n")
cat("====================================\n")

# Rename columns
names(pred_wide) <- gsub("Predicted_Percent.", "", names(pred_wide))
print(pred_wide)

# ============================================================
# 7. SUMMARY TABLE OF ALL LOGISTIC REGRESSION MODELS
# ============================================================

# Create summary table
model_summary <- data.frame(
  Model = c(
    "Year (univariate)",
    "District (univariate)",
    "Year + District (multivariable)",
    "Year * District (interaction)"
  ),
  
  AIC = round(c(
    AIC(model_logit_year),
    AIC(model_logit_district),
    AIC(model_logit_both),
    AIC(model_logit_interaction)
  ), 2),
  
  Residual_Deviance = round(c(
    model_logit_year$deviance,
    model_logit_district$deviance,
    model_logit_both$deviance,
    model_logit_interaction$deviance
  ), 2),
  
  Null_Deviance = round(c(
    model_logit_year$null.deviance,
    model_logit_district$null.deviance,
    model_logit_both$null.deviance,
    model_logit_interaction$null.deviance
  ), 2),
  
  Pseudo_R2 = round(c(
    1 - (model_logit_year$deviance / model_logit_year$null.deviance),
    1 - (model_logit_district$deviance / model_logit_district$null.deviance),
    1 - (model_logit_both$deviance / model_logit_both$null.deviance),
    1 - (model_logit_interaction$deviance / model_logit_interaction$null.deviance)
  ), 4)
)

cat("\n====================================\n")
cat("MODEL COMPARISON SUMMARY\n")
cat("====================================\n")
print(model_summary)

# ============================================================
# 8. COEFFICIENTS TABLE FOR MULTIVARIABLE MODEL
# ============================================================

cat("\n====================================\n")
cat("MULTIVARIABLE LOGISTIC REGRESSION COEFFICIENTS\n")
cat("====================================\n")

# Extract coefficients with confidence intervals
coef_summary <- summary(best_model)$coefficients
conf_int <- exp(confint(best_model))

# Combine into a nice table
result_table <- data.frame(
  Variable = rownames(coef_summary),
  Estimate = round(coef_summary[,1], 4),
  SE = round(coef_summary[,2], 4),
  Z_value = round(coef_summary[,3], 3),
  P_value = round(coef_summary[,4], 4),
  OR = round(exp(coef_summary[,1]), 3),
  OR_lower = round(exp(conf_int[,1]), 3),
  OR_upper = round(exp(conf_int[,2]), 3)
)

print(result_table)

# ============================================================
# 9. ADDITIONAL: FOREST PLOT DATA EXTRACTION
# ============================================================

cat("\n====================================\n")
cat("FOREST PLOT DATA (for visualization)\n")
cat("====================================\n")

# Extract OR and CI for forest plot
forest_data <- data.frame(
  Variable = c("Year (per year)", 
               paste("District:", levels(data$District)[-1])),
  OR = round(exp(coef(best_model)[-1]), 3),
  Lower = round(exp(confint(best_model)[-1, 1]), 3),
  Upper = round(exp(confint(best_model)[-1, 2]), 3),
  P_value = round(summary(best_model)$coefficients[-1, 4], 4)
)

print(forest_data)

# ============================================================
# 10. SAVE RESULTS TO CSV
# ============================================================

# Uncomment to save results
# write.csv(result_table, "logistic_regression_results.csv", row.names = FALSE)
# write.csv(forest_data, "forest_plot_data.csv", row.names = FALSE)

cat("\n====================================\n")
cat("Analysis Complete!\n")
cat("====================================\n")
