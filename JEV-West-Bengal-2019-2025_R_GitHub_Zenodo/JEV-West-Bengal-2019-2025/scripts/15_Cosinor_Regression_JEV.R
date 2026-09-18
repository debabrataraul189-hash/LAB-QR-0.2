# ============================================================
# R05_Cosinor_Seasonality_JEV.R
# ============================================================
# Title:
# Annual Cosinor Analysis of Monthly JEV-Positive Cases
#
# Study period:
# January 2019 – December 2025
#
# Periodicity:
# 12 months (annual seasonality)
#
# Outcome:
# Monthly number of JEV-positive cases
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(tidyverse)
library(broom)


# ============================================================
# 2. INPUT MONTHLY DATA
# ============================================================

monthly_data <- data.frame(
  
  Year = rep(2019:2025, each = 12),
  
  Month = rep(1:12, times = 7),
  
  Positive = c(
    
    # 2019
    1, 0, 1, 2, 1, 1, 6, 7, 4, 4, 2, 1,
    
    # 2020
    0, 2, 2, 0, 1, 0, 5, 6, 3, 4, 3, 1,
    
    # 2021
    1, 2, 1, 1, 0, 1, 7, 9, 5, 5, 4, 1,
    
    # 2022
    1, 1, 1, 1, 2, 0, 6, 8, 5, 5, 4, 3,
    
    # 2023
    2, 2, 1, 2, 1, 1, 8, 10, 7, 7, 4, 4,
    
    # 2024
    1, 2, 1, 1, 1, 1, 5, 6, 5, 5, 1, 0,
    
    # 2025
    1, 4, 2, 4, 2, 2, 7, 5, 5, 5, 3, 2
    
  )
)


# ============================================================
# 3. DATA VALIDATION
# ============================================================

monthly_data <- monthly_data %>%
  arrange(Year, Month)

n_months <- nrow(monthly_data)

total_positive <- sum(monthly_data$Positive)

cat("\nNumber of months:", n_months, "\n")
cat("Total JEV-positive cases:", total_positive, "\n")

stopifnot(n_months == 84)
stopifnot(total_positive == 251)

if (any(is.na(monthly_data$Year) | is.na(monthly_data$Month) | is.na(monthly_data$Positive))) {
  stop("Missing values detected in Year, Month, or Positive.")
}

stopifnot(all(monthly_data$Month %in% 1:12))


# ============================================================
# 4. CREATE TIME AND DATE VARIABLES
# ============================================================

monthly_data <- monthly_data %>%
  mutate(
    Time = row_number(),
    Month_name = factor(Month, levels = 1:12, labels = month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  )


# ============================================================
# 5. CREATE 12-MONTH HARMONIC TERMS
# ============================================================

monthly_data <- monthly_data %>%
  mutate(
    Sin12 = sin(2 * pi * Time / 12),
    Cos12 = cos(2 * pi * Time / 12)
  )


# ============================================================
# 6. LINEAR COSINOR MODEL
# ============================================================

cosinor_model <- lm(Positive ~ Sin12 + Cos12, data = monthly_data)
print(summary(cosinor_model))


# ============================================================
# 7. TEST OVERALL COSINOR SEASONALITY
# ============================================================

cosinor_null <- lm(Positive ~ 1, data = monthly_data)
cosinor_anova <- anova(cosinor_null, cosinor_model)
print(cosinor_anova)

cosinor_p_value <- cosinor_anova$`Pr(>F)`[2]


# ============================================================
# 8. EXTRACT COSINOR COEFFICIENTS
# ============================================================

cosinor_coefficients <- tidy(cosinor_model, conf.int = TRUE)
print(cosinor_coefficients)


# ============================================================
# 9. CALCULATE MESOR
# ============================================================

Mesor <- coef(cosinor_model)["(Intercept)"]


# ============================================================
# 10. CALCULATE AMPLITUDE
# ============================================================

beta_sin <- coef(cosinor_model)["Sin12"]
beta_cos <- coef(cosinor_model)["Cos12"]

Amplitude <- sqrt(beta_sin^2 + beta_cos^2)


# ============================================================
# 11. CALCULATE ACROPHASE
# ============================================================

Acrophase_rad <- atan2(beta_sin, beta_cos)
Acrophase_deg <- (Acrophase_rad * 180 / pi)
Acrophase_deg <- (Acrophase_deg %% 360)


# ============================================================
# 12. ESTIMATE PEAK MONTH
# ============================================================

Peak_month_number <- (Acrophase_deg / 360) * 12
Peak_month_index <- (round(Peak_month_number) %% 12)

if (Peak_month_index == 0) {
  Peak_month_index <- 12
}

Peak_month_name <- month.abb[Peak_month_index]


# ============================================================
# 13. MODEL R-SQUARED
# ============================================================

R_squared <- summary(cosinor_model)$r.squared
Adjusted_R_squared <- summary(cosinor_model)$adj.r.squared


# ============================================================
# 14. PRINT COSINOR RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("LINEAR COSINOR RESULTS\n")
cat("============================================\n")
cat("Mesor:", round(Mesor, 3), "\n")
cat("Amplitude:", round(Amplitude, 3), "cases\n")
cat("Acrophase:", round(Acrophase_deg, 2), "degrees\n")
cat("Estimated peak month:", Peak_month_name, "\n")
cat("R-squared:", round(R_squared, 3), "\n")
cat("Adjusted R-squared:", round(Adjusted_R_squared, 3), "\n")
cat("Overall cosinor p-value:", format.pval(cosinor_p_value, digits = 4, eps = 0.001), "\n")


# ============================================================
# 15. PREDICTED VALUES FROM LINEAR COSINOR
# ============================================================

monthly_data <- monthly_data %>%
  mutate(Predicted_Cosinor = predict(cosinor_model, newdata = monthly_data))


# ============================================================
# 16. POISSON COSINOR MODEL
# ============================================================

poisson_cosinor <- glm(Positive ~ Sin12 + Cos12, family = poisson(link = "log"), data = monthly_data)
print(summary(poisson_cosinor))


# ============================================================
# 17. POISSON NULL MODEL
# ============================================================

poisson_null <- glm(Positive ~ 1, family = poisson(link = "log"), data = monthly_data)


# ============================================================
# 18. LIKELIHOOD-RATIO TEST FOR SEASONALITY
# ============================================================

poisson_LRT <- anova(poisson_null, poisson_cosinor, test = "Chisq")
print(poisson_LRT)

poisson_LR_chisq <- poisson_LRT$Deviance[2]
poisson_LR_df <- poisson_LRT$Df[2]
poisson_LR_p <- poisson_LRT$`Pr(>Chi)`[2]


# ============================================================
# 19. CHECK POISSON DISPERSION
# ============================================================

pearson_dispersion <- sum(residuals(poisson_cosinor, type = "pearson")^2) / poisson_cosinor$df.residual

cat("\n")
cat("Poisson dispersion:", round(pearson_dispersion, 3), "\n")


# ============================================================
# 20. POISSON MODEL COEFFICIENTS
# ============================================================

poisson_results <- tidy(poisson_cosinor, conf.int = TRUE, exponentiate = TRUE)
print(poisson_results)


# ============================================================
# 21. RESIDUAL AUTOCORRELATION
# ============================================================

poisson_residuals <- residuals(poisson_cosinor, type = "pearson")


# ============================================================
# 22. LJUNG–BOX TEST
# ============================================================

ljung_box <- Box.test(poisson_residuals, lag = 12, type = "Ljung-Box")
print(ljung_box)


# ============================================================
# 23. ACF (coloured)
# ============================================================

png("JEV_ACF_Residuals.png", width = 2000, height = 1400, res = 220)
acf(poisson_residuals, lag.max = 24, main = "ACF of Poisson Cosinor Residuals", col = "#2C77B4", lwd = 2)
dev.off()


# ============================================================
# 24. PACF (coloured)
# ============================================================

png("JEV_PACF_Residuals.png", width = 2000, height = 1400, res = 220)
pacf(poisson_residuals, lag.max = 24, main = "PACF of Poisson Cosinor Residuals", col = "#2C77B4", lwd = 2)
dev.off()


# ============================================================
# 25. POISSON PREDICTED VALUES
# ============================================================

monthly_data <- monthly_data %>%
  mutate(Predicted_Poisson = predict(poisson_cosinor, newdata = monthly_data, type = "response"))


# ============================================================
# 26. PUBLICATION-QUALITY COSINOR FIGURE (COLOURED)
# ============================================================

cosinor_plot <- ggplot(monthly_data, aes(x = Date)) +
  
  # Observed monthly cases
  geom_line(
    aes(y = Positive, color = "Observed cases"),
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = Positive, color = "Observed cases"),
    size = 2.4
  ) +
  
  # Fitted cosinor curve
  geom_line(
    aes(y = Predicted_Cosinor, color = "Fitted cosinor curve"),
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "Observed cases" = "#4D4D4D",
      "Fitted cosinor curve" = "#D7263D"
    )
  ) +
  
  labs(
    x = "Year",
    y = "JEV-positive cases",
    title = "Linear Cosinor Model: Monthly JEV-Positive Cases (2019\u20132025)"
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

print(cosinor_plot)


# ============================================================
# 27. POISSON COSINOR FIGURE (COLOURED)
# ============================================================

poisson_plot <- ggplot(monthly_data, aes(x = Date)) +
  
  geom_line(
    aes(y = Positive, color = "Observed cases"),
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = Positive, color = "Observed cases"),
    size = 2.4
  ) +
  
  geom_line(
    aes(y = Predicted_Poisson, color = "Fitted Poisson cosinor curve"),
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "Observed cases" = "#4D4D4D",
      "Fitted Poisson cosinor curve" = "#1B998B"
    )
  ) +
  
  labs(
    x = "Year",
    y = "JEV-positive cases",
    title = "Poisson Cosinor Model: Monthly JEV-Positive Cases (2019\u20132025)"
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

print(poisson_plot)


# ============================================================
# 28. FINAL SUMMARY TABLE
# ============================================================

cosinor_summary <- data.frame(
  
  Parameter = c(
    "Number of months",
    "Total JEV-positive cases",
    "Mesor",
    "Amplitude",
    "Acrophase",
    "Estimated peak month",
    "R-squared",
    "Adjusted R-squared",
    "Overall cosinor p-value",
    "Poisson LR chi-square",
    "Poisson LR degrees of freedom",
    "Poisson LR p-value",
    "Poisson dispersion",
    "Ljung-Box chi-square",
    "Ljung-Box degrees of freedom",
    "Ljung-Box p-value"
  ),
  
  Value = c(
    n_months,
    total_positive,
    round(Mesor, 3),
    round(Amplitude, 3),
    paste0(round(Acrophase_deg, 2), "\u00b0"),
    Peak_month_name,
    round(R_squared, 3),
    round(Adjusted_R_squared, 3),
    format.pval(cosinor_p_value, digits = 4, eps = 0.001),
    round(poisson_LR_chisq, 3),
    poisson_LR_df,
    format.pval(poisson_LR_p, digits = 4, eps = 0.001),
    round(pearson_dispersion, 3),
    round(as.numeric(ljung_box$statistic), 3),
    as.numeric(ljung_box$parameter),
    format.pval(ljung_box$p.value, digits = 4, eps = 0.001)
  )
)

print(cosinor_summary)


# ============================================================
# 29. SAVE RESULTS
# ============================================================

write.csv(cosinor_coefficients, "JEV_Cosinor_Coefficients.csv", row.names = FALSE)
write.csv(poisson_results, "JEV_Poisson_Cosinor_Results.csv", row.names = FALSE)
write.csv(cosinor_summary, "JEV_Cosinor_Summary.csv", row.names = FALSE)
write.csv(monthly_data, "JEV_Cosinor_Predicted_Values.csv", row.names = FALSE)


# ============================================================
# 30. SAVE FIGURES
# ============================================================

ggsave("JEV_Cosinor_Seasonality.png", plot = cosinor_plot, width = 10, height = 6, dpi = 600)
ggsave("JEV_Poisson_Cosinor_Seasonality.png", plot = poisson_plot, width = 10, height = 6, dpi = 600)


# ============================================================
# 31. FINAL CONSOLE SUMMARY
# ============================================================

cat("\n")
cat("============================================\n")
cat("FINAL JEV COSINOR ANALYSIS\n")
cat("============================================\n")
cat("Study period: January 2019 \u2013 December 2025\n")
cat("Months analyzed:", n_months, "\n")
cat("JEV-positive cases:", total_positive, "\n")
cat("Mesor:", round(Mesor, 3), "cases/month\n")
cat("Amplitude:", round(Amplitude, 3), "cases\n")
cat("Acrophase:", round(Acrophase_deg, 2), "degrees\n")
cat("Estimated fitted peak:", Peak_month_name, "\n")
cat("R-squared:", round(R_squared, 3), "\n")
cat("Overall cosinor p-value:", format.pval(cosinor_p_value, digits = 4, eps = 0.001), "\n")
cat("Poisson LR chi-square:", round(poisson_LR_chisq, 3), "\n")
cat("Poisson LR p-value:", format.pval(poisson_LR_p, digits = 4, eps = 0.001), "\n")
cat("Poisson dispersion:", round(pearson_dispersion, 3), "\n")
cat("Ljung-Box p-value:", format.pval(ljung_box$p.value, digits = 4, eps = 0.001), "\n")
cat("============================================\n")
