# ============================================================
# JEV POSITIVITY: ASSOCIATION WITH YEAR AND DISTRICT
# Pearson Chi-square + Cramer's V
# ============================================================


# ------------------------------------------------------------
# 1. ENTER THE DATA
# ------------------------------------------------------------

data <- data.frame(
  
  Year = rep(2019:2025, each = 5),
  
  District = rep(
    c("DRJ", "JAL", "UDJ", "APD", "CRB"),
    times = 7
  ),
  
  # ----------------------------------------------------------
  # JEV-positive cases
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # Total definitive AES cases
  # ----------------------------------------------------------
  
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


# ------------------------------------------------------------
# 2. CALCULATE JEV-NEGATIVE CASES
# ------------------------------------------------------------

data$Negative <- data$Total - data$Positive


# ------------------------------------------------------------
# 3. CALCULATE JEV POSITIVITY PERCENTAGE
# ------------------------------------------------------------

data$Positivity_Percent <-
  100 * data$Positive / data$Total


# View complete dataset
print(data)


# ============================================================
# 4. CHECK TOTALS BY YEAR
# ============================================================

year_summary <- aggregate(
  cbind(Positive, Negative, Total) ~ Year,
  data = data,
  FUN = sum
)

year_summary$Positivity_Percent <-
  100 * year_summary$Positive / year_summary$Total


cat("\n====================================\n")
cat("SUMMARY BY YEAR\n")
cat("====================================\n")

print(year_summary)


# ============================================================
# 5. CHECK TOTALS BY DISTRICT
# ============================================================

district_summary <- aggregate(
  cbind(Positive, Negative, Total) ~ District,
  data = data,
  FUN = sum
)

district_summary$Positivity_Percent <-
  100 * district_summary$Positive / district_summary$Total


cat("\n====================================\n")
cat("SUMMARY BY DISTRICT\n")
cat("====================================\n")

print(district_summary)


# ============================================================
# 6. CHECK OVERALL TOTALS
# ============================================================

overall_positive <- sum(data$Positive)
overall_negative <- sum(data$Negative)
overall_total <- sum(data$Total)


cat("\n====================================\n")
cat("OVERALL TOTALS (2019-2025)\n")
cat("====================================\n")

cat(
  "Total Positive cases:",
  overall_positive,
  "\n"
)

cat(
  "Total Negative cases:",
  overall_negative,
  "\n"
)

cat(
  "Total Definitive AES cases:",
  overall_total,
  "\n"
)

cat(
  "Overall Positivity Rate:",
  100 * overall_positive / overall_total,
  "%\n"
)


# ============================================================
# 7. ASSOCIATION OF JEV POSITIVITY WITH YEAR
# ============================================================

# Create Year × JEV positivity contingency table

year_table <- xtabs(
  cbind(Positive, Negative) ~ Year,
  data = data
)


cat("\n====================================\n")
cat("YEAR × JEV POSITIVITY\n")
cat("====================================\n")

print(year_table)


# ------------------------------------------------------------
# Pearson Chi-square test
# ------------------------------------------------------------

chisq_year <- chisq.test(year_table)


cat("\nPearson Chi-square test for Year:\n")

print(chisq_year)


# ------------------------------------------------------------
# Expected counts
# ------------------------------------------------------------

cat("\nExpected counts:\n")

print(
  round(chisq_year$expected, 2)
)


# ------------------------------------------------------------
# Minimum expected count
# ------------------------------------------------------------

cat(
  "\nMinimum expected count:",
  min(chisq_year$expected),
  "\n"
)


# ------------------------------------------------------------
# Cramer's V — Year
# ------------------------------------------------------------

cramers_v_year <- sqrt(
  as.numeric(chisq_year$statistic) /
    (
      sum(year_table) *
        min(
          nrow(year_table) - 1,
          ncol(year_table) - 1
        )
    )
)


cat(
  "\nCramer's V for Year =",
  round(cramers_v_year, 3),
  "\n"
)


# ============================================================
# 8. ASSOCIATION OF JEV POSITIVITY WITH DISTRICT
# ============================================================

# Create District × JEV positivity contingency table

district_table <- xtabs(
  cbind(Positive, Negative) ~ District,
  data = data
)


cat("\n====================================\n")
cat("DISTRICT × JEV POSITIVITY\n")
cat("====================================\n")

print(district_table)


# ------------------------------------------------------------
# Pearson Chi-square test
# ------------------------------------------------------------

chisq_district <- chisq.test(district_table)


cat("\nPearson Chi-square test for District:\n")

print(chisq_district)


# ------------------------------------------------------------
# Expected counts
# ------------------------------------------------------------

cat("\nExpected counts:\n")

print(
  round(chisq_district$expected, 2)
)


# ------------------------------------------------------------
# Minimum expected count
# ------------------------------------------------------------

cat(
  "\nMinimum expected count:",
  min(chisq_district$expected),
  "\n"
)


# ------------------------------------------------------------
# Cramer's V — District
# ------------------------------------------------------------

cramers_v_district <- sqrt(
  as.numeric(chisq_district$statistic) /
    (
      sum(district_table) *
        min(
          nrow(district_table) - 1,
          ncol(district_table) - 1
        )
    )
)


cat(
  "\nCramer's V for District =",
  round(cramers_v_district, 3),
  "\n"
)


# ============================================================
# 9. FINAL RESULTS TABLE
# ============================================================

results <- data.frame(
  
  Association = c(
    "Year",
    "District"
  ),
  
  Chi_square = c(
    as.numeric(chisq_year$statistic),
    as.numeric(chisq_district$statistic)
  ),
  
  df = c(
    as.numeric(chisq_year$parameter),
    as.numeric(chisq_district$parameter)
  ),
  
  P_value = c(
    chisq_year$p.value,
    chisq_district$p.value
  ),
  
  Cramers_V = c(
    cramers_v_year,
    cramers_v_district
  )
)


# ------------------------------------------------------------
# Round statistical results
# ------------------------------------------------------------

results$Chi_square <-
  round(results$Chi_square, 3)

results$P_value <-
  round(results$P_value, 4)

results$Cramers_V <-
  round(results$Cramers_V, 3)


cat("\n====================================\n")
cat("FINAL RESULTS\n")
cat("====================================\n")

print(results)


# ============================================================
# 10. FINAL DISTRICT-WISE JEV POSITIVITY
# ============================================================

cat("\n====================================\n")
cat("JEV POSITIVITY BY DISTRICT\n")
cat("====================================\n")

print(district_summary)