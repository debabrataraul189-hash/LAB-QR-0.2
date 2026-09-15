#===============================================================================
# ASSOCIATION BETWEEN AGE GROUP AND JEV POSITIVITY
# Five Districts of North Bengal
#
# Analysis:
#   1. Pearson Chi-square test
#   2. Fisher's exact test using Monte-Carlo simulation
#   3. Cramer's V effect size
#   4. Expected-count assessment
#
# Outcome:
#   JEV Negative vs JEV Positive
#
# Age groups:
#   0–9, 10–19, 20–29, 30–39,
#   40–49, 50–59, >60
#
# IMPORTANT:
# All counts are entered from the final supplied dataset.
#===============================================================================


#===============================================================================
# 1. CLEAR WORKSPACE
#===============================================================================

rm(list = ls())


#===============================================================================
# 2. REPRODUCIBILITY
#===============================================================================

set.seed(12345)


#===============================================================================
# 3. AGE GROUPS
#===============================================================================

age_groups <- c(
  "0–9",
  "10–19",
  "20–29",
  "30–39",
  "40–49",
  "50–59",
  ">60"
)


#===============================================================================
# 4. ENTER FINAL DISTRICT DATA
#
# Column 1 = JEV Negative
# Column 2 = JEV Positive
#===============================================================================


#-------------------------------------------------------------------------------
# DARJEELING (DRJ)
#-------------------------------------------------------------------------------

DRJ <- matrix(
  c(
    145, 20,   # 0–9
    69, 19,   # 10–19
    94,  9,   # 20–29
    87, 16,   # 30–39
    75,  6,   # 40–49
    70,  6,   # 50–59
    59,  9    # >60
  ),
  nrow = 7,
  byrow = TRUE
)


#-------------------------------------------------------------------------------
# JALPAIGURI (JAL)
#-------------------------------------------------------------------------------

JAL <- matrix(
  c(
    205, 16,   # 0–9
    80, 14,   # 10–19
    90,  8,   # 20–29
    62,  6,   # 30–39
    67,  8,   # 40–49
    57,  7,   # 50–59
    68, 12    # >60
  ),
  nrow = 7,
  byrow = TRUE
)


#-------------------------------------------------------------------------------
# UTTAR DINAJPUR (UDJ)
#-------------------------------------------------------------------------------

UDJ <- matrix(
  c(
    92,  8,   # 0–9
    44, 11,   # 10–19
    33,  3,   # 20–29
    22,  1,   # 30–39
    17,  6,   # 40–49
    13,  1,   # 50–59
    26,  1    # >60
  ),
  nrow = 7,
  byrow = TRUE
)


#-------------------------------------------------------------------------------
# ALIPURDUAR (APD)
#-------------------------------------------------------------------------------

APD <- matrix(
  c(
    44, 4,   # 0–9
    26, 2,   # 10–19
    31, 5,   # 20–29
    32, 4,   # 30–39
    12, 7,   # 40–49
    20, 2,   # 50–59
    22, 7    # >60
  ),
  nrow = 7,
  byrow = TRUE
)


#-------------------------------------------------------------------------------
# COOCH BEHAR (CRB)
# CORRECTED FINAL DATA
#-------------------------------------------------------------------------------

CRB <- matrix(
  c(
    13, 6,   # 0–9
    13, 9,   # 10–19
    23, 5,   # 20–29
    19, 1,   # 30–39
    25, 4,   # 40–49
    19, 3,   # 50–59
    19, 5    # >60
  ),
  nrow = 7,
  byrow = TRUE
)


#===============================================================================
# 5. ADD ROW AND COLUMN NAMES
#===============================================================================

districts <- list(
  DRJ = DRJ,
  JAL = JAL,
  UDJ = UDJ,
  APD = APD,
  CRB = CRB
)

for (d in names(districts)) {
  
  rownames(districts[[d]]) <- age_groups
  
  colnames(districts[[d]]) <- c(
    "JEV Negative",
    "JEV Positive"
  )
}


#===============================================================================
# 6. DISPLAY ALL CONTINGENCY TABLES
#===============================================================================

cat("\n\n")
cat("============================================================\n")
cat("AGE GROUP × JEV STATUS TABLES\n")
cat("============================================================\n")

for (d in names(districts)) {
  
  cat("\n\n--------------------------------------------\n")
  cat("District:", d, "\n")
  cat("--------------------------------------------\n")
  
  print(districts[[d]])
}


#===============================================================================
# 7. CHECK DISTRICT TOTALS
#===============================================================================

cat("\n\n")
cat("============================================================\n")
cat("DISTRICT TOTAL CHECK\n")
cat("============================================================\n")

district_totals <- data.frame(
  District = names(districts),
  Negative = sapply(districts, function(x) sum(x[, "JEV Negative"])),
  Positive = sapply(districts, function(x) sum(x[, "JEV Positive"])),
  Total = sapply(districts, sum)
)

print(district_totals, row.names = FALSE)


#===============================================================================
# 8. FUNCTION FOR CRAMER'S V
#===============================================================================

cramers_v <- function(tab) {
  
  chi <- suppressWarnings(
    chisq.test(tab, correct = FALSE)
  )
  
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  
  V <- sqrt(
    as.numeric(chi$statistic) /
      (n * min(r - 1, c - 1))
  )
  
  return(as.numeric(V))
}


#===============================================================================
# 9. FUNCTION TO INTERPRET CRAMER'S V
#===============================================================================

interpret_cramers_v <- function(V) {
  
  if (V < 0.10) {
    
    return("Negligible")
    
  } else if (V < 0.30) {
    
    return("Weak")
    
  } else if (V < 0.50) {
    
    return("Moderate")
    
  } else {
    
    return("Strong")
  }
}


#===============================================================================
# 10. DISTRICT-SPECIFIC ANALYSIS
#===============================================================================

results <- data.frame(
  District = character(),
  N = numeric(),
  Chi_square = numeric(),
  df = numeric(),
  Chi_p_value = numeric(),
  Fisher_p_value = numeric(),
  Cramers_V = numeric(),
  Effect = character(),
  stringsAsFactors = FALSE
)


for (d in names(districts)) {
  
  tab <- districts[[d]]
  
  
  #---------------------------------------------------------------------------
  # Pearson Chi-square
  #---------------------------------------------------------------------------
  
  chi <- chisq.test(
    tab,
    correct = FALSE
  )
  
  
  #---------------------------------------------------------------------------
  # Fisher's exact test – Monte-Carlo simulation
  #---------------------------------------------------------------------------
  
  fisher <- fisher.test(
    tab,
    simulate.p.value = TRUE,
    B = 100000
  )
  
  
  #---------------------------------------------------------------------------
  # Cramer's V
  #---------------------------------------------------------------------------
  
  V <- cramers_v(tab)
  
  effect <- interpret_cramers_v(V)
  
  
  #---------------------------------------------------------------------------
  # Store results
  #---------------------------------------------------------------------------
  
  results <- rbind(
    results,
    data.frame(
      District = d,
      N = sum(tab),
      Chi_square = as.numeric(chi$statistic),
      df = as.numeric(chi$parameter),
      Chi_p_value = chi$p.value,
      Fisher_p_value = fisher$p.value,
      Cramers_V = V,
      Effect = effect
    )
  )
  
  
  #---------------------------------------------------------------------------
  # Print detailed results
  #---------------------------------------------------------------------------
  
  cat("\n\n")
  cat("============================================================\n")
  cat("DISTRICT:", d, "\n")
  cat("============================================================\n")
  
  cat("\nContingency table:\n")
  print(tab)
  
  cat("\nExpected counts:\n")
  print(round(chi$expected, 2))
  
  cat("\nChi-square test:\n")
  print(chi)
  
  cat("\nFisher's exact test (Monte-Carlo, 100,000 simulations):\n")
  print(fisher)
  
  cat("\nCramer's V:\n")
  cat(round(V, 3), "\n")
  
  cat("Effect size interpretation:\n")
  cat(effect, "\n")
}


#===============================================================================
# 11. OVERALL ANALYSIS
#===============================================================================

overall <- Reduce(
  "+",
  districts
)

rownames(overall) <- age_groups

colnames(overall) <- c(
  "JEV Negative",
  "JEV Positive"
)


#===============================================================================
# 12. DISPLAY OVERALL TABLE
#===============================================================================

cat("\n\n")
cat("============================================================\n")
cat("OVERALL AGE GROUP × JEV STATUS\n")
cat("============================================================\n")

print(overall)


#===============================================================================
# 13. OVERALL AGE-GROUP POSITIVITY
#===============================================================================

age_summary <- data.frame(
  Age_Group = age_groups,
  JEV_Negative = overall[, "JEV Negative"],
  JEV_Positive = overall[, "JEV Positive"],
  Total = rowSums(overall)
)

age_summary$Positivity <- (
  age_summary$JEV_Positive /
    age_summary$Total
) * 100

age_summary$Positivity <- round(
  age_summary$Positivity,
  2
)

cat("\n\n")
cat("============================================================\n")
cat("AGE-SPECIFIC JEV POSITIVITY\n")
cat("============================================================\n")

print(
  age_summary,
  row.names = FALSE
)


#===============================================================================
# 14. OVERALL CHI-SQUARE TEST
#===============================================================================

chi_overall <- chisq.test(
  overall,
  correct = FALSE
)

cat("\n\n")
cat("============================================================\n")
cat("OVERALL CHI-SQUARE TEST\n")
cat("============================================================\n")

print(chi_overall)


#===============================================================================
# 15. OVERALL EXPECTED COUNTS
#===============================================================================

cat("\nExpected counts:\n")

print(
  round(
    chi_overall$expected,
    2
  )
)


#===============================================================================
# 16. CHECK CHI-SQUARE ASSUMPTIONS
#===============================================================================

expected_overall <- chi_overall$expected

cat("\n\n")
cat("============================================================\n")
cat("CHI-SQUARE ASSUMPTION CHECK\n")
cat("============================================================\n")

cat(
  "\nMinimum expected cell count:",
  round(min(expected_overall), 3),
  "\n"
)

cat(
  "Number of expected cells < 5:",
  sum(expected_overall < 5),
  "\n"
)

cat(
  "Percentage of expected cells < 5:",
  round(
    mean(expected_overall < 5) * 100,
    2
  ),
  "%\n"
)


#===============================================================================
# 17. OVERALL FISHER'S EXACT TEST
#===============================================================================

fisher_overall <- fisher.test(
  overall,
  simulate.p.value = TRUE,
  B = 100000
)

cat("\n\n")
cat("============================================================\n")
cat("OVERALL FISHER'S EXACT TEST\n")
cat("============================================================\n")

print(fisher_overall)


#===============================================================================
# 18. OVERALL CRAMER'S V
#===============================================================================

V_overall <- cramers_v(
  overall
)

effect_overall <- interpret_cramers_v(
  V_overall
)

cat("\n\n")
cat("============================================================\n")
cat("OVERALL CRAMER'S V\n")
cat("============================================================\n")

cat(
  "Cramer's V =",
  round(V_overall, 3),
  "\n"
)

cat(
  "Effect size =",
  effect_overall,
  "\n"
)


#===============================================================================
# 19. ADD OVERALL RESULTS TO RESULTS TABLE
#===============================================================================

results <- rbind(
  results,
  data.frame(
    District = "Overall",
    N = sum(overall),
    Chi_square = as.numeric(
      chi_overall$statistic
    ),
    df = as.numeric(
      chi_overall$parameter
    ),
    Chi_p_value = chi_overall$p.value,
    Fisher_p_value = fisher_overall$p.value,
    Cramers_V = V_overall,
    Effect = effect_overall
  )
)


#===============================================================================
# 20. ROUND NUMERICAL RESULTS
#===============================================================================

results$Chi_square <- round(
  results$Chi_square,
  3
)

results$Cramers_V <- round(
  results$Cramers_V,
  3
)


#===============================================================================
# 21. PUBLICATION-READY P-VALUE COLUMNS
#===============================================================================

results$Chi_p <- format.pval(
  results$Chi_p_value,
  digits = 4,
  eps = 0.0001
)

results$Fisher_p <- format.pval(
  results$Fisher_p_value,
  digits = 4,
  eps = 0.0001
)


#===============================================================================
# 22. FINAL RESULTS TABLE
#===============================================================================

final_results <- results[
  ,
  c(
    "District",
    "N",
    "Chi_square",
    "df",
    "Chi_p",
    "Fisher_p",
    "Cramers_V",
    "Effect"
  )
]

cat("\n\n")
cat("============================================================\n")
cat("FINAL RESULTS TABLE\n")
cat("============================================================\n")

print(
  final_results,
  row.names = FALSE
)


#===============================================================================
# 23. SIGNIFICANCE INTERPRETATION
#===============================================================================

final_results$Significance <- ifelse(
  results$Chi_p_value < 0.05,
  "Significant",
  "Not significant"
)

cat("\n\n")
cat("============================================================\n")
cat("FINAL TABLE WITH SIGNIFICANCE\n")
cat("============================================================\n")

print(
  final_results,
  row.names = FALSE
)


#===============================================================================
# 24. SAVE RESULTS
#===============================================================================

write.csv(
  final_results,
  "Age_JEV_Association_ChiSquare_Fisher_CramersV.csv",
  row.names = FALSE
)

write.csv(
  age_summary,
  "Age_Group_JEV_Positivity_Summary.csv",
  row.names = FALSE
)

cat("\n\n")
cat("Results saved as:\n")
cat("1. Age_JEV_Association_ChiSquare_Fisher_CramersV.csv\n")
cat("2. Age_Group_JEV_Positivity_Summary.csv\n")


#===============================================================================
# 25. EXPECTED COUNT CHECK BY DISTRICT
#===============================================================================

cat("\n\n")
cat("============================================================\n")
cat("EXPECTED COUNT CHECK BY DISTRICT\n")
cat("============================================================\n")

for (d in names(districts)) {
  
  chi <- chisq.test(
    districts[[d]],
    correct = FALSE
  )
  
  expected <- chi$expected
  
  cat("\nDistrict:", d, "\n")
  
  cat(
    "Minimum expected count:",
    round(min(expected), 2),
    "\n"
  )
  
  cat(
    "Number of cells < 5:",
    sum(expected < 5),
    "\n"
  )
  
  cat(
    "Percentage of cells < 5:",
    round(mean(expected < 5) * 100, 2),
    "%\n"
  )
}


#===============================================================================
# END OF ANALYSIS
#===============================================================================
