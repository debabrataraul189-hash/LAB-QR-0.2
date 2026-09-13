#===============================================================================

#
# KRUSKAL-WALLIS TEST + DUNN'S POST-HOC TEST
#
# Comparison of monthly JEV-positive case counts among five districts
# 2019–2025
#
# Districts:
# DRJ = Darjeeling
# JAL = Jalpaiguri
# UDJ = Uttar Dinajpur
# APD = Alipurduar
# CRB = Cooch Behar
#
# Statistical approach:
# 1. Kruskal-Wallis test
# 2. Dunn's post-hoc test
# 3. Bonferroni correction
# 4. Export complete results
#===============================================================================


#-------------------------------------------------------------------------------
# 1. PACKAGES
#-------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(dunn.test)


#-------------------------------------------------------------------------------
# 2. REPRODUCIBILITY
#-------------------------------------------------------------------------------

set.seed(12345)


#-------------------------------------------------------------------------------
# 3. MONTHS AND YEARS
#-------------------------------------------------------------------------------

years <- 2019:2025

months <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)


#-------------------------------------------------------------------------------
# 4. DRJ DATA
#-------------------------------------------------------------------------------

DRJ <- data.frame(
  Y2019=c(0,1,3,1,0,0,1,1,0,1,1,0),
  Y2020=c(0,1,1,0,0,2,3,0,1,2,2,1),
  Y2021=c(1,0,0,0,0,0,2,3,3,1,1,2),
  Y2022=c(0,0,0,0,1,0,0,0,2,4,4,1),
  Y2023=c(1,3,0,0,2,1,0,6,2,1,0,1),
  Y2024=c(1,0,0,0,0,0,0,3,2,2,1,0),
  Y2025=c(1,0,0,2,0,0,3,1,1,4,0,0)
)


#-------------------------------------------------------------------------------
# 5. JAL DATA
#-------------------------------------------------------------------------------

JAL <- data.frame(
  Y2019=c(0,0,1,0,0,0,2,1,0,0,0,1),
  Y2020=c(0,0,2,0,0,1,2,2,0,0,0,1),
  Y2021=c(0,0,0,1,0,0,7,3,0,2,1,0),
  Y2022=c(0,0,0,1,0,0,0,0,2,4,0,1),
  Y2023=c(1,1,0,0,2,0,2,4,4,2,0,0),
  Y2024=c(0,0,0,0,0,0,2,3,1,1,0,0),
  Y2025=c(0,0,0,2,2,0,3,1,0,2,3,0)
)


#-------------------------------------------------------------------------------
# 6. UDJ DATA
#-------------------------------------------------------------------------------

UDJ <- data.frame(
  Y2019=c(0,0,0,0,0,0,1,2,1,0,1,0),
  Y2020=c(0,0,0,0,0,0,0,0,0,0,1,0),
  Y2021=c(0,0,0,0,0,0,2,2,0,1,0,0),
  Y2022=c(0,0,0,0,0,1,0,0,0,1,0,0),
  Y2023=c(0,0,0,0,0,0,1,0,0,3,0,0),
  Y2024=c(0,0,1,0,0,0,1,2,2,0,0,0),
  Y2025=c(0,1,0,0,0,0,3,0,1,1,1,1)
)


#-------------------------------------------------------------------------------
# 7. APD DATA
#-------------------------------------------------------------------------------

APD <- data.frame(
  Y2019=c(0,0,0,1,0,0,2,2,1,0,0,0),
  Y2020=c(0,0,0,0,0,0,2,0,0,0,0,0),
  Y2021=c(0,0,0,0,0,0,1,0,0,0,1,1),
  Y2022=c(0,0,0,0,0,0,0,5,2,0,0,1),
  Y2023=c(0,1,0,0,1,0,0,2,2,1,0,0),
  Y2024=c(0,0,0,0,0,0,0,1,1,0,1,0),
  Y2025=c(0,0,0,0,0,0,1,0,0,0,1,0)
)


#-------------------------------------------------------------------------------
# 8. CRB DATA
#-------------------------------------------------------------------------------

CRB <- data.frame(
  Y2019=c(1,0,1,0,0,1,1,1,0,0,0,0),
  Y2020=c(1,1,0,0,0,0,0,0,0,0,0,1),
  Y2021=c(0,0,0,1,0,0,0,0,0,0,1,0),
  Y2022=c(0,2,0,0,0,0,0,1,2,1,1,0),
  Y2023=c(0,1,0,0,0,0,0,1,3,0,0,0),
  Y2024=c(0,0,0,0,0,0,0,3,0,1,0,0),
  Y2025=c(0,1,0,2,0,0,2,1,1,0,0,0)
)


#===============================================================================
# 9. CONVERT EACH DISTRICT TO LONG FORMAT
#===============================================================================

DRJ_long <- DRJ %>%
  mutate(Month = months) %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "CASES"
  ) %>%
  mutate(
    Year = gsub("Y", "", Year),
    DISTRICT = "DRJ"
  )


JAL_long <- JAL %>%
  mutate(Month = months) %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "CASES"
  ) %>%
  mutate(
    Year = gsub("Y", "", Year),
    DISTRICT = "JAL"
  )


UDJ_long <- UDJ %>%
  mutate(Month = months) %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "CASES"
  ) %>%
  mutate(
    Year = gsub("Y", "", Year),
    DISTRICT = "UDJ"
  )


APD_long <- APD %>%
  mutate(Month = months) %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "CASES"
  ) %>%
  mutate(
    Year = gsub("Y", "", Year),
    DISTRICT = "APD"
  )


CRB_long <- CRB %>%
  mutate(Month = months) %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "CASES"
  ) %>%
  mutate(
    Year = gsub("Y", "", Year),
    DISTRICT = "CRB"
  )


#===============================================================================
# 10. COMBINE ALL DISTRICTS
#===============================================================================

df_long <- bind_rows(
  DRJ_long,
  JAL_long,
  UDJ_long,
  APD_long,
  CRB_long
)


#-------------------------------------------------------------------------------
# 11. FACTOR ORDER
#-------------------------------------------------------------------------------

df_long$DISTRICT <- factor(
  df_long$DISTRICT,
  levels = c(
    "DRJ",
    "JAL",
    "UDJ",
    "APD",
    "CRB"
  )
)

df_long$Month <- factor(
  df_long$Month,
  levels = months
)

df_long$Year <- factor(
  df_long$Year,
  levels = as.character(years)
)


#===============================================================================
# 12. VERIFY DATA
#===============================================================================

cat("\n")
cat("======================================================================\n")
cat("DATA VERIFICATION\n")
cat("======================================================================\n")

cat("\nNumber of observations:", nrow(df_long), "\n")
cat("Expected observations: 420\n")
cat("5 districts × 7 years × 12 months = 420\n\n")


#-------------------------------------------------------------------------------
# District totals
#-------------------------------------------------------------------------------

district_totals <- df_long %>%
  group_by(DISTRICT) %>%
  summarise(
    Total_JEV_Positive = sum(CASES),
    Number_of_Months = n(),
    .groups = "drop"
  )

print(district_totals)


#-------------------------------------------------------------------------------
# Overall total
#-------------------------------------------------------------------------------

cat(
  "\nGrand total JEV-positive cases =",
  sum(df_long$CASES),
  "\n"
)


#===============================================================================
# 13. KRUSKAL-WALLIS TEST
#===============================================================================

cat("\n")
cat("======================================================================\n")
cat("KRUSKAL-WALLIS TEST\n")
cat("======================================================================\n\n")

kw_result <- kruskal.test(
  CASES ~ DISTRICT,
  data = df_long
)

print(kw_result)


#-------------------------------------------------------------------------------
# Extract Kruskal-Wallis statistics
#-------------------------------------------------------------------------------

KW_chisq <- as.numeric(
  kw_result$statistic
)

KW_df <- as.numeric(
  kw_result$parameter
)

KW_p <- kw_result$p.value


cat(
  "\nKruskal-Wallis chi-square =",
  round(KW_chisq, 3),
  "\n"
)

cat(
  "Degrees of freedom =",
  KW_df,
  "\n"
)

cat(
  "P-value =",
  format.pval(KW_p, digits = 4),
  "\n"
)


#===============================================================================
# 14. DUNN'S POST-HOC TEST
#===============================================================================

cat("\n")
cat("======================================================================\n")
cat("DUNN'S POST-HOC TEST\n")
cat("Bonferroni correction\n")
cat("======================================================================\n\n")


dunn_result <- dunn.test(
  x = df_long$CASES,
  g = df_long$DISTRICT,
  method = "bonferroni",
  altp = TRUE
)


#===============================================================================
# 15. CREATE DUNN RESULTS TABLE
#===============================================================================

dunn_df <- data.frame(
  
  Comparison = dunn_result$comparisons,
  
  Z = round(
    dunn_result$Z,
    3
  ),
  
  P_adjusted = round(
    dunn_result$altP.adjusted,
    4
  )
  
)


#-------------------------------------------------------------------------------
# Add significance
#-------------------------------------------------------------------------------

dunn_df$Significant <- ifelse(
  dunn_df$P_adjusted < 0.05,
  "Yes",
  "No"
)


#===============================================================================
# 16. DISPLAY COMPLETE DUNN RESULTS
#===============================================================================

cat("\n")
cat("Complete pairwise Dunn's test results:\n\n")

print(
  dunn_df,
  row.names = FALSE
)


#===============================================================================
# 17. SIGNIFICANT PAIRWISE COMPARISONS
#===============================================================================

significant_pairs <- dunn_df %>%
  filter(
    P_adjusted < 0.05
  )


cat("\n")
cat("======================================================================\n")
cat("SIGNIFICANT PAIRWISE COMPARISONS\n")
cat("======================================================================\n\n")


if (nrow(significant_pairs) == 0) {
  
  cat(
    "No pairwise comparisons remained significant after Bonferroni correction.\n"
  )
  
} else {
  
  print(
    significant_pairs,
    row.names = FALSE
  )
}


#===============================================================================
# 18. DESCRIPTIVE STATISTICS BY DISTRICT
#===============================================================================

district_summary <- df_long %>%
  
  group_by(DISTRICT) %>%
  
  summarise(
    
    N = n(),
    
    Mean = mean(CASES),
    
    SD = sd(CASES),
    
    Median = median(CASES),
    
    IQR = IQR(CASES),
    
    Minimum = min(CASES),
    
    Maximum = max(CASES),
    
    .groups = "drop"
  )


cat("\n")
cat("======================================================================\n")
cat("DESCRIPTIVE STATISTICS BY DISTRICT\n")
cat("======================================================================\n\n")

print(
  district_summary,
  row.names = FALSE
)


#===============================================================================
# 19. BOXPLOT
#===============================================================================

boxplot_district <- ggplot(
  
  df_long,
  
  aes(
    x = DISTRICT,
    y = CASES
  )
  
) +
  
  geom_boxplot(
    width = 0.65
  ) +
  
  labs(
    x = "District",
    y = "Monthly JEV-Positive Cases"
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


#-------------------------------------------------------------------------------
# Display
#-------------------------------------------------------------------------------

print(boxplot_district)


#===============================================================================
# 20. SAVE BOXPLOT
#===============================================================================

ggsave(
  filename = "JEV_District_Kruskal_Wallis_Boxplot.png",
  plot = boxplot_district,
  width = 8,
  height = 5.5,
  units = "in",
  dpi = 300
)


#===============================================================================
# 21. EXPORT RESULTS
#===============================================================================

write.csv(
  district_summary,
  "JEV_District_Kruskal_Wallis_Descriptive_Statistics.csv",
  row.names = FALSE
)


write.csv(
  dunn_df,
  "JEV_District_Dunn_Bonferroni_Results.csv",
  row.names = FALSE
)


#===============================================================================
# 22. FINAL SUMMARY
#===============================================================================

cat("\n")
cat("======================================================================\n")
cat("FINAL STATISTICAL SUMMARY\n")
cat("======================================================================\n\n")

cat(
  "Kruskal-Wallis chi-square =",
  round(KW_chisq, 3),
  "\n"
)

cat(
  "df =",
  KW_df,
  "\n"
)

cat(
  "P =",
  format.pval(KW_p, digits = 4),
  "\n\n"
)


cat(
  "Number of significant Dunn pairwise comparisons =",
  nrow(significant_pairs),
  "\n\n"
)


if (nrow(significant_pairs) > 0) {
  
  print(
    significant_pairs,
    row.names = FALSE
  )
  
}


cat("\n")
cat("Analysis completed successfully.\n")
cat("======================================================================\n")


#===============================================================================
# END OF SCRIPT
#===============================================================================
