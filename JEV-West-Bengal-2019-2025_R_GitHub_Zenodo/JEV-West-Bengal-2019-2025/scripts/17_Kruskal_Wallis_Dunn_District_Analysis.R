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

# NOTE: the 'dunn.test' package is not used here. Dunn's post-hoc test is
# implemented manually below (function dunn_test_manual), following the
# standard Dunn (1964) rank-based formula with tie correction and Bonferroni
# adjustment. This reproduces the same Z statistics and adjusted p-values
# that the dunn.test package would return with method = "bonferroni".


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
# 14. DUNN'S POST-HOC TEST (MANUAL IMPLEMENTATION)
#===============================================================================
#
# dunn.test package is unavailable in this environment (no CRAN access),
# so Dunn's (1964) rank-sum post-hoc test is implemented directly:
#
#   z_ij = (Rbar_i - Rbar_j) /
#          sqrt( ((N*(N+1)/12) - (sum(t^3 - t) / (12*(N-1)))) * (1/n_i + 1/n_j) )
#
# where Rbar = mean rank per group, N = total n, t = tied-group sizes.
# Two-sided p-values are computed from the standard normal distribution
# and then Bonferroni-adjusted (p * number of comparisons, capped at 1).
#===============================================================================

cat("\n")
cat("======================================================================\n")
cat("DUNN'S POST-HOC TEST\n")
cat("Bonferroni correction\n")
cat("======================================================================\n\n")


dunn_test_manual <- function(x, g) {

  g <- factor(g)
  N <- length(x)
  R <- rank(x)                       # ranks across ALL groups (with ties averaged)

  groups <- levels(g)
  k <- length(groups)

  # Mean rank and n per group
  rank_stats <- data.frame(
    group = groups,
    n = as.numeric(table(g)[groups]),
    mean_rank = sapply(groups, function(gr) mean(R[g == gr]))
  )

  # Tie correction term: sum(t^3 - t) over all tied rank groups
  tie_table <- table(R)
  tie_term <- sum(tie_table^3 - tie_table)

  sigma_base <- (N * (N + 1) / 12) - (tie_term / (12 * (N - 1)))

  # All pairwise comparisons
  pairs <- combn(groups, 2, simplify = FALSE)
  n_comp <- length(pairs)

  comparisons <- character(n_comp)
  Z <- numeric(n_comp)
  P_raw <- numeric(n_comp)

  for (idx in seq_along(pairs)) {

    gi <- pairs[[idx]][1]
    gj <- pairs[[idx]][2]

    Ri <- rank_stats$mean_rank[rank_stats$group == gi]
    Rj <- rank_stats$mean_rank[rank_stats$group == gj]
    ni <- rank_stats$n[rank_stats$group == gi]
    nj <- rank_stats$n[rank_stats$group == gj]

    se <- sqrt(sigma_base * (1 / ni + 1 / nj))

    z_val <- (Ri - Rj) / se
    p_val <- 2 * (1 - pnorm(abs(z_val)))

    comparisons[idx] <- paste(gi, "-", gj)
    Z[idx] <- z_val
    P_raw[idx] <- p_val
  }

  P_adjusted <- pmin(P_raw * n_comp, 1)

  data.frame(
    comparisons = comparisons,
    Z = Z,
    P_raw = P_raw,
    altP.adjusted = P_adjusted
  )
}


dunn_result <- dunn_test_manual(
  x = df_long$CASES,
  g = df_long$DISTRICT
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
# 19. BOXPLOT (COLOURED BY DISTRICT)
#===============================================================================

district_colors <- c(
  "DRJ" = "#D7263D",   # red
  "JAL" = "#1B998B",   # teal
  "UDJ" = "#F4A825",   # amber
  "APD" = "#2C77B4",   # blue
  "CRB" = "#7A5195"    # purple
)

boxplot_district <- ggplot(

  df_long,

  aes(
    x = DISTRICT,
    y = CASES,
    fill = DISTRICT
  )

) +

  geom_boxplot(
    width = 0.65,
    color = "black",
    alpha = 0.9,
    outlier.color = "black",
    outlier.shape = 21,
    outlier.fill = "white",
    outlier.size = 2
  ) +

  scale_fill_manual(
    values = district_colors,
    name = "District"
  ) +

  labs(
    x = "District",
    y = "Monthly JEV-Positive Cases",
    title = "Distribution of Monthly JEV-Positive Cases by District (2019\u20132025)"
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

    plot.title = element_text(
      face = "bold",
      size = 13
    ),

    legend.position = "none"

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
