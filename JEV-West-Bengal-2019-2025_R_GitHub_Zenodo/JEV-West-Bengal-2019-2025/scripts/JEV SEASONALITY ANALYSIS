# ==============================================================
# JEV SEASONALITY ANALYSIS
# North Bengal, 2019–2025
# Correct dataset
# Overall JEV-positive cases = 251
# ==============================================================

# --------------------------------------------------------------
# 0. PACKAGES
# --------------------------------------------------------------

library(tidyverse)
library(ggplot2)


# --------------------------------------------------------------
# 1. MONTHS AND YEARS
# --------------------------------------------------------------

months <- c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)

years <- 2019:2025


# ==============================================================
# 2. DISTRICT DATA
# ==============================================================

# --------------------------------------------------------------
# DRJ
# --------------------------------------------------------------

DRJ <- data.frame(
  Y2019 = c(0,1,3,1,0,0,1,1,0,1,1,0),
  Y2020 = c(0,1,1,0,0,2,3,0,1,2,2,1),
  Y2021 = c(1,0,0,0,0,0,2,3,3,1,1,2),
  Y2022 = c(0,0,0,0,1,0,0,0,2,4,4,1),
  Y2023 = c(1,3,0,0,2,1,0,6,2,1,0,1),
  Y2024 = c(1,0,0,0,0,0,0,3,2,2,1,0),
  Y2025 = c(1,0,0,2,0,0,3,1,1,4,0,0)
)


# --------------------------------------------------------------
# JAL
# --------------------------------------------------------------

JAL <- data.frame(
  Y2019 = c(0,0,1,0,0,0,2,1,0,0,0,1),
  Y2020 = c(0,0,2,0,0,1,2,2,0,0,0,1),
  Y2021 = c(0,0,0,1,0,0,7,3,0,2,1,0),
  Y2022 = c(0,0,0,1,0,0,0,0,2,4,0,1),
  Y2023 = c(1,1,0,0,2,0,2,4,4,2,0,0),
  Y2024 = c(0,0,0,0,0,0,2,3,1,1,0,0),
  Y2025 = c(0,0,0,2,2,0,3,1,0,2,3,0)
)


# --------------------------------------------------------------
# UDJ
# --------------------------------------------------------------

UDJ <- data.frame(
  Y2019 = c(0,0,0,0,0,0,1,2,1,0,1,0),
  Y2020 = c(0,0,0,0,0,0,0,0,0,0,1,0),
  Y2021 = c(0,0,0,0,0,0,2,2,0,1,0,0),
  Y2022 = c(0,0,0,0,0,1,0,0,0,1,0,0),
  Y2023 = c(0,0,0,0,0,0,1,0,0,3,0,0),
  Y2024 = c(0,0,1,0,0,0,1,2,2,0,0,0),
  Y2025 = c(0,1,0,0,0,0,3,0,1,1,1,1)
)


# --------------------------------------------------------------
# APD
# --------------------------------------------------------------

APD <- data.frame(
  Y2019 = c(0,0,0,1,0,0,2,2,1,0,0,0),
  Y2020 = c(0,0,0,0,0,0,2,0,0,0,0,0),
  Y2021 = c(0,0,0,0,0,0,1,0,0,0,1,1),
  Y2022 = c(0,0,0,0,0,0,0,5,2,0,0,1),
  Y2023 = c(0,1,0,0,1,0,0,2,2,1,0,0),
  Y2024 = c(0,0,0,0,0,0,0,1,1,0,1,0),
  Y2025 = c(0,0,0,0,0,0,1,0,0,0,1,0)
)


# --------------------------------------------------------------
# CRB
# --------------------------------------------------------------

CRB <- data.frame(
  Y2019 = c(1,0,1,0,0,1,1,1,0,0,0,0),
  Y2020 = c(1,1,0,0,0,0,0,0,0,0,0,1),
  Y2021 = c(0,0,0,1,0,0,0,0,0,0,1,0),
  Y2022 = c(0,2,0,0,0,0,0,1,2,1,1,0),
  Y2023 = c(0,1,0,0,0,0,0,1,3,0,0,0),
  Y2024 = c(0,0,0,0,0,0,0,3,0,1,0,0),
  Y2025 = c(0,1,0,2,0,0,2,1,1,0,0,0)
)


# ==============================================================
# 3. COMBINE DISTRICTS
# ==============================================================

district_list <- list(
  DRJ = DRJ,
  JAL = JAL,
  UDJ = UDJ,
  APD = APD,
  CRB = CRB
)


# ==============================================================
# 4. CHECK DISTRICT TOTALS
# ==============================================================

district_totals <- sapply(
  district_list,
  function(x) sum(as.matrix(x))
)

print(district_totals)

overall_total <- sum(district_totals)

cat("\n====================================\n")
cat("OVERALL JEV-POSITIVE CASES:", overall_total)
cat("\n====================================\n")


# ==============================================================
# 5. CONVERT TO LONG FORMAT
# ==============================================================

all_data <- bind_rows(
  lapply(names(district_list), function(d) {
    
    x <- district_list[[d]]
    
    x$MONTH <- months
    
    x %>%
      pivot_longer(
        cols = starts_with("Y"),
        names_to = "YEAR",
        values_to = "CASES"
      ) %>%
      mutate(
        DISTRICT = d,
        YEAR = as.numeric(sub("Y", "", YEAR)),
        MONTH_NUM = match(MONTH, months)
      )
  })
)

all_data$MONTH <- factor(
  all_data$MONTH,
  levels = months,
  ordered = TRUE
)


# ==============================================================
# 6. VERIFY DATA
# ==============================================================

cat("\nTotal JEV-positive cases =", sum(all_data$CASES))

stopifnot(sum(all_data$CASES) == 251)


# ==============================================================
# 7. DISTRICT-WISE TOTALS
# ==============================================================

district_summary <- all_data %>%
  group_by(DISTRICT) %>%
  summarise(
    Total_Positive = sum(CASES),
    .groups = "drop"
  )

print(district_summary)


# ==============================================================
# 8. MONTH-WISE TOTALS
# ==============================================================

monthly_summary <- all_data %>%
  group_by(MONTH, MONTH_NUM) %>%
  summarise(
    Total_Cases = sum(CASES),
    .groups = "drop"
  ) %>%
  arrange(MONTH_NUM) %>%
  mutate(
    Percentage = round(
      Total_Cases / sum(Total_Cases) * 100,
      2
    )
  )

print(monthly_summary)


# ==============================================================
# 9. YEAR-WISE TOTALS
# ==============================================================

yearly_summary <- all_data %>%
  group_by(YEAR) %>%
  summarise(
    Total_Cases = sum(CASES),
    .groups = "drop"
  ) %>%
  arrange(YEAR)

print(yearly_summary)


# ==============================================================
# 10. DISTRICT × YEAR
# ==============================================================

district_year <- all_data %>%
  group_by(DISTRICT, YEAR) %>%
  summarise(
    Cases = sum(CASES),
    .groups = "drop"
  ) %>%
  arrange(DISTRICT, YEAR)

print(district_year)


# ==============================================================
# 11. PEAK AND LOWEST MONTH
# ==============================================================

peak_month <- monthly_summary %>%
  slice_max(Total_Cases, n = 1, with_ties = FALSE)

trough_month <- monthly_summary %>%
  slice_min(Total_Cases, n = 1, with_ties = FALSE)

cat(
  "\nPeak month:",
  as.character(peak_month$MONTH),
  "=",
  peak_month$Total_Cases,
  "cases"
)

cat(
  "\nLowest month:",
  as.character(trough_month$MONTH),
  "=",
  trough_month$Total_Cases,
  "cases\n"
)


# ==============================================================
# 12. JULY–OCTOBER CONTRIBUTION
# ==============================================================

jul_oct <- monthly_summary %>%
  filter(MONTH_NUM %in% 7:10)

jul_oct_cases <- sum(jul_oct$Total_Cases)

jul_oct_percentage <- jul_oct_cases /
  sum(monthly_summary$Total_Cases) * 100

cat(
  "\nJuly–October cases:",
  jul_oct_cases,
  "/",
  sum(monthly_summary$Total_Cases),
  "=",
  round(jul_oct_percentage, 2),
  "%\n"
)


# ==============================================================
# 13. OVERALL MONTHLY TIME SERIES
# ==============================================================

ts_data <- all_data %>%
  group_by(YEAR, MONTH, MONTH_NUM) %>%
  summarise(
    Total_Cases = sum(CASES),
    .groups = "drop"
  ) %>%
  arrange(YEAR, MONTH_NUM)


ts_je <- ts(
  ts_data$Total_Cases,
  start = c(2019, 1),
  frequency = 12
)

print(ts_je)


# ==============================================================
# 14. STL SEASONAL DECOMPOSITION
# ==============================================================

stl_je <- stl(
  ts_je,
  s.window = "periodic"
)

plot(stl_je)


# ==============================================================
# 15. MONTHLY SEASONAL INDEX
# ==============================================================

seasonal_index <- all_data %>%
  group_by(MONTH, MONTH_NUM) %>%
  summarise(
    Mean_Cases = mean(CASES),
    .groups = "drop"
  ) %>%
  arrange(MONTH_NUM)

overall_mean <- mean(all_data$CASES)

seasonal_index <- seasonal_index %>%
  mutate(
    Seasonal_Index = Mean_Cases / overall_mean
  )

print(seasonal_index)


# ==============================================================
# 16. DISTRICT × MONTH CONTINGENCY TABLE
# ==============================================================

district_month_table <- all_data %>%
  group_by(DISTRICT, MONTH) %>%
  summarise(
    Cases = sum(CASES),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = MONTH,
    values_from = Cases,
    values_fill = 0
  )

# Convert to matrix
district_month_matrix <- as.matrix(
  district_month_table[, -1]
)

rownames(district_month_matrix) <-
  district_month_table$DISTRICT

print(district_month_matrix)


# ==============================================================
# 17. PEARSON CHI-SQUARE TEST
# ==============================================================

chi_result <- chisq.test(
  district_month_matrix,
  correct = FALSE
)

cat("\n====================================\n")
cat("DISTRICT × MONTH CHI-SQUARE TEST\n")
cat("====================================\n")

print(chi_result)


# ==============================================================
# 18. EXPECTED CELL COUNTS
# ==============================================================

expected_counts <- chi_result$expected

n_low_expected <- sum(expected_counts < 5)

total_cells <- length(expected_counts)

percent_low_expected <-
  n_low_expected / total_cells * 100

cat(
  "\nExpected cells <5:",
  n_low_expected,
  "of",
  total_cells,
  "(",
  round(percent_low_expected, 1),
  "%)\n"
)


# ==============================================================
# 19. MONTE CARLO CHI-SQUARE TEST
# ==============================================================

set.seed(12345)

chi_mc <- chisq.test(
  district_month_matrix,
  correct = FALSE,
  simulate.p.value = TRUE,
  B = 100000
)

cat("\n====================================\n")
cat("MONTE CARLO CHI-SQUARE TEST\n")
cat("====================================\n")

print(chi_mc)


# ==============================================================
# 20. CRAMÉR'S V
# ==============================================================

chi_square <- as.numeric(
  chi_result$statistic
)

N <- sum(district_month_matrix)

r <- nrow(district_month_matrix)
c <- ncol(district_month_matrix)

cramers_v <- sqrt(
  chi_square /
    (N * min(r - 1, c - 1))
)

cat(
  "\nCramer's V =",
  round(cramers_v, 3),
  "\n"
)


# ==============================================================
# 21. KRUSKAL–WALLIS TEST
# ==============================================================

kw_result <- kruskal.test(
  CASES ~ DISTRICT,
  data = all_data
)

cat("\n====================================\n")
cat("KRUSKAL–WALLIS TEST\n")
cat("====================================\n")

print(kw_result)


# ==============================================================
# 22. DUNN POST-HOC TEST
# ==============================================================

if (!requireNamespace("dunn.test", quietly = TRUE)) {
  install.packages("dunn.test")
}

library(dunn.test)

dunn_result <- dunn.test(
  x = all_data$CASES,
  g = all_data$DISTRICT,
  method = "bonferroni",
  kw = FALSE,
  list = TRUE
)

cat("\n====================================\n")
cat("DUNN POST-HOC TEST\n")
cat("Bonferroni correction\n")
cat("====================================\n")

print(dunn_result)


# ==============================================================
# 23. MONTHLY CASE GRAPH
# ==============================================================

p_monthly <- ggplot(
  monthly_summary,
  aes(x = MONTH, y = Total_Cases)
) +
  geom_col() +
  geom_text(
    aes(label = Total_Cases),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    x = "Month",
    y = "JEV-positive cases"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

print(p_monthly)


# ==============================================================
# 24. YEAR-WISE MONTHLY SEASONAL PATTERN
# ==============================================================

p_yearly <- ggplot(
  ts_data,
  aes(
    x = MONTH_NUM,
    y = Total_Cases,
    group = YEAR,
    color = factor(YEAR)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = months
  ) +
  labs(
    x = "Month",
    y = "JEV-positive cases",
    color = "Year"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

print(p_yearly)


# ==============================================================
# 25. SAVE RESULTS
# ==============================================================

write.csv(
  all_data,
  "JEV_CORRECTED_DATA_2019_2025.csv",
  row.names = FALSE
)

write.csv(
  district_summary,
  "JEV_DISTRICT_TOTALS.csv",
  row.names = FALSE
)

write.csv(
  monthly_summary,
  "JEV_MONTHLY_TOTALS.csv",
  row.names = FALSE
)

write.csv(
  yearly_summary,
  "JEV_YEARLY_TOTALS.csv",
  row.names = FALSE
)

write.csv(
  seasonal_index,
  "JEV_SEASONAL_INDEX.csv",
  row.names = FALSE
)


# ==============================================================
# 26. FINAL DATA CHECK
# ==============================================================

cat("\n\n====================================")
cat("\nFINAL DATA CHECK")
cat("\n====================================")

cat("\nDRJ =", sum(DRJ))
cat("\nJAL =", sum(JAL))
cat("\nUDJ =", sum(UDJ))
cat("\nAPD =", sum(APD))
cat("\nCRB =", sum(CRB))

cat("\n-------------------------")

cat("\nOVERALL =", sum(all_data$CASES))

cat("\n====================================\n")

stopifnot(sum(all_data$CASES) == 251)
