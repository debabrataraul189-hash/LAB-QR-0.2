# ============================================
# Association between District and Death
# among JEV-positive cases
# ============================================

# Data
data <- data.frame(
  District = c(
    "Darjeeling",
    "Jalpaiguri",
    "Alipurduar",
    "Uttar Dinajpur",
    "Cooch Behar"
  ),
  
  Total_positive = c(85, 71, 31, 31, 33),
  
  Death = c(18, 13, 9, 11, 7)
)

# Calculate non-death
data$Non_death <- data$Total_positive - data$Death

# View data
print(data)


# ============================================
# Create 5 × 2 contingency table
# ============================================

death_table <- data.frame(
  Death = data$Death,
  `Non-death` = data$Non_death,
  row.names = data$District
)

death_table <- as.matrix(death_table)

print(death_table)


# ============================================
# 1. Pearson's Chi-square test
# ============================================

pearson_chi <- chisq.test(
  death_table,
  correct = FALSE
)

print(pearson_chi)


# ============================================
# Expected frequencies
# ============================================

print(pearson_chi$expected)


# ============================================
# 2. Monte Carlo Chi-square test
# ============================================

set.seed(12345)

monte_carlo_chi <- chisq.test(
  death_table,
  correct = FALSE,
  simulate.p.value = TRUE,
  B = 100000
)

print(monte_carlo_chi)


# ============================================
# 3. Fisher's exact test
# ============================================

fisher_test <- fisher.test(
  death_table,
  simulate.p.value = TRUE,
  B = 100000
)

print(fisher_test)
