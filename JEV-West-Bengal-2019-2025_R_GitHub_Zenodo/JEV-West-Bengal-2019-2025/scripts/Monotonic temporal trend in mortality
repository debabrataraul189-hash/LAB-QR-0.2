# ============================================
# Monotonic temporal trend in mortality
# among JEV-positive cases, 2019–2025
# ============================================

# Install once if required:
# install.packages("DescTools")

library(DescTools)

# --------------------------------------------
# Data
# --------------------------------------------

data <- data.frame(
  Year = 2019:2025,
  Total_positive = c(30, 27, 37, 37, 49, 29, 42),
  Death = c(10, 5, 3, 4, 14, 8, 14)
)

# Non-death
data$Non_death <- data$Total_positive - data$Death

# View data
print(data)

# --------------------------------------------
# Create 2 × 7 contingency table
# --------------------------------------------

death_table <- rbind(
  Death = data$Death,
  `Non-death` = data$Non_death
)

colnames(death_table) <- data$Year

print(death_table)

# --------------------------------------------
# Cochran-Armitage trend test
# --------------------------------------------

trend_test <- CochranArmitageTest(
  death_table,
  alternative = "two.sided"
)

print(trend_test)
