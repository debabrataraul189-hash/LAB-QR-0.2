#===============================================================================
# FOREST PLOT – AGE GROUP AND JEV POSITIVITY
# Exact values from final post-hoc analysis
# Reference group: 50–59 years
#===============================================================================

library(ggplot2)

#-------------------------------------------------------------------------------
# 1. EXACT POST-HOC RESULTS
#-------------------------------------------------------------------------------

forest_data <- data.frame(
  
  Age_Group = c(
    "0–9",
    "10–19",
    "20–29",
    "30–39",
    "40–49",
    ">60"
  ),
  
  OR = c(
    1.02,
    2.23,
    1.04,
    1.19,
    1.49,
    1.65
  ),
  
  CI_Lower = c(
    0.59,
    1.28,
    0.57,
    0.64,
    0.81,
    0.91
  ),
  
  CI_Upper = c(
    1.77,
    3.90,
    1.91,
    2.20,
    2.73,
    3.00
  ),
  
  P_value = c(
    0.9451,
    0.0047,
    0.8917,
    0.5825,
    0.1970,
    0.0997
  )
)

#-------------------------------------------------------------------------------
# 2. AGE-GROUP ORDER
#-------------------------------------------------------------------------------

forest_data$Age_Group <- factor(
  forest_data$Age_Group,
  levels = rev(c(
    "0–9",
    "10–19",
    "20–29",
    "30–39",
    "40–49",
    ">60"
  ))
)

#-------------------------------------------------------------------------------
# 3. FOREST PLOT
#-------------------------------------------------------------------------------

ggplot(
  forest_data,
  aes(x = OR, y = Age_Group)
) +
  
  # 95% confidence intervals
  geom_errorbar(
    aes(
      xmin = CI_Lower,
      xmax = CI_Upper
    ),
    orientation = "y",
    height = 0.12,
    linewidth = 0.7
  ) +
  
  # Odds ratio points
  geom_point(
    size = 4
  ) +
  
  # Null/reference line
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # X-axis
  scale_x_continuous(
    limits = c(0, 4.3),
    breaks = seq(0, 4, 0.5),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Age Group (years)"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.6),
    plot.margin = margin(10, 15, 10, 10)
  )
