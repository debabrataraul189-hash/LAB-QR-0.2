# ============================================================
# FIGURE X: FOREST PLOT OF ADJUSTED ODDS RATIOS
# ============================================================

library(ggplot2)

# Forest plot data from final multivariable model
forest_data <- data.frame(
  Variable = c(
    "Year (per 1-year increase)",
    "DRJ vs JAL",
    "UDJ vs JAL",
    "APD vs JAL",
    "CRB vs JAL"
  ),
  
  OR = c(
    0.902,
    1.265,
    1.147,
    1.477,
    2.190
  ),
  
  Lower = c(
    0.838,
    0.906,
    0.724,
    0.928,
    1.376
  ),
  
  Upper = c(
    0.971,
    1.772,
    1.780,
    2.305,
    3.429
  )
)

# Order variables
forest_data$Variable <- factor(
  forest_data$Variable,
  levels = rev(forest_data$Variable)
)

# Forest plot
fig_forest <- ggplot(
  forest_data,
  aes(
    x = OR,
    y = Variable
  )
) +
  
  # OR = 1 reference line
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Confidence intervals
  geom_errorbarh(
    aes(
      xmin = Lower,
      xmax = Upper
    ),
    height = 0.18,
    linewidth = 0.8
  ) +
  
  # Odds ratio points
  geom_point(
    size = 3.5
  ) +
  
  # Log scale
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3, 4),
    limits = c(0.5, 4)
  ) +
  
  labs(
    x = "Adjusted odds ratio (log scale)",
    y = NULL
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.6),
    plot.title = element_blank()
  )

fig_forest

# Save high-resolution figure
ggsave(
  "Figure_X_Forest_Plot_JEV_Logistic_Regression.tiff",
  fig_forest,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)
