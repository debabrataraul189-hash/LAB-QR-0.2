# ============================================================
# FIGURE Y: ANNUAL TEMPORAL TREND IN JEV POSITIVITY
# ============================================================

library(ggplot2)

# Annual data
year_data <- data.frame(
  Year = 2019:2025,
  Positive = c(30, 27, 37, 37, 49, 29, 42),
  Total = c(144, 159, 267, 442, 366, 301, 365)
)

# Calculate positivity
year_data$Positivity <- 100 * year_data$Positive / year_data$Total

# Plot
fig_year <- ggplot(year_data,
                   aes(x = Year, y = Positivity, group = 1)) +
  
  geom_line(linewidth = 0.9) +
  
  geom_point(size = 3.2) +
  
  scale_x_continuous(
    breaks = 2019:2025
  ) +
  
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Year",
    y = "JEV positivity (%)"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.6),
    plot.title = element_blank(),
    legend.position = "none"
  )

fig_year

# Save high-resolution figure
ggsave(
  "Figure_Y_Annual_JEV_Temporal_Trend.tiff",
  fig_year,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)
