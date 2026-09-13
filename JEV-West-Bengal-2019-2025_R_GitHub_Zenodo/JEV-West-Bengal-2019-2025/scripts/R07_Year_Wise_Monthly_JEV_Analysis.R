#===============================================================================
#
# MONTHLY JEV-POSITIVE CASES BY YEAR
# Five districts of North Bengal, 2019-2025
#
# FIX NOTE (this version):
#   1. Namespace-collision hardening. The original script called mutate(),
#      group_by(), summarise(), and pivot_longer() as bare functions. If any
#      other loaded package also defines one of these (plyr, Hmisc, and many
#      spatial/stats packages do), R silently uses the WRONG function and
#      throws errors like "unused arguments (...)" - exactly what happened
#      with select() in the companion heatmap script. All dplyr/tidyr calls
#      are now explicitly namespaced (dplyr::, tidyr::) so behaviour no
#      longer depends on package load order.
#   2. Hard-coded y-axis limits. scale_y_continuous(limits = c(0, 14)) was
#      hard-coded. The current max cell (13, Aug 2023) happens to fit, but
#      if the dataset is ever updated and a monthly count exceeds 14, ggplot
#      would silently drop that point instead of erroring. Limits/breaks are
#      now computed dynamically from the data.
#===============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

#-------------------------------------------------------------------------------
# 1. MONTHS AND YEARS
#-------------------------------------------------------------------------------

months <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

years <- 2019:2025

#-------------------------------------------------------------------------------
# 2. DRJ DATA
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
# 3. JAL DATA
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
# 4. UDJ DATA
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
# 5. APD DATA
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
# 6. CRB DATA
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

#-------------------------------------------------------------------------------
# 7. CALCULATE YEAR-WISE MONTHLY TOTALS
#-------------------------------------------------------------------------------

monthly_total <- DRJ + JAL + UDJ + APD + CRB

# Add month names
monthly_total$Month <- months

#-------------------------------------------------------------------------------
# 8. CONVERT TO LONG FORMAT  (FIX: explicit tidyr:: / dplyr:: namespacing)
#-------------------------------------------------------------------------------

plot_data <- monthly_total %>%
  
  tidyr::pivot_longer(
    cols = tidyr::starts_with("Y"),
    names_to = "Year",
    values_to = "JEV_Positive_Cases"
  ) %>%
  
  dplyr::mutate(
    Year = gsub("Y", "", Year),
    Year = factor(
      Year,
      levels = as.character(years)
    ),
    
    Month = factor(
      Month,
      levels = months
    )
  )

#-------------------------------------------------------------------------------
# 9. VERIFY YEARLY TOTALS  (FIX: explicit dplyr:: namespacing)
#-------------------------------------------------------------------------------

yearly_totals <- plot_data %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    Total_JEV_Positive = sum(JEV_Positive_Cases),
    .groups = "drop"
  )

cat("\n========================================\n")
cat("YEAR-WISE JEV-POSITIVE CASES\n")
cat("========================================\n")

print(yearly_totals)

cat("\nGrand total across all years =", sum(yearly_totals$Total_JEV_Positive), "\n")

#-------------------------------------------------------------------------------
# 10. DYNAMIC Y-AXIS LIMITS AND BREAKS  (FIX)
#
# Previously: scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2))
# was hard-coded. The current max (13, Aug 2023) fits, but any future data
# update exceeding 14 would be silently dropped by ggplot's limits=. Both
# are now derived from the data itself.
#-------------------------------------------------------------------------------

max_cases <- max(plot_data$JEV_Positive_Cases)

y_upper <- ceiling(max_cases * 1.1 / 2) * 2   # round up to nearest even number, ~10% headroom

y_breaks <- seq(0, y_upper, by = 2)

#-------------------------------------------------------------------------------
# 11. PUBLICATION-QUALITY LINE GRAPH
#-------------------------------------------------------------------------------

p <- ggplot(
  plot_data,
  aes(
    x = Month,
    y = JEV_Positive_Cases,
    group = Year,
    colour = Year
  )
) +
  
  geom_line(
    linewidth = 1.0
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = y_breaks,
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Month",
    y = "JEV-Positive Cases",
    colour = "Year"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      color = "black"
    ),
    
    axis.text.y = element_text(
      color = "black"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    panel.grid = element_blank()
  )

#-------------------------------------------------------------------------------
# 12. DISPLAY GRAPH
#-------------------------------------------------------------------------------

print(p)

#-------------------------------------------------------------------------------
# 13. SAVE PNG
#-------------------------------------------------------------------------------

ggsave(
  filename = "JEV_Monthly_Trend_By_Year.png",
  plot = p,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)

#-------------------------------------------------------------------------------
# 14. SAVE TIFF FOR JOURNAL
#-------------------------------------------------------------------------------

ggsave(
  filename = "JEV_Monthly_Trend_By_Year.tiff",
  plot = p,
  width = 9,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

#===============================================================================
# END OF SCRIPT
#===============================================================================
