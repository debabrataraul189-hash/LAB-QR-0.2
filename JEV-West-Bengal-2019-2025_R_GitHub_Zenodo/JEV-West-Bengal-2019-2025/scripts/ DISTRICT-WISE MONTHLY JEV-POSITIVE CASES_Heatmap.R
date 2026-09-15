#===============================================================================
#
# DISTRICT-WISE MONTHLY JEV-POSITIVE CASES
# Five districts of North Bengal, 2019-2025
#===============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

#-------------------------------------------------------------------------------
# 1. MONTHS AND YEARS
#-------------------------------------------------------------------------------

years <- 2019:2025

months <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

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
# 7. CALCULATE TOTAL CASES BY MONTH
#-------------------------------------------------------------------------------

APD_total <- rowSums(APD)
CRB_total <- rowSums(CRB)
DRJ_total <- rowSums(DRJ)
JAL_total <- rowSums(JAL)
UDJ_total <- rowSums(UDJ)

#-------------------------------------------------------------------------------
# 8. CREATE HEATMAP DATA
#-------------------------------------------------------------------------------

heatmap_data <- data.frame(
  Month = months,

  APD = APD_total,
  CRB = CRB_total,
  DRJ = DRJ_total,
  JAL = JAL_total,
  UDJ = UDJ_total
)

#-------------------------------------------------------------------------------
# 9. VERIFY TOTALS
#-------------------------------------------------------------------------------

cat("\n==============================\n")
cat("MONTHLY TOTALS\n")
cat("==============================\n")

heatmap_data$Monthly_Total <- rowSums(
  heatmap_data[, c("APD", "CRB", "DRJ", "JAL", "UDJ")]
)

print(heatmap_data)

cat("\n==============================\n")
cat("DISTRICT TOTALS\n")
cat("==============================\n")

print(
  colSums(
    heatmap_data[, c("APD", "CRB", "DRJ", "JAL", "UDJ")]
  )
)

cat("\nGrand total =", sum(heatmap_data$Monthly_Total), "\n")

#-------------------------------------------------------------------------------
# 10. CONVERT TO LONG FORMAT
#-------------------------------------------------------------------------------

heatmap_long <- heatmap_data %>%

  dplyr::select(
    Month,
    APD,
    CRB,
    DRJ,
    JAL,
    UDJ
  ) %>%

  tidyr::pivot_longer(
    cols = c(APD, CRB, DRJ, JAL, UDJ),
    names_to = "District",
    values_to = "Total_Cases"
  )

#-------------------------------------------------------------------------------
# 11. FACTOR ORDER
#-------------------------------------------------------------------------------

heatmap_long$District <- factor(
  heatmap_long$District,
  levels = c(
    "APD",
    "CRB",
    "DRJ",
    "JAL",
    "UDJ"
  )
)

heatmap_long$Month <- factor(
  heatmap_long$Month,
  levels = rev(months)
)

#-------------------------------------------------------------------------------
# 12. DYNAMIC COLOR-SCALE LIMITS AND BREAKS  (FIX)
#
# Previously: limits = c(0, 18) and breaks = c(0, 5, 10, 15) were hard-coded.
# These happened to be safe for the current data (max cell = 18) but would
# silently clip/grey-out any future value above 18, and the legend never
# displayed a tick at the true maximum.
#
# Fix: derive both from the data itself, so the scale always covers the
# full range and the legend always labels the maximum value.
#-------------------------------------------------------------------------------

max_cases <- max(heatmap_long$Total_Cases)

fill_limits <- c(0, max_cases)

fill_breaks <- unique(
  c(
    pretty(c(0, max_cases), n = 4),
    max_cases
  )
)

fill_breaks <- sort(
  fill_breaks[fill_breaks >= 0 & fill_breaks <= max_cases]
)

#-------------------------------------------------------------------------------
# 13. PUBLICATION-QUALITY HEATMAP
#-------------------------------------------------------------------------------

p <- ggplot(
  heatmap_long,
  aes(
    x = District,
    y = Month,
    fill = Total_Cases
  )
) +

  geom_tile(
    color = "white",
    linewidth = 0.6
  ) +

  scale_fill_gradientn(
    colours = c(
      "#DCEEFF",
      "#65B8F0",
      "#00A6D6",
      "#75D6C2",
      "#B8E769",
      "#FDE047",
      "#FF8C24",
      "#D90416"
    ),

    limits = fill_limits,

    breaks = fill_breaks,

    name = "Total\nCases"
  ) +

  labs(
    x = NULL,
    y = NULL
  ) +

  theme_classic(
    base_size = 13
  ) +

  theme(

    axis.text.x = element_text(
      face = "bold",
      color = "black",
      size = 13
    ),

    axis.text.y = element_text(
      face = "bold",
      color = "black",
      size = 12
    ),

    axis.title = element_text(
      face = "bold"
    ),

    legend.title = element_text(
      face = "bold",
      size = 12
    ),

    legend.text = element_text(
      color = "black"
    ),

    legend.position = "right",

    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.7
    ),

    plot.margin = margin(
      10,
      10,
      10,
      10
    )
  )

#-------------------------------------------------------------------------------
# 14. DISPLAY
#-------------------------------------------------------------------------------

print(p)

#-------------------------------------------------------------------------------
# 15. SAVE 300 DPI
#-------------------------------------------------------------------------------

ggsave(
  filename = "JEV_District_Month_Heatmap.png",
  plot = p,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)

#-------------------------------------------------------------------------------
# 16. SAVE AS TIFF FOR JOURNAL SUBMISSION
#-------------------------------------------------------------------------------

ggsave(
  filename = "JEV_District_Month_Heatmap.tiff",
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
