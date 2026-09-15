#===============================================================================
# STANDARDIZED RESIDUAL PLOT 
#===============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# 1. DATA
#-------------------------------------------------------------------------------

overall <- matrix(
  c(
    499, 54,   # 0–9
    232, 55,   # 10–19
    271, 30,   # 20–29
    222, 28,   # 30–39
    196, 31,   # 40–49
    179, 19,   # 50–59
    194, 34    # >60
  ),
  nrow = 7,
  byrow = TRUE
)

rownames(overall) <- c(
  "0–9", "10–19", "20–29", "30–39",
  "40–49", "50–59", ">60"
)

colnames(overall) <- c(
  "JEV Negative",
  "JEV Positive"
)

#-------------------------------------------------------------------------------
# 2. STANDARDIZED RESIDUALS
#-------------------------------------------------------------------------------

chi_age <- chisq.test(
  overall,
  correct = FALSE
)

residual_df <- data.frame(
  Age_Group = rownames(overall),
  Negative = chi_age$stdres[, "JEV Negative"],
  Positive = chi_age$stdres[, "JEV Positive"]
)

#-------------------------------------------------------------------------------
# 3. LONG FORMAT
#-------------------------------------------------------------------------------

plot_data <- residual_df %>%
  pivot_longer(
    cols = c(Negative, Positive),
    names_to = "JEV_Status",
    values_to = "Residual"
  )

plot_data$JEV_Status <- factor(
  plot_data$JEV_Status,
  levels = c("Negative", "Positive")
)

plot_data$Age_Group <- factor(
  plot_data$Age_Group,
  levels = c(
    "0–9", "10–19", "20–29", "30–39",
    "40–49", "50–59", ">60"
  )
)

#-------------------------------------------------------------------------------
# 4. LABELS
#-------------------------------------------------------------------------------

plot_data$Label <- sprintf(
  "%.2f",
  plot_data$Residual
)

plot_data$Label <- ifelse(
  abs(plot_data$Residual) > 2,
  paste0(plot_data$Label, "*"),
  plot_data$Label
)

#-------------------------------------------------------------------------------
# 5. PLOT
#-------------------------------------------------------------------------------

p <- ggplot(
  plot_data,
  aes(
    x = Age_Group,
    y = Residual,
    fill = JEV_Status
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  
  # Zero line
  geom_hline(
    yintercept = 0,
    linewidth = 0.6
  ) +
  
  # Significance limits
  geom_hline(
    yintercept = c(-2, 2),
    linetype = "dashed",
    linewidth = 0.45
  ) +
  
  # Residual values
  geom_text(
    aes(
      label = Label,
      vjust = ifelse(
        Residual >= 0,
        -0.45,
        1.25
      )
    ),
    position = position_dodge(width = 0.75),
    size = 3.8,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "Negative" = "#56B4E9",
      "Positive" = "#E64B35"
    )
  ) +
  
  scale_y_continuous(
    limits = c(-4.5, 4.9),
    breaks = seq(-4, 4, 1),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Age Group (years)",
    y = "Standardized Residual",
    fill = "JEV Status"
  ) +
  
  # Explanation positioned safely above bars
  annotate(
    "text",
    x = 4,
    y = 4.65,
    label = "* indicates |residual| > 2 (significant contribution)",
    size = 3.7,
    fontface = "italic"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    
    plot.margin = margin(
      t = 15,
      r = 10,
      b = 10,
      l = 10
    )
  )

print(p)
