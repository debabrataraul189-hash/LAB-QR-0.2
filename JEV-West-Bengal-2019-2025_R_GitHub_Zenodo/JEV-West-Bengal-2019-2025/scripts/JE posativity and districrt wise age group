#===============================================================================
# JEV POSITIVITY BY AGE GROUP AND DISTRICT
#===============================================================================

library(ggplot2)

#-------------------------------------------------------------------------------
# 1. DATA
#-------------------------------------------------------------------------------

age_district <- data.frame(
  
  Age_Group = rep(
    c("0–9", "10–19", "20–29", "30–39",
      "40–49", "50–59", ">60"),
    each = 5
  ),
  
  District = rep(
    c("DRJ", "JAL", "UDJ", "APD", "CRB"),
    times = 7
  ),
  
  Positivity_Rate = c(
    
    # 0–9
    12.12, 7.24, 8.00, 8.33, 31.58,
    
    # 10–19
    21.59, 14.89, 20.00, 7.14, 40.91,
    
    # 20–29
    8.74, 8.16, 8.33, 13.89, 17.86,
    
    # 30–39
    15.53, 8.82, 4.35, 11.11, 17.86,
    
    # 40–49
    7.41, 10.67, 26.09, 36.84, 13.79,
    
    # 50–59
    8.97, 10.94, 7.14, 9.09, 13.64,
    
    # >60
    13.24, 15.00, 3.70, 24.14, 20.83
  )
)

#-------------------------------------------------------------------------------
# 2. AGE GROUP ORDER
#-------------------------------------------------------------------------------

age_district$Age_Group <- factor(
  age_district$Age_Group,
  levels = c(
    "0–9",
    "10–19",
    "20–29",
    "30–39",
    "40–49",
    "50–59",
    ">60"
  )
)

#-------------------------------------------------------------------------------
# 3. DISTRICT ORDER
#-------------------------------------------------------------------------------

age_district$District <- factor(
  age_district$District,
  levels = c(
    "DRJ",
    "JAL",
    "UDJ",
    "APD",
    "CRB"
  )
)

#-------------------------------------------------------------------------------
# 4. GROUPED BAR PLOT
#-------------------------------------------------------------------------------

p <- ggplot(
  age_district,
  aes(
    x = Age_Group,
    y = Positivity_Rate,
    fill = District
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.80),
    width = 0.70
  ) +
  
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Age Group (years)",
    y = "JEV Positivity (%)",
    fill = "District"
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p)
