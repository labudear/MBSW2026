# load packages
library(dplyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)

set.seed(123)

#--------------------------------------------------
# 0. Output directory
#--------------------------------------------------

out_dir <- "U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

#--------------------------------------------------
# 1. Simulate lab + AE data
#--------------------------------------------------

n <- 300

lab_data <- data.frame(
  USUBJID = paste0("SUBJ_", 1001:(1000+n)),
  TRT = sample(c("Treatment", "Control"), n, replace = TRUE),
  LBTEST = "ALT",
  LBSTRESN = rlnorm(n, meanlog = 3, sdlog = 0.5)
)

# AE terms
ae_terms <- c(
  "Nausea", "Headache", "Dizziness", "Fatigue",
  "Elevated liver enzymes", "Vomiting", "Abdominal pain"
)

ae_data <- data.frame(
  USUBJID = sample(lab_data$USUBJID, 150),
  AEDECOD = sample(ae_terms, 150, replace = TRUE)
)

lab_data <- lab_data %>%
  left_join(ae_data, by = "USUBJID")

#--------------------------------------------------
# 2. IQR-based outlier detection (BY TRT)
#--------------------------------------------------

lab_data <- lab_data %>%
  group_by(TRT) %>%
  mutate(
    Q1 = quantile(LBSTRESN, 0.25),
    Q3 = quantile(LBSTRESN, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = LBSTRESN < lower | LBSTRESN > upper
  ) %>%
  ungroup()

#--------------------------------------------------
# 3. Hover text (AE ONLY for outliers)
#--------------------------------------------------

lab_data <- lab_data %>%
  mutate(
    AE_display = ifelse(is.na(AEDECOD), "None", AEDECOD),
    
    hover_text = ifelse(
      is_outlier,
      paste0(
        "<b>Subject:</b> ", USUBJID,
        "<br><b>Treatment:</b> ", TRT,
        "<br><b>ALT:</b> ", round(LBSTRESN, 2),
        "<br><b>Associated AE:</b> ", AE_display
      ),
      paste0(
        "<b>Subject:</b> ", USUBJID,
        "<br><b>Treatment:</b> ", TRT,
        "<br><b>ALT:</b> ", round(LBSTRESN, 2)
      )
    )
  )

# Split data
outliers <- lab_data %>% filter(is_outlier)
non_outliers <- lab_data %>% filter(!is_outlier)

#--------------------------------------------------
# 4. INTERACTIVE BOXPLOT (plotly)
#--------------------------------------------------
# IMPORTANT:
# boxpoints = FALSE → avoids plotly's incorrect whisker/outlier behavior

fig <- plot_ly() %>%
  
  # Boxplot (distribution only)
  add_trace(
    data = lab_data %>% filter(!is_outlier),
    x = ~TRT,
    y = ~LBSTRESN,
    type = "box",
    color = ~TRT,
    boxpoints = FALSE,   # ✅ critical fix
    name = "Distribution"
  ) %>%
  
  # Non-outlier points
  add_trace(
    data = non_outliers,
    x = ~TRT,
    y = ~LBSTRESN,
    type = "scatter",
    mode = "markers",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(color = "grey60", size = 6, opacity = 0.5),
    showlegend = FALSE
  ) %>%
  
  # Outliers (highlighted)
  add_trace(
    data = outliers,
    x = ~TRT,
    y = ~LBSTRESN,
    type = "scatter",
    mode = "markers",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(color = "red", size = 8),
    name = "Outliers"
  ) %>%
  
  layout(
    title = "Interactive Boxplot of ALT by Treatment",
    xaxis = list(title = "Treatment Group"),
    yaxis = list(title = "ALT (U/L)")
  )

#--------------------------------------------------
# 5. STATIC BOXPLOT (ggplot)
#--------------------------------------------------

gg <- ggplot(lab_data, aes(x = TRT, y = LBSTRESN)) +
  
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  
  geom_jitter(
    color = "grey60",
    alpha = 0.5,
    width = 0.2
  ) +
  
  geom_point(
    data = outliers,
    color = "red",
    size = 2.5
  ) +
  
  labs(
    title = "Boxplot of ALT by Treatment",
    x = "Treatment Group",
    y = "ALT (U/L)"
  ) +
  
  theme_minimal()

#--------------------------------------------------
# 6. EXPORT
#--------------------------------------------------

# Interactive HTML
saveWidget(
  fig,
  file = file.path(out_dir, "Boxplot_int_ALT2.html"),
  selfcontained = TRUE
)

# Static PNG
ggsave(
  filename = file.path(out_dir, "Boxplot_static_ALT2.png"),
  plot = gg,
  width = 8,
  height = 6,
  dpi = 300
)

fig

gg
