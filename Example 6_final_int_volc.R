# প্রয়োজনীয় packages
library(dplyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)

set.seed(123)

#--------------------------------------------------
# 0. Output directory (USER-SPECIFIED)
#--------------------------------------------------

out_dir <- "U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"

# Create directory if it doesn't exist
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

#--------------------------------------------------
# 1. SOC definition (ordered)
#--------------------------------------------------

soc_levels <- c(
  "Cardiac disorders",
  "Gastrointestinal disorders",
  "Nervous system disorders",
  "Respiratory disorders",
  "General disorders",
  "Infections and infestations"
)

soc_terms <- list(
  "Cardiac disorders" = c("Atrial fibrillation", "Myocardial infarction", "Tachycardia", "Bradycardia"),
  "Gastrointestinal disorders" = c("Nausea", "Vomiting", "Diarrhea", "Constipation"),
  "Nervous system disorders" = c("Headache", "Dizziness", "Seizure", "Migraine"),
  "Respiratory disorders" = c("Dyspnea", "Cough", "Sinusitis", "Pneumonia"),
  "General disorders" = c("Fatigue", "Pyrexia", "Edema peripheral", "Pain"),
  "Infections and infestations" = c("Urinary tract infection", "Influenza", "COVID-19", "Sepsis")
)

ae_terms_df <- bind_rows(lapply(names(soc_terms), function(soc) {
  data.frame(
    AEBODSYS = soc,
    AEDECOD = soc_terms[[soc]]
  )
}))

ae_terms_df <- ae_terms_df[rep(1:nrow(ae_terms_df), length.out = 200), ]

#--------------------------------------------------
# 2. Simulate stats
#--------------------------------------------------

ae_summary <- ae_terms_df %>%
  mutate(
    TRT_total = 200,
    CTRL_total = 200,
    TRT_count = rpois(n(), lambda = sample(5:50, n(), replace = TRUE)),
    CTRL_count = rpois(n(), lambda = sample(5:50, n(), replace = TRUE))
  ) %>%
  rowwise() %>%
  mutate(
    p_trt = TRT_count / TRT_total,
    p_ctrl = CTRL_count / CTRL_total,
    log2FC = log2((p_trt + 1e-6) / (p_ctrl + 1e-6)),
    
    p_value = fisher.test(matrix(c(
      TRT_count,
      TRT_total - TRT_count,
      CTRL_count,
      CTRL_total - CTRL_count
    ), nrow = 2))$p.value,
    
    negLog10P = -log10(p_value)
  ) %>%
  ungroup()

#--------------------------------------------------
# 3. Apply SOC ordering
#--------------------------------------------------

ae_summary$AEBODSYS <- factor(ae_summary$AEBODSYS, levels = soc_levels)

#--------------------------------------------------
# 4. Thresholds
#--------------------------------------------------

fc_thresh <- 1
p_thresh <- 0.05

ae_summary <- ae_summary %>%
  mutate(
    p_flag = p_value <= p_thresh
  )

#--------------------------------------------------
# 5. Fixed SOC colors
#--------------------------------------------------

soc_colors <- c(
  "Cardiac disorders" = "#1b9e77",
  "Gastrointestinal disorders" = "#d95f02",
  "Nervous system disorders" = "#7570b3",
  "Respiratory disorders" = "#e7298a",
  "General disorders" = "#66a61e",
  "Infections and infestations" = "#e6ab02"
)

#--------------------------------------------------
# 6. Hover text (clean)
#--------------------------------------------------

ae_summary <- ae_summary %>%
  mutate(
    hover_text = paste0(
      "<b>SOC:</b> ", AEBODSYS,
      "<br><b>AE:</b> ", AEDECOD,
      "<br>Log2FC: ", round(log2FC, 2),
      "<br>-log10(p): ", round(negLog10P, 2),
      "<br>p-value: ", signif(p_value, 3)
    )
  )

sig_data <- ae_summary %>% filter(p_flag)
nonsig_data <- ae_summary %>% filter(!p_flag)

#--------------------------------------------------
# 7. Interactive plot
#--------------------------------------------------

fig <- fig %>%
  layout(
    title = "Volcano Plot of Adverse Events by SOC",
    xaxis = list(title = "Log2 Fold Change"),
    yaxis = list(title = "-log10(p-value)"),
    
    shapes = list(
      
      # Vertical thresholds (FC)
      list(type = "line",
           x0 = -fc_thresh, x1 = -fc_thresh,
           y0 = 0, y1 = max(ae_summary$negLog10P),
           line = list(dash = "dash", color = "black")),
      
      list(type = "line",
           x0 = fc_thresh, x1 = fc_thresh,
           y0 = 0, y1 = max(ae_summary$negLog10P),
           line = list(dash = "dash", color = "black")),
      
      # ✅ Horizontal threshold (p = 0.05)
      list(type = "line",
           x0 = min(ae_summary$log2FC),
           x1 = max(ae_summary$log2FC),
           y0 = -log10(p_thresh),
           y1 = -log10(p_thresh),
           line = list(dash = "dash", color = "black"))
    )
  )

fig
#--------------------------------------------------
# 8. Static plot
#--------------------------------------------------

gg <- ggplot(ae_summary, aes(x = log2FC, y = negLog10P)) +
  
  geom_point(
    data = subset(ae_summary, p_value > 0.05),
    color = "grey70",
    alpha = 0.6
  ) +
  
  geom_point(
    data = subset(ae_summary, p_value <= 0.05),
    aes(color = AEBODSYS),
    size = 2.5
  ) +
  
  scale_color_manual(values = soc_colors) +
  
  geom_vline(xintercept = c(-fc_thresh, fc_thresh), linetype = "dashed") +
  geom_hline(yintercept = -log10(p_thresh), linetype = "dashed") +
  
  labs(
    title = "Volcano Plot of Adverse Events by SOC",
    x = "Log2 Fold Change",        # ✅ Updated
    y = "-log10(p-value)",         # ✅ Updated
    color = "SOC"
  ) +
  
  theme_minimal()
gg
#--------------------------------------------------
# 9. EXPORT (USES out_dir)
#--------------------------------------------------

# HTML
saveWidget(
  fig,
  file = file.path(out_dir, "Volcano_Plot_int_AE2.html"),
  selfcontained = TRUE
)

# PNG
ggsave(
  filename = file.path(out_dir, "Volcano_Plot_static_AE2.png"),
  plot = gg,
  width = 10,
  height = 6,
  dpi = 300
)

# PDF
# ggsave(
#   filename = file.path(out_dir, "Volcano_Plot_AE.pdf"),
#   plot = gg,
#   width = 10,
#   height = 6
# )

# TIFF (optional, publication)
# ggsave(
#   filename = file.path(out_dir, "Volcano_Plot_AE.tiff"),
#   plot = gg,
#   width = 10,
#   height = 6,
#   dpi = 600
# )