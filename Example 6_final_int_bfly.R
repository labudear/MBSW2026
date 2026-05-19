#install.packages("r2rtf")
#install.packages("rtf")
#install.packages("ggiraph")
#install.packages("htmlwidgets")
#install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(r2rtf)
library(rtf)
library(rlang)      # %||%
library(grid)
library(patchwork)
library(ggiraph)
library(htmlwidgets)
library(tidyr)

## -----------------------------------------------------------------------------
## Title block content
## -----------------------------------------------------------------------------
report_title <- "Figure 14.02.03.01: Summary of TEAEs by Treatment and Toxicity Grade"
sponsor_name <- "Acme Pharma, Inc."
protocol_num <- "Protocol ABC-1234"
CUTOFF_DATE  <- as.Date("2026-01-15")

# ------------------------------------------------------------------------------
#  Load ADSL-like dataset (REPLACE THIS with your actual import)
#  Needs: USUBJID, TRT01A
# ------------------------------------------------------------------------------
adsl <- data.frame(
  USUBJID = 1:410,
  TRT01A  = c(rep("A", 200), rep("B", 210))
)

# Extract N from ADSL (base-R setNames version; no tibble::deframe needed)
N_by_trt <- adsl %>%
  distinct(USUBJID, TRT01A) %>%
  count(TRT01A, name = "n") %>%
  { setNames(.$n, .$TRT01A) }

getN <- function(trt) N_by_trt[[trt]]

# ----------------------------
# AE dataset (replace with yours)
# ----------------------------
df <- data.frame(
  Term = rep(c("Nausea", "Fatigue", "Headache"), each = 6),
  Treatment = rep(c("A","A","A","B","B","B"), times = 3),
  Grade = rep(c("G1","G2","G3"), times = 6),
  Count = c(30,20,10, 25,15,5,
            40,25,15, 35,20,10,
            50,35,25, 45,30,20)
)

# ===========================
# Parameters
# ===========================
grades              <- c("G1","G2","G3")
treat_right         <- "A"
treat_left          <- "B"
gap_center          <- 4
bar_half_height     <- 0.49
segment_border_size <- 0
segment_border_col  <- "white"
fill_cols           <- c("G1"="#9ecae1","G2"="#4292c6","G3"="#08519c")

# ===========================
# Prepare data
# ===========================
df <- df %>%
  mutate(
    Grade = factor(Grade, levels = grades),
    TermF = factor(Term, levels = unique(Term))
  )

term_pos <- df %>%
  distinct(TermF, Term) %>%
  mutate(
    y  = as.numeric(TermF),
    y0 = y - bar_half_height,
    y1 = y + bar_half_height
  )

df <- df %>% left_join(term_pos, by = c("TermF","Term"))

# Percent within Term × Treatment + labels
df <- df %>%
  group_by(Term, Treatment) %>%
  mutate(
    Percent   = 100 * Count / sum(Count),
    pct_lab   = sprintf("%.1f%%", Percent),
    count_lab = Count
  ) %>%
  ungroup()

# ===========================
# Compute stacks per Term × Treatment
# ===========================
df_stack <- df %>%
  arrange(TermF, Treatment, Grade) %>%
  group_by(TermF, Treatment) %>%
  mutate(
    left  = cumsum(lag(Count, default = 0)),
    right = cumsum(Count)
  ) %>%
  ungroup()

dfA <- df_stack %>%
  filter(Treatment == treat_right) %>%
  mutate(
    xmin = gap_center/2 + left,
    xmax = gap_center/2 + right
  )

dfB <- df_stack %>%
  filter(Treatment == treat_left) %>%
  mutate(
    xmin = -(gap_center/2 + right),
    xmax = -(gap_center/2 + left)
  )

df_plot <- bind_rows(dfA, dfB) %>%
  mutate(
    xmid = (xmin + xmax)/2,
    ymid = (y0 + y1)/2
  )

# ===========================
# Add per-term stats (Any-grade risk, RD, Fisher p, -log10 p)
# ===========================

term_stats <- df %>%
  group_by(Term, Treatment) %>%
  summarise(events = sum(Count), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = events,
    values_fill = 0
  ) %>%
  mutate(
    N_A = as.numeric(N_by_trt["A"]),
    N_B = as.numeric(N_by_trt["B"]),
    risk_A = A / N_A,
    risk_B = B / N_B,
    rd = risk_A - risk_B,
    
    # ---- FEXACT FIX ----
    p_fisher = {
      a_no <- pmax(N_A - A, 0)
      b_no <- pmax(N_B - B, 0)
      m <- matrix(c(A, a_no, B, b_no), nrow = 2, byrow = TRUE)
      
      # Simulated Fisher test (NO MORE ERRORS)
      fisher.test(m, simulate.p.value = TRUE, B = 200000)$p.value
    },
    
    neglog10_p = -log10(p_fisher)
  )


df_plot <- df_plot %>%
  left_join(term_stats, by = "Term") %>%
  mutate(
    tooltip = paste0(
      "<b>", Term, "</b>",
      "<br>Grade: ", Grade,
      "<br>Treatment: ", Treatment,
      "<br>Count: ", count_lab,
      "<br>Percent: ", sprintf('%.1f%%', Percent),
      "<hr>",
      "<b>Any-grade summary</b>",
      "<br>Risk A: ", sprintf('%.1f%%', 100*risk_A), " (N=", N_A, ")",
      "<br>Risk B: ", sprintf('%.1f%%', 100*risk_B), " (N=", N_B, ")",
      "<br><b>RD (A − B): ", sprintf('%.1f%%', 100*rd), "</b>",
      "<br>Fisher p-value: ", format.pval(p_fisher, digits = 3),
      "<br>−log10(p): ", sprintf('%.2f', neglog10_p)
    ),
    data_id = paste(Term, Treatment, Grade, sep = "|")
  )

# ===========================
# Build the butterfly plot object (static)
# ===========================
p <- ggplot() +
  geom_rect(
    data = df_plot,
    aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = Grade),
    colour    = if (segment_border_size > 0) segment_border_col else NA,
    linewidth = segment_border_size
  ) +
  geom_text(
    data = df_plot,
    aes(x = xmid, y = ymid, label = count_lab),
    size = 3, color = "white"
  ) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.6) +
  scale_y_continuous(
    breaks = term_pos$y,
    labels = term_pos$Term,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_x_continuous(
    breaks = {
      max_abs <- max(abs(df_plot$xmax))
      data_breaks <- (seq(0, 100, by = 5) / 100) * max_abs
      c(-rev(data_breaks), data_breaks)
    },
    labels = function(x) {
      max_abs <- max(abs(df_plot$xmax))
      round(abs(x) / max_abs * 100, 0)
    },
    limits = {
      max_abs <- max(abs(df_plot$xmax))
      c(-max_abs, max_abs)
    },
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(values = fill_cols) +
  annotate("text",
           x = -max(df_plot$xmax) * 0.9,
           y = min(term_pos$y0) - 0.3,
           label = paste0("Treatment ", treat_left, " (N=", getN(treat_left), ")"),
           hjust = 0, size = 3) +
  annotate("text",
           x =  max(df_plot$xmax) * 0.9,
           y = min(term_pos$y0) - 0.3,
           label = paste0("Treatment ", treat_right, " (N=", getN(treat_right), ")"),
           hjust = 1, size = 3) +
  coord_cartesian(clip = "off") +
  labs(
    title = "",
    x = "Percent",
    y = "Adverse Event",
    fill = "Grade"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 6),
    axis.text.x        = element_text(size = 6),
    axis.title.x       = element_text(size = 8),
    axis.title.y       = element_text(size = 8),
    legend.title       = element_text(size = 8),
    legend.text        = element_text(size = 7),
    plot.title         = element_text(size = 10, face = "bold"),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    plot.margin        = margin(t = 10, r = 10, b = 30, l = 10)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             keyheight = unit(6, "pt"),
                             keywidth  = unit(10, "pt")))

# ==============================================================================
# Output directories
# ==============================================================================
out_dir <- "/mnt/drives/U/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ===========================
# Save static PNG
# ===========================
plot_png <- file.path(out_dir, "butterfly_plot.png")
ggsave(
  filename  = plot_png,
  plot      = p,
  width     = 6.3,
  height    = 4.5,
  dpi       = 300
)

stopifnot(file.exists(plot_png), file.info(plot_png)$size > 0)

# ===========================
# Interactive HTML with hover tooltips
# ===========================
p_html <- ggplot() +
  ggiraph::geom_rect_interactive(
    data = df_plot,
    aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1,
        fill = Grade, tooltip = tooltip, data_id = data_id),
    colour    = if (segment_border_size > 0) segment_border_col else NA,
    linewidth = segment_border_size
  ) +
  geom_text(
    data = df_plot,
    aes(x = xmid, y = ymid, label = count_lab),
    size = 3, color = "white"
  ) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.6) +
  scale_y_continuous(
    breaks = term_pos$y,
    labels = term_pos$Term
  ) +
  scale_x_continuous(
    breaks = {
      max_abs <- max(abs(df_plot$xmax))
      data_breaks <- (seq(0, 100, by = 5) / 100) * max_abs
      c(-rev(data_breaks), data_breaks)
    },
    labels = function(x) {
      max_abs <- max(abs(df_plot$xmax))
      round(abs(x) / max_abs * 100, 0)
    },
    limits = {
      max_abs <- max(abs(df_plot$xmax))
      c(-max_abs, max_abs)
    }
  ) +
  scale_fill_manual(values = fill_cols) +
  coord_cartesian(clip = "off") +
  labs(x = "Percent", y = "Adverse Event", fill = "Grade") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),   # <<< REDUCED TICK SIZE FOR HTML
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 6),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.y       = element_blank(),
    legend.position    = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             keyheight = unit(6, "pt"),
                             keywidth  = unit(10, "pt")))

gir <- girafe(
  ggobj = p_html,
  width_svg  = 6.3,
  height_svg = 4.5
)

gir <- girafe_options(
  gir,
  opts_hover(css = "fill-opacity:0.9; stroke:black; stroke-width:0.3px;"),
  opts_tooltip(css = "
    background-color: rgba(255,255,255,0.98);
    color: #222;
    border: 1px solid #aaa;
    border-radius: 4px;
    padding: 6px;
    font-size: 10px;
    line-height: 1.25;
    max-width: 280px;
  ")
)

html_file <- file.path(out_dir, "butterfly_plot_interact.html")
htmlwidgets::saveWidget(gir, file = html_file, selfcontained = TRUE)

message("Interactive HTML written: ", normalizePath(html_file))

# ===========================
# RTF (your simplified pipeline; keep your advanced header/footer if needed)
# ===========================
rtf_path <- file.path(out_dir, "bfly_plot_landscape.rtf")

rtf_read_figure(plot_png) %>%
  rtf_page(orientation = "landscape") %>%
  rtf_title(
    title    = report_title,
    subtitle = c(
      paste0("Sponsor: ", sponsor_name),
      paste0("Protocol: ", protocol_num),
      paste0("Data cutoff: ", format(CUTOFF_DATE, "%Y-%m-%d"))
    )
  ) %>%
  rtf_figure(fig_width = 6.5, fig_height = 4.5) %>%
  rtf_encode(doc_type = "figure") %>%
  write_rtf(file = rtf_path)

message("RTF written to: ", rtf_path)