
## =============================================================================
## Volcano Plot from Simulated Patient-level Data (20 SOCs)
## Landscape RTF with:
##   - One-line HEADER (L/C/R) starting at page margins + bottom border (full width)
##   - Left-justified TITLE block above the plot (Title, Sponsor, Protocol)
##   - Solid horizontal rules in BODY:
##       • immediately after header (optional, included)
##       • immediately before the plot (required)
##   - One-line FOOTER (L/C/R) at page margins + top border (full width)
##   - Plot colored & shaped by SOC; top-left inset with n/N
## =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(readr)
  library(tibble)
  library(r2rtf)
  library(rlang)   # `%||%`
  library(grid)
  library(patchwork)
})

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

set.seed(1234)

## -----------------------------------------------------------------------------
## [EDIT HERE] Title block content
## -----------------------------------------------------------------------------
report_title <- "Figure 14.02.04.01: Volcano Plot for all AEs at SOC Level"
space_title  <- ""
sponsor_name <- "Acme Pharma, Inc."
protocol_num <- "Protocol ABC-1234"

## -----------------------------------------------------------------------------
## [EDIT HERE] Data cutoff date "macro"
## -----------------------------------------------------------------------------
CUTOFF_DATE <- as.Date("2026-01-15")

## -----------------------------------------------------------------------------
## 1) Simulation parameters
## -----------------------------------------------------------------------------
n_per_arm <- 600
alpha_sig <- 0.05
n_soc     <- 20

soc_names <- c(
  "Cardiac disorders", "Gastrointestinal disorders", "Nervous system disorders",
  "Skin and subcutaneous tissue disorders", "Infections and infestations",
  "Respiratory disorders", "Vascular disorders", "Renal and urinary disorders",
  "Hepatobiliary disorders", "Musculoskeletal disorders", "Eye disorders",
  "Ear and labyrinth disorders", "Endocrine disorders", "Metabolism disorders",
  "Immune system disorders", "Blood and lymphatic disorders",
  "Reproductive system disorders", "Psychiatric disorders",
  "General disorders", "Investigations"
)[seq_len(n_soc)]

## -----------------------------------------------------------------------------
## 2) Simulate patient-level events per SOC and arm
## -----------------------------------------------------------------------------
adsl <- tibble(
  id  = 1:(2 * n_per_arm),
  arm = rep(c("Control", "Treatment"), each = n_per_arm)
)

p_ctrl <- runif(n_soc, min = 0.02, max = 0.20)
log_rr_true <- rnorm(n_soc, mean = 0, sd = 0.40)
rr_true     <- exp(log_rr_true)
p_trt <- pmin(p_ctrl * rr_true, 0.95)

event_df <- adsl %>%
  tidyr::crossing(SOC = soc_names) %>%
  mutate(
    SOC = factor(SOC, levels = soc_names),
    p_event = ifelse(arm == "Control",
                     p_ctrl[match(SOC, soc_names)],
                     p_trt[match(SOC, soc_names)])
  ) %>%
  mutate(event = rbinom(n(), size = 1, prob = p_event))

## -----------------------------------------------------------------------------
## 3) Aggregate counts, Fisher p-values and RR (Haldane–Anscombe)
## -----------------------------------------------------------------------------
counts <- event_df %>%
  group_by(SOC, arm) %>%
  summarise(
    events     = sum(event),
    non_events = n() - events,
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = arm,
    values_from = c(events, non_events),
    values_fill = 0
  ) %>%
  mutate(
    a = events_Treatment, b = non_events_Treatment,
    c = events_Control,   d = non_events_Control,
    n_t = a + b, n_c = c + d,
    risk_trt  = (a + 0.5) / (n_t + 1),
    risk_ctrl = (c + 0.5) / (n_c + 1),
    RR        = risk_trt / risk_ctrl
  )

fisher_p <- function(a, b, c, d) {
  mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
  stats::fisher.test(mat, alternative = "two.sided")$p.value
}

df_plot <- counts %>%
  rowwise() %>% mutate(pval = fisher_p(a, b, c, d)) %>% ungroup() %>%
  mutate(neglog10p = -log10(pval), sig = pval < alpha_sig)

## -----------------------------------------------------------------------------
## 4) ADSL-based n/N per arm + inset (top-left, no border)
## -----------------------------------------------------------------------------
subject_any_event <- event_df %>%
  group_by(id, arm) %>%
  summarise(any_event = as.integer(any(event == 1)), .groups = "drop")

adsl_counts <- adsl %>%
  left_join(subject_any_event, by = c("id","arm")) %>%
  mutate(any_event = ifelse(is.na(any_event), 0L, any_event)) %>%
  group_by(arm) %>% summarise(N = n(), n = sum(any_event), .groups = "drop")

N_ctl <- adsl_counts$N[adsl_counts$arm == "Control"];    n_ctl <- adsl_counts$n[adsl_counts$arm == "Control"]
N_trt <- adsl_counts$N[adsl_counts$arm == "Treatment"];  n_trt <- adsl_counts$n[adsl_counts$arm == "Treatment"]

line_trt <- sprintf("Treatment: n/N = %d/%d", n_trt, N_trt)
line_ctl <- sprintf("Placebo: n/N = %d/%d",   n_ctl, N_ctl)

p_inset <- ggplot() +
  annotate("text", x = 0, y = 0.95, label = line_trt, hjust = 0, vjust = 1, size = 3.5) +
  annotate("text", x = 0, y = 0.60, label = line_ctl, hjust = 0, vjust = 1, size = 3.5) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "white", linewidth = 0))

## -----------------------------------------------------------------------------
## 5) Legend mapping (alphabetical SOC), colors & shapes
## -----------------------------------------------------------------------------
df_plot$SOC <- factor(df_plot$SOC, levels = sort(unique(as.character(df_plot$SOC))))
soc_levels <- levels(df_plot$SOC); n_soc <- length(soc_levels)

pal <- scales::hue_pal(l = 65, c = 100)(n_soc); names(pal) <- soc_levels
shape_values <- setNames(0:19, soc_levels)

## -----------------------------------------------------------------------------
## 6) Volcano plot (RR vs -log10(p)) with styling + inset
## -----------------------------------------------------------------------------
p_ticks  <- c(1, 0.1, 0.05, 0.01, 0.001, 0.0001)
y_breaks <- -log10(p_ticks)

x_min <- max(0, min(df_plot$RR, na.rm = TRUE) * 0.9)
x_max <- max(df_plot$RR, na.rm = TRUE) * 1.1

volcano <- ggplot(df_plot, aes(x = RR, y = neglog10p)) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "gray40") +
  geom_hline(yintercept = -log10(alpha_sig), linetype = "dotted", color = "gray40") +
  geom_point(aes(color = SOC, shape = SOC), size = 3, alpha = 0.9, stroke = 1) +
  ggrepel::geom_text_repel(
    data  = dplyr::filter(df_plot, sig),
    aes(label = SOC),
    size = 2.0, color = "black",
    max.overlaps = Inf, min.segment.length = 0,
    box.padding = 0.15, point.padding = 0.2,
    seed = 1234
  ) +
  scale_x_continuous(name = "Risk Ratio (RR)", limits = c(x_min, x_max)) +
  scale_y_continuous(
    name   = "Fisher’s exact p-value",
    breaks = y_breaks,
    labels = format(p_ticks, scientific = FALSE, trim = TRUE)
  ) +
  scale_color_manual(values = pal, name = "SOC") +
  scale_shape_manual(values = shape_values, name = "SOC") +
  theme_minimal(base_size = 8) +
  theme(
    legend.position   = "right",
    legend.direction  = "vertical",
    legend.key.height = unit(0.52, "lines"),
    legend.key.width  = unit(0.85, "lines"),
    legend.spacing.y  = unit(0.28, "lines"),
    legend.box.margin = margin(t = 3, r = 6, b = 3, l = 6),
    legend.text       = element_text(size = 7),
    legend.title      = element_text(size = 7, face = "bold"),
    axis.text.x       = element_text(size = 4),
    axis.text.y       = element_text(size = 4),
    panel.grid.minor  = element_blank()
  )

volcano_final <- volcano +
  inset_element(
    p_inset,
    left   = -0.10, right = 0.45, top = 0.98, bottom = 0.85,
    align_to = "panel", clip = TRUE
  )

## -----------------------------------------------------------------------------
## 7) Save the plot PNG
## -----------------------------------------------------------------------------
out_dir <- "/mnt/drives/U/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
plot_png <- file.path(out_dir, "volcano_plot.png")
ggsave(filename = plot_png, plot = volcano_final, width = 8.0, height = 3.8, dpi = 300)


## -----------------------------------------------------------------------------
## 8) Export data used in the plot as CSV files for qc 
## -----------------------------------------------------------------------------
qc_out_dir <- "/mnt/drives/U/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Qc Doc/IDMCxy/W_20250129/DUMMY/Outputs/Figures/RPlots"

# Make sure the directory exists - if not, create
dir.create(qc_out_dir, showWarnings = FALSE, recursive = TRUE)

# 1) Patient-level (long): id, arm, SOC, p_event, event
# readr::write_csv(event_df, file.path(qc_out_dir, "patient_level_events.csv"))

# 2) 2x2 counts and derived metrics per SOC
# readr::write_csv(counts,   file.path(qc_out_dir, "soc_counts_rr.csv"))

# 3) Minimal plot dataset (only essential columns)
# df_plot_min <- df_plot %>% dplyr::select(SOC, RR, pval, neglog10p, sig)
# readr::write_csv(df_plot_min,  file.path(qc_out_dir, "volcano_soc_summary_min.csv"))

# 4) Summary used by the plot (includes RR, pval, -log10p, significance)
readr::write_csv(df_plot,      file.path(qc_out_dir, "volcano_soc_summary.csv"))


## -----------------------------------------------------------------------------
## 9) Page setup
## -----------------------------------------------------------------------------
page_orientation <- "landscape"
margins <- c(1.0, 1.0, 1.0, 1.0, 0.5, 0.5) # left, right, top, bottom, header, footer (in)
page_width_in <- ifelse(page_orientation == "portrait", 8.5, 11.0)
text_width_in <- page_width_in - margins[1] - margins[2]
col_width_in  <- text_width_in

## -----------------------------------------------------------------------------
## 10) Header/Footer row builder with optional row borders
## -----------------------------------------------------------------------------
make_threecol_row <- function(left, center, right, text_width_in,
                              border_top = FALSE,
                              border_bottom = FALSE,
                              border_w_pt = 0.75) {
  tw  <- 1220L
  cx1 <- as.integer(text_width_in * tw / 3)
  cx2 <- as.integer(text_width_in * tw * 2 / 3)
  cx3 <- as.integer(text_width_in * tw)
  brw <- as.integer(border_w_pt * 20)
  row_borders <- paste0(
    if (border_top)    paste0("\\trbrdrt\\brdrs\\brdrw", brw) else "",
    if (border_bottom) paste0("\\trbrdrb\\brdrs\\brdrw", brw) else ""
  )
  paste0(
    "{\\trowd\\trleft0\\trgaph0", row_borders,
    "\\cellx", cx1, " \\pard\\intbl\\li0\\ri0\\ql ", left, " \\cell ",
    "\\cellx", cx2, " \\pard\\intbl\\li0\\ri0\\qc ", center, " \\cell ",
    "\\cellx", cx3, " \\pard\\intbl\\li0\\ri0\\qr ", right, " \\cell ",
    "\\row}"
  )
}

## -----------------------------------------------------------------------------
## 11) Build one-line HEADER & FOOTER rows (with borders)
## -----------------------------------------------------------------------------
header_one_line <- make_threecol_row(
  left   = "SOC Volcano Report",
  center = "RR vs -log10(p)",
  right  = format(Sys.Date()),
  text_width_in = text_width_in,
  border_bottom = TRUE,
  border_w_pt   = 0.75
)

left_footer <- paste0(
  "Example 6_final_volc.R",
  " \\line ",
  "Production date: ", format(Sys.Date(), "%Y-%m-%d")
)
center_footer <- paste0("Data cutoff: ", format(CUTOFF_DATE, "%Y-%m-%d"))
right_footer <- "page \\chpgn of {\\field{\\*\\fldinst NUMPAGES}}"

footer_one_line <- make_threecol_row(
  left   = left_footer,
  center = center_footer,
  right  = right_footer,
  text_width_in = text_width_in,
  border_top    = TRUE,
  border_w_pt   = 0.75
)

## -----------------------------------------------------------------------------
## 12) Helper: raw RTF paragraph that renders a solid horizontal rule
## -----------------------------------------------------------------------------
make_hrule_rtf <- function(width_pt = 0.15, side = c("top","bottom"),
                           space_before = 0L, space_after = 0L) {
  side <- match.arg(side)
  w_twips <- as.integer(width_pt * 20)       # 1 pt = 20 twips
  br <- if (side == "top") "\\brdrt" else "\\brdrb"
  paste0("{\\pard\\plain\\s0\\li0\\ri0\\sb", space_before, "\\sa", space_after,
         br, "\\brdrs\\brdrw", w_twips, "\\par}")
}

## -----------------------------------------------------------------------------
## 13) Assemble LANDSCAPE RTF (SINGLE rtf_title call preserves title block)
## -----------------------------------------------------------------------------
rtf_path <- file.path(out_dir, "volcano_plot_landscape.rtf")

rtf_read_figure(plot_png) %>%
  rtf_page(orientation = page_orientation, margin = margins, col_width = col_width_in) %>%
  # Header row with bottom border (margin-to-margin)
#  rtf_page_header(text = header_one_line, text_convert = FALSE) %>% # Turn off the header
  # SINGLE title/subtitle block with both rules + title lines
  rtf_title(
    title = report_title,
    subtitle = c(
      make_hrule_rtf(width_pt = 0.15, side = "bottom", space_before = 0L, space_after = 0L),  # after header
#      paste0("", space_title),
      paste0("Sponsor: ", sponsor_name),
      paste0("Protocol: ", protocol_num),
      make_hrule_rtf(width_pt = 0.15, side = "bottom", space_before = 0L, space_after = 0L)   # before plot
    ),
    text_justification    = "l",
    text_indent_reference = "page_margin",
    text_convert          = FALSE,   # keep raw RTF for the rules
    text_space_before     = 0,
    text_space_after      = 0
  ) %>%
  # Footnotes under the figure
  rtf_footnote(c(
    make_hrule_rtf(width_pt = 0.15, side = "bottom", space_before = 0L, space_after = 0L),  # after footer
    "Note 1: The y-axis ticks displays p-values that were plotted as -log10(p).",
    "Note 2: n is the number of events and N is the total number of subjects.",
    "Note 3: XXXX More footnote if needed XXXX.",
    ""
  )) %>%
  # Footer row with top border (margin-to-margin)
  rtf_page_footer(text = footer_one_line, text_convert = FALSE) %>%
  # Figure itself
  rtf_figure(fig_width = col_width_in, fig_height = 3.8) %>%
  rtf_encode(doc_type = "figure") %>%
  write_rtf(file = rtf_path)

message("RTF written to: ", rtf_path)
