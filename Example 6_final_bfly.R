#install.packages("r2rtf")
# install.packages("rtf") if needed

library(dplyr)
library(ggplot2)
library(r2rtf)
library(rtf)     
library(rlang)   # `%||%`
library(grid)
library(patchwork)
## -----------------------------------------------------------------------------
## [EDIT HERE] Title block content
## -----------------------------------------------------------------------------
report_title <- "Figure 14.02.03.01: Summary of TEAEs by Treatment and Toxicity Grade"
space_title  <- ""
sponsor_name <- "Acme Pharma, Inc."
protocol_num <- "Protocol ABC-1234"

## -----------------------------------------------------------------------------
## [EDIT HERE] Data cutoff date "macro"
## -----------------------------------------------------------------------------
CUTOFF_DATE <- as.Date("2026-01-15")

# ----------------------------
# Example data (replace with yours)
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
    pct_lab   = sprintf("%.1f%%", Percent),  # keep if you want to show later
    count_lab = Count                        # counts inside bars (requested)
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
# Build the butterfly plot object
# ===========================
# NOTE: Tighten plot margins to avoid clipping in RTF
p <- ggplot() +
  geom_rect(
    data = df_plot,
    aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = Grade),
    colour    = if (segment_border_size > 0) segment_border_col else NA,
    linewidth = segment_border_size
  ) +
  # Counts inside bars
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
  # X-axis: 0..100 by 5 (no '%' symbol), symmetric for left/right
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
  # Treatment N annotations from external pop dataset
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
    axis.text.y       = element_text(size = 6),
    axis.text.x        = element_text(size = 6),         # tick labels
    #    axis.ticks.x       = element_line(linewidth = 0.3),  # tick marks
    
    # ↓ New: label/title/legend sizing
    axis.title.x       = element_text(size = 8),
    axis.title.y       = element_text(size = 8),
    legend.title       = element_text(size = 8),
    legend.text        = element_text(size = 7),
    plot.title         = element_text(size = 10, face = "bold"),
    
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    
    # Keep margins tight for RTF (avoid huge padding that causes clipping)
    plot.margin        = margin(t = 10, r = 10, b = 30, l = 10)
  ) +
  #  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  #For very tight layouts, you can also reduce legend key sizes:
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, keyheight = unit(6, "pt"), keywidth = unit(10, "pt")))
# ===========================


##### Save the png file and rtf file ###############

# --- 2) Save the plot as a high-res PNG (or EMF on Windows for vector) ---
#  1) Point directly to your intended output directory
out_dir <- "/mnt/drives/U/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"

# 2) Make sure the directory exists
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# Left footer: Production date (today)
prod_date <- format(Sys.Date(), "%d-%b-%Y")

# Center footer: Extraction date (EDIT to your actual extraction date)
# If you have it as a Date object, format it; if as string, keep as-is
extraction_date <- format(Sys.Date(), "%d-%b-%Y")  # <-- replace with your real extraction date


## Program name
pgm_name <- "Example 4_final.R"   # <-- edit to your script name

# ===========================
# Save plot to PNG at text width
# ===========================
# For US Letter portrait with 1" margins, text width = 6.5 in.
# Save a touch smaller (safety factor) to avoid touching edges in Word.
page_width   <- 8.5
margin_left  <- 1.0
margin_right <- 1.0
text_width   <- page_width - margin_left - margin_right   # 6.5 in
png_width_in <- text_width * 0.98                         # 98% of text width (center nicely)
png_height_in <- 4.5

plot_png <- file.path(out_dir, "butterfly_plot.png")


# Choose a robust PNG device function
dev_fun <- if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png              # ← pass the function, not "ragg_png"
} else {
  grDevices::png             # fallback to base PNG device
}

ggsave(
  filename  = plot_png,
  plot      = p,
  width     = png_width_in,  # your computed width in inches
  height    = png_height_in, # your computed height in inches
  units     = "in",
  dpi       = 300,
  device    = dev_fun,       # <— the function, not a string
  limitsize = FALSE
)

stopifnot(file.exists(plot_png), file.info(plot_png)$size > 0)



## -----------------------------------------------------------------------------
## 8) Export data used in the plot as CSV files for qc 
## -----------------------------------------------------------------------------
qc_out_dir <- "/mnt/drives/U/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Qc Doc/IDMCxy/W_20250129/DUMMY/Outputs/Figures/RPlots"

# Make sure the directory exists - if not, create
dir.create(qc_out_dir, showWarnings = FALSE, recursive = TRUE)

# a) Summary used by the plot 
readr::write_csv(df_plot,      file.path(qc_out_dir, "bfly_ae_summary.csv"))

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
  left   = "SOC Butterfly Report",
  center = "RR vs -log10(p)",
  right  = format(Sys.Date()),
  text_width_in = text_width_in,
  border_bottom = TRUE,
  border_w_pt   = 0.75
)

left_footer <- paste0(
  "Example 6_final_bfly.R",
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
## 12) Assemble LANDSCAPE RTF (SINGLE rtf_title call preserves title block)
## -----------------------------------------------------------------------------
rtf_path <- file.path(out_dir, "bfly_plot_landscape.rtf")

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
    "Note 1: Footnote 1 goes here",
    "Note 2: N is the total number of subjects.",
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
