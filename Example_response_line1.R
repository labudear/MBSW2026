## Housekeeping
graphics.off()
rm(list=ls())
closeAllConnections()

# Packages and library
# install.packages(c("ggplot2", "plotly", "dplyr", "tidyr", "haven"))

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(haven)

# ----------------------------------
# Simulated Data
# ----------------------------------

# Standard reading of a .sas7bdat file
adsl <- read_sas("U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Datasets/IDMCxy/W_20250129/DUMMY/adsl2.sas7bdat")

# Reading with a catalog file for formats/labels
# my_data <- read_sas("U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Datasets/IDMCxy/W_20250129/DUMMY/adsl2.sas7bdat", catalog_file = "U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Programs/IDMCxx/Format/W_20250129/formats.sas7bcat")

# Reading response data with selected variables
adqs <- read_sas("U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Datasets/IDMCxy/W_20250129/DUMMY/adqs.sas7bdat", col_select = c(USUBJID, SUBJID, TRT01A, VISIT, VISITNUM, PARCAT1, PARAM, PARAMCD, BASE, ABLFL, qsorres, ADY))
# Apply a selected condition (e.g., PARAMCD="TOTAL")
adqs1 <- adqs %>%
  filter(PARAMCD=='TOTAL')

# Reading lab data 
adlb_long <- read_sas("U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Datasets/IDMCxy/W_20250129/DUMMY/adlb.sas7bdat", col_select = c(USUBJID, SUBJID, TRT01A, VISIT, VISITNUM, TRTSDT, ADT, PARCAT1, PARAM, PARAMCD, BASE, ABLFL, AVAL, CHG))
# Apply a selected condition (e.g., PARAMCD="TOTAL")
adlb0 <- adlb_long %>%
  filter(  PARAMCD=='HGB' |
           PARAMCD=='BASO' |
           PARAMCD=='NEUT' |
           PARAMCD=='PLAT') 

adlb1 <- adlb0 %>%
  mutate(ADY = ifelse(ADT >= TRTSDT,
                      as.numeric(ADT - TRTSDT) + 1,
                      as.numeric(ADT - TRTSDT)))

# convert lab data to wide format

labs_wide <- adlb1 %>%
  select(SUBJID, USUBJID, TRT01A, VISIT, VISITNUM, PARAMCD, AVAL, ADY) %>%
  pivot_wider(
    names_from = PARAMCD,
    values_from = AVAL
  )



# Reading AE data 
adae <- read_sas("U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Datasets/IDMCxy/W_20250129/DUMMY/adae.sas7bdat", col_select = c(USUBJID, AESOC, AEDECOD, AETERM, AESTDY, AEENDY))

# Ensure AENDY exists
adae <- adae %>%
  mutate(
    AEENDY = ifelse(is.na(AEENDY), AESTDY, AEENDY)
  )


adae_clean <- adae %>%
  mutate(
    # Convert to numeric (important)
    AESTDY = as.numeric(AESTDY),
    AEENDY = as.numeric(AEENDY),
    
    # Handle missing end day
    AEENDY = ifelse(is.na(AEENDY), AESTDY, AEENDY)
  ) %>%
  # Remove invalid rows
  filter(
    !is.na(AESTDY),
    !is.na(AEENDY),
    AESTDY <= AEENDY
  )

# ----------------------------------
# Expand AE across duration (KEY STEP)
# ----------------------------------


# ae_expanded <- adae_clean %>%
#   mutate(ADY = purrr::map2(AESTDY, AEENDY, seq)) %>%
#   unnest(ADY) %>%
#   select(USUBJID, ADY, AEDECOD)
# 
# 
# ae_expanded <- adae_clean %>%
#   rowwise() %>%
#   do(data.frame(
#     USUBJID = .$USUBJID,
#     ADY = seq(.$AESTDY, .$AEENDY),
#     AEDECOD = .$AEDECOD
#   ))


ae_expanded <- adae_clean %>%
  rowwise() %>%
  mutate(ADY = list(seq(AESTDY, AEENDY))) %>%
  unnest(ADY) %>%
  ungroup() %>%
  select(USUBJID, ADY, AEDECOD)

# ----------------------------------
# 5. Collapse AE per day
# ----------------------------------

ae_by_day <- ae_expanded %>%
  group_by(USUBJID, ADY) %>%
  summarise(
    AE = paste(unique(AEDECOD), collapse = ", "),
    .groups = "drop"
  )



# ----------------------------------
# 6. Merge everything
# ----------------------------------

plot_data0 <- adlb1 %>%
  left_join(labs_wide, by = c("USUBJID", "ADY")) %>%
  left_join(ae_by_day, by = c("USUBJID", "ADY")) %>%
  left_join(adqs1, by = c("USUBJID", "ADY"))

# Keep rows where column 'x' is not NA
plot_data <- plot_data0[!is.na(plot_data0$qsorres), ]

# ----------------------------------
# 7. Clean missing values
# ----------------------------------

plot_data <- plot_data %>%
  mutate(
    BASO = ifelse(is.na(BASO), "No Lab", BASO),
    NEUT = ifelse(is.na(NEUT), "No Lab", NEUT),
    PLAT = ifelse(is.na(PLAT), "No Lab", PLAT),
    HGB = ifelse(is.na(HGB), "No Lab", HGB),
    AE  = ifelse(is.na(AE), "No AE", AE)
  )

# ----------------------------------
# 8. Create tooltip
# ----------------------------------

plot_data <- plot_data %>%
  mutate(
    tooltip = paste0(
      "Subject: ", USUBJID,
      "<br>Day: ", ADY,
      "<br><b>Response:</b> ", qsorres,
      "<br><b>Labs:</b>",
      "<br>&nbsp;&nbsp;BASO: ", BASO,
      "<br>&nbsp;&nbsp;NEUT: ", NEUT,
      "<br>&nbsp;&nbsp;PLAT: ", PLAT,
      "<br>&nbsp;&nbsp;HGB: ", HGB,
      "<br><b>Adverse Events:</b> ", AE
    )
  )

# loop with fix number of patient to be displayed by treatment
plot_data <- plot_data %>%
  arrange(TRT01A, USUBJID, ADY)

# Create subject-level grouping
subject_groups <- plot_data %>%
  distinct(TRT01A, USUBJID) %>%
  group_by(TRT01A) %>%
  mutate(
    subj_group = ceiling(row_number() / 10)  # 10 subjects per group
  )

# Merge grouping back
plot_data <- plot_data %>%
  left_join(subject_groups, by = c("TRT01A", "USUBJID"))

#Generate plots in loop

plots <- list()

for(trt in unique(plot_data$TRT01A)) {
  
  trt_data <- plot_data %>% filter(TRT01A == trt)
  
  for(g in unique(trt_data$subj_group)) {
    
    df <- trt_data %>% filter(subj_group == g)
    
    p <- ggplot(df, aes(x = ADY, y = qsorres, color = USUBJID)) +
      geom_line() +
      geom_point(aes(text = tooltip), size = 2) +
      labs(
        title = paste("Treatment:", trt, "| Subjects Group", g),
        x = "Study Day",
        y = "Response"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
    
    # Convert to interactive
    p_interactive <- ggplotly(p, tooltip = "text")
    
    # Store plot
    plots[[paste(trt, g, sep = "_")]] <- p_interactive
  }
}

# Show one plot
plots[[1]]


# Save all as html plots

# install.packages("htmlwidgets")

library(htmlwidgets)

# Define output directory
out_dir <- "U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots"

# Create directory if it doesn't exist
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Save plots
for (name in names(plots)) {
  file_path <- file.path(out_dir, paste0("plot_", name, ".html"))
  
  saveWidget(
    plots[[name]],
    file = file_path,
    selfcontained = TRUE
  )
}


# ----------------------------------
# 9. Interactive Plot
# ----------------------------------

# p <- ggplot(plot_data, aes(x = ADY, y = qsorres, color = USUBJID)) +
#   geom_line() +
#   geom_point(aes(text = tooltip), size = 2) +
#   labs(
#     title = "Patient Response with Labs and AE (Duration-aware)",
#     x = "Study Day (ADY)",
#     y = "Response"
#   ) +
#   theme_minimal()
# 
# ggplotly(p, tooltip = "text")


