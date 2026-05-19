## Housekeeping
graphics.off()
rm(list=ls())
closeAllConnections()


###############################################
# Efficacy–Safety Dual Panel Plot (FINAL)
###############################################

library(dplyr)
library(plotly)
library(purrr)
library(tibble)
library(htmlwidgets)

set.seed(123)

###############################################
# 1. ADSL
###############################################
n <- 150

adsl <- data.frame(
  USUBJID = sprintf("SUBJ%03d", 1:n),
  TRT01A  = sample(c("Placebo", "Drug"), n, TRUE)
)

###############################################
# 2. ADQS (Efficacy with AVAL + CHG)
###############################################
visits <- c(0, 7, 14, 28, 56, 84)

adqs <- expand.grid(
  USUBJID = adsl$USUBJID,
  AVISITN = visits
) %>%
  left_join(adsl, by = "USUBJID") %>%
  mutate(
    BASE = rnorm(n(), 50, 10),
    
    CHG = ifelse(
      AVISITN == 0,
      0,   # ✅ enforce baseline
      ifelse(TRT01A == "Drug",
             -0.3 * AVISITN + rnorm(n(), 0, 3),
             -0.1 * AVISITN + rnorm(n(), 0, 3))
    ),
    
    AVAL = BASE + CHG
  )

eff_summary <- adqs %>%
  group_by(TRT01A, AVISITN) %>%
  summarise(
    MEAN_CHG  = mean(CHG),
    MEAN_AVAL = mean(AVAL),
    .groups = "drop"
  ) %>%
  mutate(
    MEAN_CHG = ifelse(AVISITN == 0, 0, MEAN_CHG)  # ✅ safety check
  )

###############################################
# 3. ADAE
###############################################
adae <- map_dfr(1:nrow(adsl), function(i) {
  
  if (rbinom(1,1,0.6)==0) return(NULL)
  
  n_ae <- rpois(1,4)
  
  tibble(
    USUBJID = adsl$USUBJID[i],
    TRT01A  = adsl$TRT01A[i],
    AESTDY  = sample(1:84, n_ae, TRUE),
    AEDECOD = sample(
      c("Headache","Nausea","Fatigue","Dizziness","Diarrhea"),
      n_ae, TRUE
    )
  )
})

###############################################
# 4. SAFETY SUMMARY (VISITS ONLY ✅)
###############################################

first_ae <- adae %>%
  group_by(USUBJID, TRT01A) %>%
  summarise(FIRST_AE_DAY = min(AESTDY), .groups="drop")

n_trt <- adsl %>% count(TRT01A, name="N")

grid <- expand.grid(
  TRT01A = unique(adsl$TRT01A),
  DAY    = visits
)

ae_by_trt <- split(adae, adae$TRT01A)

safe_list <- lapply(1:nrow(grid), function(i){
  
  trt <- grid$TRT01A[i]
  day <- grid$DAY[i]
  
  df <- ae_by_trt[[trt]]
  ae_sub <- df$AEDECOD[df$AESTDY <= day]
  
  if(length(ae_sub)==0){
    return(data.frame(AE_SUBJ=0, TOP_AE1="", TOP_AE2=""))
  }
  
  tbl <- sort(table(ae_sub), decreasing = TRUE)
  
  data.frame(
    AE_SUBJ = sum(first_ae$TRT01A==trt &
                    first_ae$FIRST_AE_DAY <= day),
    TOP_AE1 = names(tbl)[1],
    TOP_AE2 = if(length(tbl)>=2) names(tbl)[2], 
    TOP_AE3 = if(length(tbl)>=3) names(tbl)[3] else ""
  )
})

safety_summary <- bind_cols(
  grid,
  bind_rows(safe_list)
) %>%
  left_join(n_trt, by="TRT01A") %>%
  mutate(AE_RATE = AE_SUBJ/N * 100)

###############################################
# 5. BUILD PLOT (ALIGNED AXES ✅)
###############################################

fig <- plot_ly(width = 850, height = 650)

# --- EFFICACY ---
fig <- fig %>%
  add_trace(
    data = eff_summary,
    x = ~AVISITN,
    y = ~MEAN_CHG,
    color = ~TRT01A,
    type = "scatter",
    mode = "lines+markers",
    xaxis = "x",
    yaxis = "y",
    hoverinfo = "text",
    hovertext = ~paste0(
      "Treatment: ", TRT01A,
      "<br>Day: ", AVISITN,
      "<br>Mean Value: ", round(MEAN_AVAL, 2),
      "<br>Mean Change: ", round(MEAN_CHG, 2)
    )
  )

# --- SAFETY ---
fig <- fig %>%
  add_trace(
    data = safety_summary,
    x = ~DAY,
    y = ~AE_RATE,
    color = ~TRT01A,
    type = "scatter",
    mode = "lines",
    xaxis = "x2",
    yaxis = "y2",
    showlegend = FALSE,
    hoverinfo = "text",
    hovertext = ~paste0(
      "Treatment: ", TRT01A,
      "<br>Day: ", DAY,
      "<br>AE Rate: ", round(AE_RATE,1), "%",
      "<br>Top AE: ", TOP_AE1,
      "<br>Second AE: ", TOP_AE2,
      "<br>Third AE: ", TOP_AE3
    )
  )

###############################################
# 6. LAYOUT (FIXED X-AXIS ✅)
###############################################
padding <- 3
fig <- fig %>%
  layout(
#    hovermode = "x unified",
#    hoverlabel = list(align = "left"),
    title = "Efficacy–Safety Juxtaposition Over Time",
    
    # --- TOP PANEL ---
    yaxis = list(
      title = "Mean Change (Efficacy)",
      domain = c(0.55, 1)
    ),
    
   
    xaxis = list(
      showticklabels = FALSE,
      tickvals = visits,                    # ✅ SAME ticks
      showgrid = TRUE,                      # ✅ enable gridlines
      gridcolor = "lightgray",
      gridwidth = 1,
      zeroline = FALSE,
      range = c(min(visits) - padding, max(visits) + padding)   # ✅ same range
    ),
    
    # --- BOTTOM PANEL ---
    yaxis2 = list(
      title = "Cumulative AE Incidence (%)",
      domain = c(0, 0.45),
      range  = c(0, 100)
    ),
    
    xaxis2 = list(
      title = "Study Day",
      tickvals = visits,                    # ✅ SAME ticks
      ticktext = paste("Day", visits),
      showgrid = TRUE,                      # ✅ enable gridlines
      gridcolor = "lightgray",
      gridwidth = 1,
      zeroline = FALSE,
      range = c(min(visits) - padding, max(visits) + padding),  # ✅ SAME range
      anchor = "y2"
    ),
    
    legend = list(
      orientation = "h",
      x = 0.3,
      y = -0.15
    )
  )

###############################################
# 7. DISPLAY ✅
###############################################
fig


###############################################
# 8. SAVE (SAFE ✅)
###############################################
file_path <- "U:/Biostatistical Services/Working/IDMC_Graphs/Test/Analysis Outputs/IDMCxy/W_20250129/DUMMY/Figures/RPlots/Efficacy_Safety_Top3.html"

htmlwidgets::saveWidget(
  fig,
  file = file_path,
  selfcontained = FALSE
)



