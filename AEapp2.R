library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# ------------------------------------------------------------
# ADSL
# ------------------------------------------------------------
set.seed(123)

adsl <- data.frame(
  USUBJID = paste0("SUBJ", sprintf("%03d", 1:180)),
  TRT = rep(c("Placebo", "Treatment"), each = 90)
)

# ------------------------------------------------------------
# AE
# ------------------------------------------------------------
ae <- data.frame(
  USUBJID = sample(adsl$USUBJID, 450, replace = TRUE),
  SOC = sample(
    c(
      "Gastrointestinal disorders",
      "Nervous system disorders",
      "General disorders",
      "Cardiac disorders",
      "Skin and subcutaneous tissue disorders",
      "Respiratory disorders"
    ),
    450, replace = TRUE
  ),
  PT = sample(
    c(
      "Nausea", "Diarrhea", "Vomiting",
      "Headache", "Dizziness",
      "Fatigue", "Pyrexia",
      "Palpitations", "Tachycardia",
      "Rash", "Pruritus",
      "Dyspnea", "Cough"
    ),
    450, replace = TRUE
  ),
  AEGRADE = sample(
    c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
    450, replace = TRUE
  ),
  TEAE = sample(c(TRUE, FALSE), 450, replace = TRUE),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# ADLB‑like labs
# ------------------------------------------------------------
adlb <- data.frame(
  USUBJID  = rep(adsl$USUBJID, each = 4),
  PARAM    = rep(c("ALT", "AST", "Hemoglobin", "Creatinine"),
                 times = nrow(adsl)),
  VISITNUM = sample(c(0, 4, 8, 12),
                    nrow(adsl) * 4, replace = TRUE),
  AVAL     = round(rnorm(nrow(adsl) * 4, 1, 0.25), 2)
)

lab_summary <- adlb %>%
  filter(VISITNUM > 0) %>%
  group_by(USUBJID, PARAM) %>%
  summarise(WORST = max(AVAL), .groups = "drop") %>%
  pivot_wider(
    names_from = PARAM,
    values_from = WORST,
    names_prefix = "LAB_"
  )

# ------------------------------------------------------------
# UI  ✅ PLOT ABOVE TABLE
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Adverse Event Butterfly Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "soc",
        "System Organ Class:",
        choices = c("All", sort(unique(ae$SOC))),
        selected = "All"
      ),
      checkboxInput(
        "teae_only",
        "Treatment‑Emergent AEs only",
        TRUE
      )
    ),
    
    mainPanel(
      plotlyOutput("ae_plot", height = 600),
      hr(),
      DTOutput("ae_table")
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  ae_filtered <- reactive({
    dat <- ae
    if (input$teae_only) dat <- dat %>% filter(TEAE)
    if (input$soc != "All") dat <- dat %>% filter(SOC == input$soc)
    dat
  })
  
  plot_df <- reactive({
    
    denom <- adsl %>% count(TRT, name = "N")
    
    ae_filtered() %>%
      left_join(adsl, by = "USUBJID") %>%
      distinct(USUBJID, TRT, SOC, PT, AEGRADE) %>%
      count(TRT, SOC, PT, AEGRADE, name = "n") %>%
      left_join(denom, by = "TRT") %>%
      mutate(
        pct = round(100 * n / N, 1),
        hover_txt = paste0(n, " / ", N, " (", pct, "%)"),
        xval = ifelse(TRT == "Placebo", -n, n)
      )
  })
  
  output$ae_plot <- renderPlotly({
    
    df <- plot_df()
    
    plot_ly() %>%
      add_bars(
        data = df %>% filter(TRT == "Placebo"),
        x = ~xval,
        y = ~PT,
        orientation = "h",
        color = ~AEGRADE,
        customdata = ~SOC,
        text = ~hover_txt,
        textposition = "none",
        hovertemplate =
          "<b>Placebo</b><br>" %>%
          paste0(
            "SOC: %{customdata}<br>",
            "PT: %{y}<br>",
#            "Grade: %{color}<br>",
            "n / N (%): %{text}<extra></extra>"
          ),
        showlegend = TRUE
      ) %>%
      add_bars(
        data = df %>% filter(TRT == "Treatment"),
        x = ~xval,
        y = ~PT,
        orientation = "h",
        color = ~AEGRADE,
        customdata = ~SOC,
        text = ~hover_txt,
        textposition = "none",
        hovertemplate =
          "<b>Treatment</b><br>" %>%
          paste0(
            "SOC: %{customdata}<br>",
            "PT: %{y}<br>",
#            "Grade: %{color}<br>",
            "n / N (%): %{text}<extra></extra>"
          ),
        showlegend = FALSE
      ) %>%
      layout(
        barmode = "relative",
        xaxis = list(
          title = "Patients (%)",
          tickvals = seq(-50, 50, 10),
          ticktext = abs(seq(-50, 50, 10))
        ),
        yaxis = list(title = "Preferred Term"),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        )
      )
  })
  
  output$ae_table <- renderDT({
    
    ae_filtered() %>%
      left_join(adsl, by = "USUBJID") %>%
      left_join(lab_summary, by = "USUBJID") %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          search = list(smart = FALSE)
        )
      )
  })
}

shinyApp(ui, server)
