library(shiny)
library(plotly)
library(timetk)

scores <- readRDS("scores.rds")
dataset <- readRDS("dataset.rds")

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("FLOSS Model"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for parameter values ----
      sliderInput(
        inputId = "window_size",
        label = "window_size:",
        min = 25,
        max = 150,
        value = 100,
        step = 25
      ),
      sliderInput(
        inputId = "regime_landmark",
        label = "regime_landmark:",
        min = 1,
        max = 10,
        value = 3,
        step = 0.5
      ),
      sliderInput(
        inputId = "regime_threshold",
        label = "regime_threshold:",
        min = 0.05,
        max = 0.9,
        value = 0.7,
        step = 0.05
      ),
      sliderInput(
        inputId = "time_constraint",
        label = "time_constraint:",
        min = 0,
        max = 2000,
        value = 950,
        step = 50
      ),
      sliderInput(
        inputId = "mp_threshold",
        label = "mp_threshold:",
        min = 0.0,
        max = 0.9,
        value = 0.1,
        step = 0.1
      ),
      selectInput(
        "filename",
        "filename:",
        unique(scores$record)
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("num_truth_var"),
      textOutput("num_pred_var"),
      textOutput("score_var"),
      plotlyOutput(outputId = "distPlot", height = "500px")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  observeEvent(input$filename, {
    res <- scores %>%
      dplyr::filter(
        record == input$filename
      ) %>%
      dplyr::arrange(score)


    updateSliderInput(
      inputId = "window_size",
      value = res$window_size[1]
    )
    updateSliderInput(
      inputId = "regime_landmark",
      value = res$regime_landmark[1]
    )
    updateSliderInput(
      inputId = "regime_threshold",
      value = res$regime_threshold[1]
    )
    updateSliderInput(
      inputId = "time_constraint",
      value = res$time_constraint[1]
    )
    updateSliderInput(
      inputId = "mp_threshold",
      value = res$mp_threshold[1]
    )
  })

  scoreResult <- reactive({
    res <- scores %>% dplyr::filter(
      window_size == input$window_size,
      time_constraint == input$time_constraint,
      round(mp_threshold * 10) == round(input$mp_threshold * 10),
      round(regime_landmark * 100) == round(input$regime_landmark * 100),
      round(regime_threshold * 100) == round(input$regime_threshold * 100),
      record == input$filename
    )
    if (nrow(res) == 1) {
      res
    } else {
      list(
        score = NA,
        pred = list(0)
      )
    }
  })

  # Only reads the ECG when filename changes ----
  ecgRecord <- reactive({
    data <- dataset[[input$filename]]
    plot <- data$ecg %>% plot_time_series(
      time, value,
      .title = glue::glue("FLOSS for {input$filename}"),
      .interactive = TRUE,
      .smooth = FALSE,
      .line_alpha = 0.3,
      .line_size = 0.2,
      .plotly_slider = TRUE
    )
    list(plot = plot, ecg = data$ecg, truth = data$truth, min = min(data$ecg$value), max = max(data$ecg$value))
  })

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlotly(
    plot <- ecgRecord()$plot %>% plotly::add_segments(
      x = scoreResult()$pred[[1]], xend = scoreResult()$pred[[1]], y = ecgRecord()$min,
      yend = ecgRecord()$max * 1.1,
      line = list(width = 2.5, color = "#0108c77f"),
      name = "Predicted"
    ) %>% plotly::add_segments(
      x = ecgRecord()$truth, xend = ecgRecord()$truth, y = ecgRecord()$min,
      yend = ecgRecord()$max,
      line = list(width = 2.5, color = "#ff00007f"),
      name = "Truth"
    )
  )

  output$num_truth_var <- renderText({
    paste("Num of regime changes (truth):", length(ecgRecord()$truth))
  })

  output$num_pred_var <- renderText({
    paste("Num of predicted changes:", length(scoreResult()$pred[[1]]))
  })

  output$score_var <- renderText({
    paste("Score:", round(scoreResult()$score, digits = 4))
  })
}

shinyApp(ui = ui, server = server)
