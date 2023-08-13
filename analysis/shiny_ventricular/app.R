library(shiny)
library(shinyFeedback)
library(waiter)
library(plotly)
library(timetk)

# scores <- readRDS("scores.rds")
# dataset <- readRDS("dataset.rds")
data_names <- readRDS("data_names.rds")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  useShinyFeedback(feedback = TRUE),
  autoWaiter("distPlot", html = spin_loaders(id = 5, color = "#0000006e"), color = "#ffffff7f", fadeout = TRUE),
  # App title ----
  titlePanel("FLOSS Model"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for parameter values ----
      sliderInput(
        inputId = "window_size", label = "window_size:", min = 25, max = 200, value = 50, step = 25
      ),
      sliderInput(
        inputId = "regime_landmark", label = "regime_landmark:", min = 2, max = 9.5, value = 7, step = 0.5
      ),
      sliderInput(
        inputId = "regime_threshold", label = "regime_threshold:", min = 0.05, max = 0.9, value = 0.55, step = 0.05
      ),
      selectInput(
        "filename", "filename:", data_names
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("update_var"),
      textOutput("num_truth_var"),
      textOutput("num_pred_var"),
      textOutput("score_var"),
      plotlyOutput(outputId = "distPlot", height = "500px"),
      helpText(
        "Instructions: 1) select a file from the dropdown menu.",
        "2) The best parameters will be automatically selected.",
        "3) You can change the parameters to see the effect on the model.",
        "4) If a combination of parameters is not found in the dataset, a warning will be displayed.",
        "5) You can zoom in and out the plot. The data has been downsampled to improve the App responsiveness."
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  dataset <- readRDS(glue::glue("{data_names[1]}.rds"))
  scores <- readRDS(glue::glue("{data_names[1]}_scores.rds"))


  observeEvent(input$filename, {
    freezeReactiveValue(input, "window_size")
  })

  observeEvent(input$window_size, {
    if (!(input$window_size %in% ecgRecord()$valid_inputs$window_size)) {
      val <- which.min(abs(ecgRecord()$valid_inputs$window_size - input$window_size))
      updateSliderInput(
        inputId = "window_size",
        value = ecgRecord()$valid_inputs$window_size[val]
      )
    }
  })

  observeEvent(input$regime_landmark, {
    if (!(input$regime_landmark %in% ecgRecord()$valid_inputs$regime_landmark)) {
      val <- which.min(abs(ecgRecord()$valid_inputs$regime_landmark - input$regime_landmark))
      updateSliderInput(
        inputId = "regime_landmark",
        value = ecgRecord()$valid_inputs$regime_landmark[val]
      )
    }
  })

  observeEvent(input$regime_threshold, {
    if (!(input$regime_threshold %in% ecgRecord()$valid_inputs$regime_threshold)) {
      val <- which.min(abs(ecgRecord()$valid_inputs$regime_threshold - input$regime_threshold))
      updateSliderInput(
        inputId = "regime_threshold",
        value = ecgRecord()$valid_inputs$regime_threshold[val]
      )
    }
  })

  # Only reads the ECG when filename changes ----
  ecgRecord <- eventReactive(input$filename, {
    dataset <<- readRDS(glue::glue("{input$filename}.rds"))
    scores <<- readRDS(glue::glue("{input$filename}_scores.rds"))
    data <- dataset
    plot <- data$ecg |> plot_time_series(
      time, value,
      .title = glue::glue("FLOSS for {input$filename}"),
      .interactive = TRUE,
      .smooth = FALSE,
      .line_alpha = 0.3,
      .line_size = 0.2,
      .plotly_slider = TRUE
    )

    res <- scores |>
      dplyr::arrange(score)

    valid_inputs <- list(
      window_size = unique(res$window_size),
      regime_landmark = unique(res$regime_landmark),
      regime_threshold = unique(res$regime_threshold)
    )

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

    list(plot = plot, valid_inputs = valid_inputs, ecg = data$ecg, truth = data$truth, min = min(data$ecg$value), max = max(data$ecg$value))
  })


  scoreResult <- reactive({
    wsize <- input$window_size # hack, dunno why this is necessary
    res <- scores |> dplyr::filter(
      window_size == input$window_size,
      round(regime_landmark * 100) == round(input$regime_landmark * 100),
      round(regime_threshold * 100) == round(input$regime_threshold * 100),
      record == input$filename
    )
    if (nrow(res) == 1) {
      hideToast(animate = FALSE)
      res
    } else {
      list(
        score = NA,
        pred = list(0)
      )
    }
  })


  output$distPlot <- renderPlotly(
    ecgRecord()$plot |> plotly::add_segments(
      x = scoreResult()$pred[[1]], xend = scoreResult()$pred[[1]], y = ecgRecord()$min,
      yend = ecgRecord()$max * 1.1,
      line = list(width = 2.5, color = "#0108c77f"),
      name = "Predicted"
    ) |> plotly::add_segments(
      x = ecgRecord()$truth, xend = ecgRecord()$truth, y = ecgRecord()$min,
      yend = ecgRecord()$max,
      line = list(width = 2.5, color = "#ff00007f"),
      name = "Truth"
    )
  )

  output$update_var <- renderText({
    paste("Update:", ifelse(is.na(runif(1) * scoreResult()$score), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  })

  output$num_truth_var <- renderText({
    paste("Num of regime changes (truth):", length(ecgRecord()$truth))
  })

  output$num_pred_var <- renderText({
    paste("Num of predicted changes:", length(scoreResult()$pred[[1]]))
  })

  output$score_var <- renderText({
    if (is.na(scoreResult()$score)) {
      showToast("warning", "Invalid parameter, Score is NA",
        keepVisible = TRUE,
        .options = list(
          positionClass = "toast-top-center",
          progressBar = FALSE
        )
      )
    }
    paste("Score:", round(scoreResult()$score, digits = 4))
  })

  showToast(
    "success",
    "FLOSS App Loaded!",
    .options = list(
      positionClass = "toast-top-center",
      progressBar = FALSE,
      timeOut = 3000,
      closeButton = FALSE,
      newestOnTop = TRUE,
      preventDuplicates = TRUE,
      showDuration = 300,
      hideDuration = 1000,
      extendedTimeOut = 1000,
      showEasing = "linear",
      hideEasing = "linear",
      showMethod = "fadeIn",
      hideMethod = "fadeOut"
    )
  )
}

shinyApp(ui = ui, server = server)
