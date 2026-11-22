# Australian Wine Sales Forecasting Shiny App

library(tidyverse)
library(fpp3)
library(shiny)

# Locate the CSV.
data_file <- "AustralianWines.csv"

if (!file.exists(data_file)) {
  stop("AustralianWines.csv must be located in the same folder.")
}

# Ensure all wine columns are numeric.
aus_wine <- read_csv(
  data_file,
  na = "*",
  col_types = cols(Month = col_character(), .default = col_double()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = yearmonth(my(Month)))

wine_data <- aus_wine |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales",
    values_transform = list(Sales = as.numeric)
  ) |>
  as_tsibble(index = Month, key = Varietal) |>
  filter(!is.na(Sales))

wine_varietals <- sort(unique(wine_data$Varietal))
wine_min <- min(wine_data$Month)
wine_max <- max(wine_data$Month)
wine_min_date <- as.Date(wine_min)
wine_max_date <- as.Date(wine_max)

range_months <- function(date_range) {
  req(date_range, length(date_range) == 2,
      !is.na(date_range[1]), !is.na(date_range[2]))
  list(
    start = yearmonth(format(as.Date(date_range[1]), "%Y-%m")),
    end = yearmonth(format(as.Date(date_range[2]), "%Y-%m"))
  )
}


ui <- navbarPage(
  title = "Australian Wine Sales Forecasting",
  tabPanel(
    "Data Exploration",
    sidebarLayout(
      sidebarPanel(
        selectInput("varietal_explore", "Select wine varietal(s):",
          choices = wine_varietals, selected = wine_varietals[1], multiple = TRUE
        ),
        dateRangeInput("date_explore", "Historical date range:",
          start = wine_min_date, end = wine_max_date,
          min = wine_min_date, max = wine_max_date
        )
      ),
      mainPanel(
        textOutput("explore_range_label"),
        plotOutput("sales_plot"),
        plotOutput("season_plot")
      )
    )
  ),
  tabPanel(
    "Model Evaluation",
    sidebarLayout(
      sidebarPanel(
        selectInput("varietal_eval", "Select wine varietal(s):",
          choices = wine_varietals, selected = wine_varietals[1], multiple = TRUE
        ),
        dateRangeInput("date_eval", "Evaluation date range:",
          start = wine_min_date, end = wine_max_date,
          min = wine_min_date, max = wine_max_date
        ),
        dateInput("train_end", "Training period ends:",
          value = wine_max_date - 365,
          min = wine_min_date, max = wine_max_date - 1
        )
      ),
      mainPanel(
        textOutput("eval_range_label"),
        h3("Model Specifications"),
        verbatimTextOutput("model_specs"),
        h3("Training Accuracy"),
        tableOutput("training_metrics"),
        h3("Validation Accuracy"),
        tableOutput("validation_metrics"),
        h3("Best Model by Validation RMSE"),
        tableOutput("best_model")
      )
    )
  ),
  tabPanel(
    "Forecasting",
    sidebarLayout(
      sidebarPanel(
        selectInput("varietal_fc", "Select wine varietal(s): ",
          choices = wine_varietals, selected = wine_varietals[1], multiple = TRUE
        ),
        dateRangeInput("date_fc", "Forecast using historical date range:",
          start = wine_min_date, end = wine_max_date,
          min = wine_min_date, max = wine_max_date
        ),
        sliderInput("horizon", "Forecast horizon (months): ",
          min = 1, max = 36, value = 12, step = 1
        )
      ),
      mainPanel(
        textOutput("forecast_range_label"),
        plotOutput("future_plot"),
        downloadButton("download_metrics", "Download Validation Accuracy (CSV)")
      )
    )
  )
)


server <- function(input, output, session) {
  # Exploration - filtering is based on the actual yearmonth index.
  selected_explore_data <- reactive({
    req(input$varietal_explore, input$date_explore)
    date_months <- range_months(input$date_explore)
    wine_data |>
      filter(
        Varietal %in% input$varietal_explore,
        Month >= date_months$start,
        Month <= date_months$end
      )
  })

  output$explore_range_label <- renderText({
    req(input$date_explore)
    date_months <- range_months(input$date_explore)
    paste("Selected period:", format(as.Date(date_months$start), "%b %Y"),
          "through", format(as.Date(date_months$end), "%b %Y"),
          " | Rows displayed:", nrow(selected_explore_data()))
  })

  output$sales_plot <- renderPlot({
    data <- selected_explore_data()
    validate(need(nrow(data) > 0, "No observations for the selected dates/varietals."))
    autoplot(data, Sales) +
      labs(title = "Australian Wine Sales", x = "Month", y = "Sales") +
      theme_minimal()
  })


  output$season_plot <- renderPlot({
    data <- selected_explore_data()
    validate(need(nrow(data) > 0, "No observations for the selected dates/varietals."))
    gg_season(data, Sales) +
      labs(title = "Seasonal Sales Pattern", x = "Month", y = "Sales") +
      theme_minimal()
  })


  # Model Evaluation - date range and training cutoff jointly determine the split.
  selected_eval_data <- reactive({
    req(input$varietal_eval, input$date_eval)
    date_months <- range_months(input$date_eval)
    wine_data |>
      filter(
        Varietal %in% input$varietal_eval,
        Month >= date_months$start,
        Month <= date_months$end
      )
  })

  output$eval_range_label <- renderText({
    req(input$date_eval)
    date_months <- range_months(input$date_eval)
    paste("Evaluation period:", format(as.Date(date_months$start), "%b %Y"),
          "through", format(as.Date(date_months$end), "%b %Y"),
          " | Rows selected:", nrow(selected_eval_data()))
  })

  training_data <- reactive({
    req(input$train_end)
    selected_eval_data() |> filter(Month <= yearmonth(as.Date(input$train_end)))
  })

  validation_data <- reactive({
    req(input$train_end)
    selected_eval_data() |> filter(Month > yearmonth(as.Date(input$train_end)))
  })

  training_models <- reactive({
    data <- training_data()
    validate(need(nrow(data) > 0, "No training data. Adjust the date range or training cutoff."))
    data |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  output$model_specs <- renderPrint({
    req(training_models())
    training_models() |> select(Varietal, .model) |> report()
  })


  output$training_metrics <- renderTable({
    training_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  }, digits = 2)


  validation_metrics <- reactive({
    models <- training_models()
    actual <- validation_data()
    validate(need(nrow(actual) > 0, "No validation observations. Set the training cutoff earlier."))
    h <- actual |> distinct(Month) |> nrow()
    models |> forecast(h = h) |> accuracy(actual) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  })


  output$validation_metrics <- renderTable({
    validation_metrics()
  }, digits = 2)


  output$best_model <- renderTable({
    validation_metrics() |>
      group_by(Varietal) |>
      slice_min(RMSE, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(Varietal, Best_Model = .model, RMSE, MAE, MAPE)
  }, digits = 2)


  # Forecasting tab - fit using the selected historical months and forecast forward.
  forecast_data <- reactive({
    req(input$varietal_fc, input$date_fc)
    date_months <- range_months(input$date_fc)
    wine_data |>
      filter(
        Varietal %in% input$varietal_fc,
        Month >= date_months$start,
        Month <= date_months$end
      )
  })

  output$forecast_range_label <- renderText({
    req(input$date_fc)
    date_months <- range_months(input$date_fc)
    paste("Forecast history:", format(as.Date(date_months$start), "%b %Y"),
          "through", format(as.Date(date_months$end), "%b %Y"),
          " | Rows selected:", nrow(forecast_data()))
  })

  future_models <- reactive({
    data <- forecast_data()
    validate(need(nrow(data) > 0, "No observations for selected dates/varietals."))
    data |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })


  future_forecasts <- reactive({
    future_models() |> forecast(h = input$horizon)
  })


  output$future_plot <- renderPlot({
    fc <- future_forecasts()
    hist <- forecast_data()
    validate(need(nrow(hist) > 0, "No historical data to forecast."))
    autoplot(fc, hist, level = c(80, 95)) +
      labs(title = "Comparative Future Wine Sales Forecasts",
           x = "Month", y = "Sales", colour = "Model") +
      theme_minimal()
  })


  output$download_metrics <- downloadHandler(
    filename = function() "wine_validation_accuracy.csv",
    content = function(file) {
      write_csv(validation_metrics(), file)
    }
  )
}


shinyApp(ui, server)
