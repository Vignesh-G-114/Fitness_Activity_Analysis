# ============================================================
# FITNESS PULSE
# Fitness Activity Analysis Dashboard
# Built with R Shiny, Plotly, ggplot2 and Bootstrap 5
# ============================================================

# 1. CHECK AND LOAD REQUIRED PACKAGES -------------------------

required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", "DT",
  "scales", "readr", "plotly", "bsicons", "lubridate"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them using install.packages()."
  )
}

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(readr)
library(plotly)
library(bsicons)
library(lubridate)


# 2. CONFIGURATION --------------------------------------------

DATA_FILE <- file.path("data", "fitness_activity.csv")

APP_TITLE <- "FITNESS PULSE"
STEP_GOAL <- 10000

APP_COLORS <- list(
  primary = "#2364AA",
  success = "#16A085",
  warning = "#E76F51",
  dark = "#183153",
  background = "#F4F7FB"
)


# 3. GENERATE DEMO DATA ---------------------------------------

make_demo_data <- function(n = 1800, seed = 2026) {

  set.seed(seed)

  dates <- seq.Date(
    from = as.Date("2025-01-01"),
    by = "day",
    length.out = n
  )

  weekday <- lubridate::wday(dates)

  steps <- round(
    pmin(
      pmax(
        rnorm(n, 7600, 2100) *
          (1 + 0.12 * sin(seq_len(n) * 2 * pi / 365.25)) +
          ifelse(weekday %in% c(1, 7), 650, 0),
        800
      ),
      22000
    )
  )

  active_minutes <- round(
    pmin(
      pmax(
        steps / 100 + rnorm(n, 8, 12),
        5
      ),
      180
    )
  )

  activity_types <- c(
    "Walking", "Running", "Cycling",
    "Strength", "Yoga", "Rest"
  )

  data.frame(
    Date = dates,

    Activity_Type = sample(
      activity_types,
      n,
      replace = TRUE
    ),

    Steps = steps,

    Distance_km = round(
      pmax(
        0.5,
        steps * 0.00075 + rnorm(n, 0, 0.35)
      ),
      2
    ),

    Active_Minutes = active_minutes,

    Calories_Burned = round(
      pmin(
        pmax(
          1650 + steps * 0.035 +
            active_minutes * 3.1 +
            rnorm(n, 0, 180),
          1200
        ),
        4500
      )
    ),

    Sleep_Hours = round(
      pmin(
        pmax(rnorm(n, 7.1, 0.9), 3.5),
        10.5
      ),
      1
    ),

    Avg_Heart_Rate = round(
      pmin(
        pmax(
          72 - (steps - 7500) / 1800 +
            rnorm(n, 0, 5),
          48
        ),
        115
      )
    ),

    Workout_Level = ifelse(
      active_minutes >= 45,
      "Workout",
      ifelse(active_minutes >= 25, "Moderate", "Light")
    ),

    Goal_Met = ifelse(
      steps >= STEP_GOAL,
      "Yes",
      "No"
    ),

    Day_Type = ifelse(
      weekday %in% c(1, 7),
      "Weekend",
      "Weekday"
    ),

    stringsAsFactors = FALSE
  )
}


# 4. LOAD AND VALIDATE DATA -----------------------------------

load_fitness_data <- function() {

  if (file.exists(DATA_FILE)) {

    data <- tryCatch(
      readr::read_csv(
        DATA_FILE,
        show_col_types = FALSE,
        na = c("", "NA", "N/A", "null")
      ),
      error = function(e) {
        warning(
          "Could not read CSV: ",
          conditionMessage(e),
          ". Using demo data instead."
        )

        make_demo_data()
      }
    )

  } else {

    data <- make_demo_data()

    message(
      "CSV not found. Using generated demo data."
    )
  }

  data <- as.data.frame(data)

  if (nrow(data) == 0) {
    warning("Dataset is empty. Using demo data.")
    data <- make_demo_data()
  }

  # Required columns and safe defaults
  defaults <- list(
    Date = seq.Date(
      as.Date("2025-01-01"),
      by = "day",
      length.out = nrow(data)
    ),
    Activity_Type = rep("Walking", nrow(data)),
    Steps = rep(0, nrow(data)),
    Distance_km = rep(0, nrow(data)),
    Active_Minutes = rep(0, nrow(data)),
    Calories_Burned = rep(0, nrow(data)),
    Sleep_Hours = rep(NA_real_, nrow(data)),
    Avg_Heart_Rate = rep(NA_real_, nrow(data)),
    Workout_Level = rep("Light", nrow(data)),
    Goal_Met = rep("No", nrow(data)),
    Day_Type = rep("Weekday", nrow(data))
  )

  for (column in names(defaults)) {

    if (!column %in% names(data)) {
      data[[column]] <- defaults[[column]]
    }
  }

  # Parse dates safely
  if (!inherits(data$Date, "Date")) {

    original_dates <- data$Date

    parsed_dates <- suppressWarnings(
      as.Date(original_dates)
    )

    if (all(is.na(parsed_dates))) {
      parsed_dates <- suppressWarnings(
        lubridate::ymd(original_dates)
      )
    }

    if (all(is.na(parsed_dates))) {
      parsed_dates <- defaults$Date
    }

    data$Date <- parsed_dates
  }

  # Convert numeric columns
  numeric_columns <- c(
    "Steps",
    "Distance_km",
    "Active_Minutes",
    "Calories_Burned",
    "Sleep_Hours",
    "Avg_Heart_Rate"
  )

  for (column in numeric_columns) {

    data[[column]] <- suppressWarnings(
      as.numeric(as.character(data[[column]]))
    )
  }

  # Handle missing categorical values
  for (column in c("Activity_Type", "Workout_Level")) {

    data[[column]] <- as.character(data[[column]])

    data[[column]][
      is.na(data[[column]]) |
        trimws(data[[column]]) == ""
    ] <- if (column == "Activity_Type") "Walking" else "Light"
  }

  data$Goal_Met <- ifelse(
    data$Steps >= STEP_GOAL,
    "Yes",
    "No"
  )

  data$Day_Type <- ifelse(
    lubridate::wday(data$Date) %in% c(1, 7),
    "Weekend",
    "Weekday"
  )

  data <- data %>%
    filter(!is.na(Date)) %>%
    arrange(Date)

  if (nrow(data) == 0) {
    data <- make_demo_data()
  }

  data
}

fitness <- load_fitness_data()

DATE_MIN <- min(fitness$Date)
DATE_MAX <- max(fitness$Date)

STEP_MAX <- max(
  25000,
  ceiling(max(fitness$Steps, na.rm = TRUE) / 500) * 500
)


# 5. GLOBAL VISUAL THEME --------------------------------------

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = APP_COLORS$dark
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 18, 12, 12),
      legend.position = "bottom"
    )
)


# 6. USER INTERFACE -------------------------------------------

ui <- page_sidebar(

  title = div(
    span(
      "FITNESS",
      style = "font-weight:800;letter-spacing:2px;"
    ),
    span(
      " PULSE",
      style = "font-weight:300;"
    )
  ),

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = APP_COLORS$primary,
    base_font = font_google("Inter")
  ),

  sidebar = sidebar(
    width = 275,

    h5(
      "ACTIVITY CONTROLS",
      class = "fw-bold"
    ),

    p(
      "Filter activity history and compare patterns.",
      class = "text-muted small"
    ),

    dateRangeInput(
      "date_range",
      "Date range",
      start = DATE_MIN,
      end = DATE_MAX,
      min = DATE_MIN,
      max = DATE_MAX
    ),

    selectInput(
      "activity_filter",
      "Activity type",
      choices = c(
        "All activities",
        sort(unique(fitness$Activity_Type))
      ),
      selected = "All activities"
    ),

    selectInput(
      "day_filter",
      "Day type",
      choices = c(
        "All days",
        "Weekday",
        "Weekend"
      ),
      selected = "All days"
    ),

    sliderInput(
      "steps_filter",
      "Daily steps",
      min = 0,
      max = STEP_MAX,
      value = c(0, STEP_MAX),
      step = 500,
      sep = ","
    ),

    actionButton(
      "reset",
      "Reset filters",
      class = "btn-outline-primary w-100"
    ),

    hr(),

    p(
      "Synthetic sample dataset included.",
      class = "text-muted small"
    ),

    downloadButton(
      "download_data",
      "Download filtered CSV",
      class = "btn-primary w-100"
    )
  ),

  tags$head(
    tags$style(
      HTML("
        body {
          background: #F4F7FB;
          overflow-x: hidden;
        }

        .bslib-page-title {
          font-weight: 800;
          color: #183153;
        }

        .card {
          border: 1px solid #E3EAF3;
          border-radius: 15px;
          box-shadow: 0 3px 12px rgba(24,49,83,.05);
          min-width: 0;
        }

        .card-header {
          font-weight: 700;
          padding: 13px 16px;
        }

        .card-body {
          min-width: 0;
        }

        .value-box {
          min-height: 120px;
          overflow: hidden;
          border-radius: 15px;
        }

        .value-box .value-box-value {
          font-size: clamp(1.25rem, 1.8vw, 2rem);
          line-height: 1.25;
          overflow-wrap: anywhere;
        }

        .value-box .value-box-showcase {
          opacity: .2;
        }

        .plotly {
          width: 100% !important;
          min-width: 0;
        }

        .dataTables_wrapper {
          width: 100%;
        }

        @media (max-width: 768px) {
          .value-box {
            min-height: 100px;
          }

          .card {
            margin-bottom: 10px;
          }
        }
      ")
    )
  ),

  # KPI CARDS
  layout_column_wrap(
    width = 1 / 4,

    value_box(
      title = "Activity days",
      value = textOutput("kpi_days", inline = TRUE),
      showcase = bs_icon("calendar-check"),
      theme = "primary"
    ),

    value_box(
      title = "Average steps",
      value = textOutput("kpi_steps", inline = TRUE),
      showcase = bs_icon("activity"),
      theme = "success"
    ),

    value_box(
      title = "Active minutes/day",
      value = textOutput("kpi_active", inline = TRUE),
      showcase = bs_icon("stopwatch"),
      theme = "info"
    ),

    value_box(
      title = "Goal achievement",
      value = textOutput("kpi_goal", inline = TRUE),
      showcase = bs_icon("bullseye"),
      theme = "secondary"
    )
  ),

  # CHART ROW 1
  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Daily steps trend"),
      plotlyOutput(
        "steps_plot",
        height = "340px",
        width = "100%"
      )
    ),

    card(
      card_header("Activity type distribution"),
      plotlyOutput(
        "activity_plot",
        height = "340px",
        width = "100%"
      )
    )
  ),

  # CHART ROW 2
  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Calories burned vs active minutes"),
      plotlyOutput(
        "calories_plot",
        height = "340px",
        width = "100%"
      )
    ),

    card(
      card_header("Sleep and activity relationship"),
      plotlyOutput(
        "sleep_plot",
        height = "340px",
        width = "100%"
      )
    )
  ),

  # CHART ROW 3
  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Weekday vs weekend"),
      plotlyOutput(
        "day_plot",
        height = "320px",
        width = "100%"
      )
    ),

    card(
      card_header("Heart-rate distribution"),
      plotlyOutput(
        "heart_plot",
        height = "320px",
        width = "100%"
      )
    )
  ),

  # DATA TABLE
  card(
    card_header(
      div(
        class = paste(
          "d-flex",
          "justify-content-between",
          "align-items-center",
          "flex-wrap",
          "gap-2"
        ),

        span("Activity explorer"),
        textOutput("row_count", inline = TRUE)
      )
    ),

    DTOutput(
      "activity_table",
      width = "100%"
    )
  ),

  # ABOUT
  card(
    card_header("About this dashboard"),

    p(
      paste(
        "Fitness Pulse summarizes daily steps, distance,",
        "active minutes, calories, sleep and average heart rate.",
        "Records are synthetic examples for learning and are",
        "not medical guidance."
      )
    )
  )
)


# 7. SERVER LOGIC ---------------------------------------------

server <- function(input, output, session) {

  # Reset all filters
  observeEvent(input$reset, {

    updateDateRangeInput(
      session,
      "date_range",
      start = DATE_MIN,
      end = DATE_MAX
    )

    updateSelectInput(
      session,
      "activity_filter",
      selected = "All activities"
    )

    updateSelectInput(
      session,
      "day_filter",
      selected = "All days"
    )

    updateSliderInput(
      session,
      "steps_filter",
      value = c(0, STEP_MAX)
    )
  })


  # Reactive filtered dataset
  filtered_data <- reactive({

    req(input$date_range)
    req(input$steps_filter)

    validate(
      need(
        !any(is.na(input$date_range)),
        "Please select a valid date range."
      )
    )

    data <- fitness %>%
      filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        Steps >= input$steps_filter[1],
        Steps <= input$steps_filter[2]
      )

    if (
      !is.null(input$activity_filter) &&
      input$activity_filter != "All activities"
    ) {
      data <- data %>%
        filter(Activity_Type == input$activity_filter)
    }

    if (
      !is.null(input$day_filter) &&
      input$day_filter != "All days"
    ) {
      data <- data %>%
        filter(Day_Type == input$day_filter)
    }

    data
  })


  # Reusable empty-data message
  empty_message <- function() {
    validate(
      need(
        nrow(filtered_data()) > 0,
        "No records match your selected filters."
      )
    )
  }


  # KPI: Number of activity days
  output$kpi_days <- renderText({

    comma(nrow(filtered_data()))
  })


  # KPI: Average steps
  output$kpi_steps <- renderText({

    data <- filtered_data()

    if (nrow(data) == 0) {
      return("—")
    }

    comma(round(mean(data$Steps, na.rm = TRUE)))
  })


  # KPI: Average active minutes
  output$kpi_active <- renderText({

    data <- filtered_data()

    if (nrow(data) == 0) {
      return("—")
    }

    paste0(
      round(mean(data$Active_Minutes, na.rm = TRUE)),
      " min"
    )
  })


  # KPI: Goal achievement percentage
  output$kpi_goal <- renderText({

    data <- filtered_data()

    if (nrow(data) == 0) {
      return("—")
    }

    goal_rate <- mean(
      data$Goal_Met == "Yes",
      na.rm = TRUE
    )

    percent(goal_rate, accuracy = 0.1)
  })


  # Table record count
  output$row_count <- renderText({

    paste(
      comma(nrow(filtered_data())),
      "records"
    )
  })


  # Chart 1: Daily steps trend
  output$steps_plot <- renderPlotly({

    empty_message()

    data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        Steps = mean(Steps, na.rm = TRUE),
        .groups = "drop"
      )

    plot <- ggplot(
      data,
      aes(
        x = Date,
        y = Steps,
        text = paste0(
          "Date: ", format(Date),
          "<br>Steps: ", comma(round(Steps))
        )
      )
    ) +
      geom_line(
        colour = APP_COLORS$primary,
        linewidth = 0.8
      ) +
      scale_y_continuous(
        labels = label_comma()
      ) +
      labs(
        x = NULL,
        y = "Steps"
      )

    ggplotly(
      plot,
      tooltip = "text"
    ) %>%
      layout(
        margin = list(l = 65, r = 15, b = 60, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Chart 2: Activity distribution
  output$activity_plot <- renderPlotly({

    empty_message()

    data <- filtered_data() %>%
      count(Activity_Type, sort = TRUE)

    plot <- ggplot(
      data,
      aes(
        x = reorder(Activity_Type, n),
        y = n,
        fill = Activity_Type,
        text = paste0(
          Activity_Type, ": ",
          comma(n), " days"
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(
        labels = label_comma()
      ) +
      labs(
        x = NULL,
        y = "Days"
      )

    ggplotly(
      plot,
      tooltip = "text"
    ) %>%
      layout(
        margin = list(l = 90, r = 15, b = 50, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Chart 3: Calories vs active minutes
  output$calories_plot <- renderPlotly({

    empty_message()

    plot <- ggplot(
      filtered_data(),
      aes(
        x = Active_Minutes,
        y = Calories_Burned,
        colour = Activity_Type,
        text = paste0(
          "Active: ", Active_Minutes, " min",
          "<br>Calories: ", Calories_Burned,
          "<br>Activity: ", Activity_Type
        )
      )
    ) +
      geom_point(
        alpha = 0.7,
        size = 2
      ) +
      labs(
        x = "Active minutes",
        y = "Calories burned",
        colour = "Activity"
      )

    ggplotly(
      plot,
      tooltip = "text"
    ) %>%
      layout(
        margin = list(l = 65, r = 15, b = 75, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Chart 4: Sleep vs steps
  output$sleep_plot <- renderPlotly({

    data <- filtered_data() %>%
      filter(
        !is.na(Sleep_Hours),
        !is.na(Steps)
      )

    validate(
      need(
        nrow(data) > 0,
        "No valid sleep records match these filters."
      )
    )

    plot <- ggplot(
      data,
      aes(
        x = Sleep_Hours,
        y = Steps,
        colour = Goal_Met,
        text = paste0(
          "Sleep: ", Sleep_Hours, " hours",
          "<br>Steps: ", comma(Steps),
          "<br>Goal met: ", Goal_Met
        )
      )
    ) +
      geom_point(
        alpha = 0.7,
        size = 2
      ) +
      scale_colour_manual(
        values = c(
          "Yes" = APP_COLORS$success,
          "No" = APP_COLORS$warning
        )
      ) +
      labs(
        x = "Sleep (hours)",
        y = "Steps",
        colour = "10k step goal"
      )

    ggplotly(
      plot,
      tooltip = "text"
    ) %>%
      layout(
        margin = list(l = 65, r = 15, b = 75, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Chart 5: Weekday vs weekend
  output$day_plot <- renderPlotly({

    empty_message()

    data <- filtered_data() %>%
      group_by(Day_Type) %>%
      summarise(
        Avg_Steps = mean(Steps, na.rm = TRUE),
        .groups = "drop"
      )

    plot <- ggplot(
      data,
      aes(
        x = Day_Type,
        y = Avg_Steps,
        fill = Day_Type,
        text = paste0(
          Day_Type, ": ",
          comma(round(Avg_Steps)),
          " average steps"
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(
        labels = label_comma()
      ) +
      labs(
        x = NULL,
        y = "Average daily steps"
      )

    ggplotly(
      plot,
      tooltip = "text"
    ) %>%
      layout(
        margin = list(l = 65, r = 15, b = 60, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Chart 6: Heart-rate distribution
  output$heart_plot <- renderPlotly({

    data <- filtered_data() %>%
      filter(!is.na(Avg_Heart_Rate))

    validate(
      need(
        nrow(data) > 0,
        "No heart-rate records match these filters."
      )
    )

    plot <- ggplot(
      data,
      aes(
        x = Avg_Heart_Rate,
        fill = Activity_Type
      )
    ) +
      geom_histogram(
        binwidth = 3,
        position = "identity",
        alpha = 0.55,
        colour = "white"
      ) +
      labs(
        x = "Average heart rate (bpm)",
        y = "Days",
        fill = "Activity"
      )

    ggplotly(plot) %>%
      layout(
        margin = list(l = 65, r = 15, b = 75, t = 15)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })


  # Activity data table
  output$activity_table <- renderDT({

    datatable(
      filtered_data(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "lfrtip"
      )
    )
  })


  # Download filtered records
  output$download_data <- downloadHandler(

    filename = function() {
      paste0(
        "fitness_activity_filtered_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {

      write.csv(
        filtered_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}


# 8. START APPLICATION ----------------------------------------

shinyApp(
  ui = ui,
  server = server
)