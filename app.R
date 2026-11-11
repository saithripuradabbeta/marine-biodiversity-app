# INTERACTIVE MARINE BIODIVERSITY VISUALISATION
# Data Visualization Laboratory Course-Based Project

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(scales)

# ------------------------------------------------------------
# 1. CREATE A DEMONSTRATION DATASET
# ------------------------------------------------------------
set.seed(123)

n <- 500

groups <- c(
  "Fish", "Coral", "Mollusc",
  "Crustacean", "Marine Mammal", "Plankton"
)

regions <- c(
  "Indian Ocean", "Pacific Ocean",
  "Atlantic Ocean", "Southern Ocean"
)

demo_data <- data.frame(
  scientificName = sample(
    paste0("Demo_Species_", sprintf("%02d", 1:40)),
    n, replace = TRUE
  ),
  group = sample(groups, n, replace = TRUE),
  region = sample(regions, n, replace = TRUE),
  year = sample(2015:2025, n, replace = TRUE),
  latitude = runif(n, -60, 60),
  longitude = runif(n, -180, 180),
  depth_m = runif(n, 0, 1000),
  observation_count = sample(1:20, n, replace = TRUE),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. PREPARE AND VALIDATE THE DATA
# ------------------------------------------------------------
required_columns <- c(
  "scientificName", "group", "region", "year",
  "latitude", "longitude", "depth_m"
)

prepare_data <- function(data) {

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  # Convert values to suitable data types.
  data$scientificName <- as.character(data$scientificName)
  data$group  <- as.character(data$group)
  data$region <- as.character(data$region)

  data$year      <- suppressWarnings(as.numeric(as.character(data$year)))
  data$latitude  <- suppressWarnings(as.numeric(as.character(data$latitude)))
  data$longitude <- suppressWarnings(as.numeric(as.character(data$longitude)))
  data$depth_m   <- suppressWarnings(as.numeric(as.character(data$depth_m)))

  # Replace missing text categories with Unknown.
  for (column in c("scientificName", "group", "region")) {
    missing_value <- is.na(data[[column]]) | trimws(data[[column]]) == ""
    data[[column]][missing_value] <- "Unknown"
  }

  # Retain only valid calendar years.
  data$year[
    !is.na(data$year) & (data$year < 1000 | data$year > 2100)
  ] <- NA_real_

  # Keep only valid geographic coordinates.
  invalid_coordinates <-
    is.na(data$latitude) |
    is.na(data$longitude) |
    data$latitude < -90 |
    data$latitude > 90 |
    data$longitude < -180 |
    data$longitude > 180

  data$latitude[invalid_coordinates]  <- NA_real_
  data$longitude[invalid_coordinates] <- NA_real_

  # Negative depths are not valid for this analysis.
  data$depth_m[!is.na(data$depth_m) & data$depth_m < 0] <- NA_real_

  # Add optional organism count if it is absent.
  # NA means unavailable, not zero.
  if (!"observation_count" %in% names(data)) {
    data$observation_count <- NA_real_
  }

  data$observation_count <- suppressWarnings(
    as.numeric(as.character(data$observation_count))
  )

  data$observation_count[
    !is.na(data$observation_count) & data$observation_count < 0
  ] <- NA_real_

  data
}

demo_data <- prepare_data(demo_data)

# ------------------------------------------------------------
# 3. USER INTERFACE
# ------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Marine Biodiversity"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",      tabName = "overview", icon = icon("chart-bar")),
      menuItem("Distributions", tabName = "dist",     icon = icon("chart-area")),
      menuItem("Map",           tabName = "map",      icon = icon("globe")),
      menuItem("Data",          tabName = "data",     icon = icon("table"))
    ),
    hr(),
    fileInput("file", "Upload CSV (optional)", accept = ".csv"),
    uiOutput("group_ui"),
    uiOutput("region_ui"),
    uiOutput("year_ui"),
    uiOutput("depth_ui")
  ),

  dashboardBody(
    tabItems(

      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("box_records", width = 3),
          valueBoxOutput("box_species", width = 3),
          valueBoxOutput("box_groups",  width = 3),
          valueBoxOutput("box_depth",   width = 3)
        ),
        fluidRow(
          box(title = "Records by group", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("group_plot", height = 320)),
          box(title = "Records per year by region", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("trend_plot", height = 320))
        ),
        fluidRow(
          box(title = "Group by region (record counts)", width = 12,
              solidHeader = TRUE, status = "primary",
              plotOutput("heat_plot", height = 320))
        )
      ),

      tabItem(
        tabName = "dist",
        fluidRow(
          box(width = 12,
              sliderInput("bins", "Number of histogram bins",
                          min = 5, max = 50, value = 20))
        ),
        fluidRow(
          box(title = "Depth distribution", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("depth_hist", height = 350)),
          box(title = "Depth by group", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("depth_box", height = 350))
        )
      ),

      tabItem(
        tabName = "map",
        fluidRow(
          box(title = "Observation locations", width = 12,
              solidHeader = TRUE, status = "primary",
              plotOutput("map_plot", height = 520))
        )
      ),

      tabItem(
        tabName = "data",
        fluidRow(
          box(width = 12,
              downloadButton("download", "Download filtered data (CSV)"),
              br(), br(),
              DTOutput("table"))
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Data source: uploaded CSV or demo data ----
  raw_data <- reactive({
    if (is.null(input$file)) return(demo_data)

    df <- tryCatch(
      read.csv(input$file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    validate(need(!is.null(df), "The CSV file could not be read."))

    out <- tryCatch(prepare_data(df), error = function(e) e)
    if (inherits(out, "error")) {
      validate(need(FALSE, conditionMessage(out)))
    }
    out
  })

  # ---- Dynamic filter controls ----
  output$group_ui <- renderUI({
    choices <- sort(unique(raw_data()$group))
    selectInput("group", "Group", choices = choices,
                selected = choices, multiple = TRUE)
  })

  output$region_ui <- renderUI({
    choices <- sort(unique(raw_data()$region))
    selectInput("region", "Region", choices = choices,
                selected = choices, multiple = TRUE)
  })

  output$year_ui <- renderUI({
    years <- raw_data()$year
    if (all(is.na(years))) return(NULL)
    rng <- range(years, na.rm = TRUE)
    sliderInput("year", "Year", min = rng[1], max = rng[2],
                value = rng, step = 1, sep = "")
  })

  output$depth_ui <- renderUI({
    depth <- raw_data()$depth_m
    if (all(is.na(depth))) return(NULL)
    rng <- floor(range(depth, na.rm = TRUE))
    sliderInput("depth", "Depth (m)", min = rng[1], max = rng[2],
                value = rng)
  })

  # ---- Filtered data ----
  # Note: when a slider is active, rows with a missing value in that
  # column are excluded.
  filtered <- reactive({
    req(input$group, input$region)
    d <- raw_data() %>%
      filter(group %in% input$group, region %in% input$region)

    if (!is.null(input$year)) {
      d <- d %>% filter(!is.na(year),
                        year >= input$year[1], year <= input$year[2])
    }
    if (!is.null(input$depth)) {
      d <- d %>% filter(!is.na(depth_m),
                        depth_m >= input$depth[1], depth_m <= input$depth[2])
    }
    d
  })

  has_rows <- function(d) {
    validate(need(nrow(d) > 0, "No records match the current filters."))
  }

  # ---- Value boxes ----
  output$box_records <- renderValueBox({
    valueBox(format(nrow(filtered()), big.mark = ","),
             "Records", icon = icon("database"), color = "blue")
  })

  output$box_species <- renderValueBox({
    valueBox(n_distinct(filtered()$scientificName),
             "Species", icon = icon("fish"), color = "teal")
  })

  output$box_groups <- renderValueBox({
    valueBox(n_distinct(filtered()$group),
             "Groups", icon = icon("layer-group"), color = "purple")
  })

  output$box_depth <- renderValueBox({
    m <- mean(filtered()$depth_m, na.rm = TRUE)
    valueBox(if (is.nan(m)) "N/A" else paste0(round(m), " m"),
             "Mean depth", icon = icon("water"), color = "navy")
  })

  # ---- Overview plots ----
  output$group_plot <- renderPlot({
    d <- filtered(); has_rows(d)
    d %>%
      count(group) %>%
      ggplot(aes(x = reorder(group, n), y = n, fill = group)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_viridis_d() +
      labs(x = NULL, y = "Number of records") +
      theme_minimal(base_size = 13)
  })

  output$trend_plot <- renderPlot({
    d <- filtered() %>% filter(!is.na(year)); has_rows(d)
    d %>%
      count(year, region) %>%
      ggplot(aes(x = year, y = n, color = region)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = breaks_pretty()) +
      scale_color_viridis_d(end = 0.9) +
      labs(x = "Year", y = "Number of records", color = "Region") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$heat_plot <- renderPlot({
    d <- filtered(); has_rows(d)
    d %>%
      count(group, region) %>%
      complete(group, region, fill = list(n = 0)) %>%
      ggplot(aes(x = region, y = group, fill = n)) +
      geom_tile(color = "white") +
      geom_text(aes(label = n), color = "white") +
      scale_fill_viridis_c(option = "mako", direction = -1) +
      labs(x = NULL, y = NULL, fill = "Records") +
      theme_minimal(base_size = 13)
  })

  # ---- Distribution plots ----
  output$depth_hist <- renderPlot({
    d <- filtered() %>% filter(!is.na(depth_m)); has_rows(d)
    ggplot(d, aes(x = depth_m)) +
      geom_histogram(bins = input$bins, fill = "#2C7FB8", color = "white") +
      labs(x = "Depth (m)", y = "Number of records") +
      theme_minimal(base_size = 13)
  })

  output$depth_box <- renderPlot({
    d <- filtered() %>% filter(!is.na(depth_m)); has_rows(d)
    ggplot(d, aes(x = group, y = depth_m, fill = group)) +
      geom_boxplot(show.legend = FALSE) +
      scale_fill_viridis_d(alpha = 0.7) +
      labs(x = NULL, y = "Depth (m)") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # ---- Map ----
  output$map_plot <- renderPlot({
    d <- filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    has_rows(d)
    ggplot(d, aes(x = longitude, y = latitude, color = group)) +
      geom_point(alpha = 0.6, size = 2) +
      scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 60)) +
      scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
      scale_color_viridis_d(end = 0.9) +
      coord_fixed() +
      labs(x = "Longitude", y = "Latitude", color = "Group") +
      theme_minimal(base_size = 13)
  })

  # ---- Table and download ----
  output$table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("marine_biodiversity_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
}

# ------------------------------------------------------------
# 5. RUN THE APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
