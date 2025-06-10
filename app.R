# app.R
# EEFI European Ecological Forecasting Application Database Shiny App

# Load required libraries
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(leaflet)
library(DT)
library(dplyr)
library(htmltools)
library(shinyjs)
library(tidyverse)

source("UTILS.R")

# Set up for reading a public Google Sheet - no authentication needed
# This allows reading from any Google Sheet set to "anyone with link can view"
gs4_deauth()

# Define spreadsheet ID - replace with your actual spreadsheet ID
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1HoKfDf0e0mwUIYJ2u4oPk0JnWdC7x_VAjyNfn9y17FY/edit?usp=sharing"
FORM_ID <- "https://docs.google.com/forms/d/e/1FAIpQLSc6qn_UQNvXDXNuc_lXgy9pfiyYgUYpemhQIXOI4eRnnZZxPQ/viewform?embedded=true"

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "EEFI European Ecological Forecasting Application Database", 
                  titleWidth = 550),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "landing", icon = icon("home")),
      menuItem("Glossary & FAQ", tabName = "glossary", icon = icon("book")),
      menuItem("Map View", tabName = "map", icon = icon("map")),
      menuItem("Table View", tabName = "view", icon = icon("table")),
      menuItem("Submit Application", tabName = "submit", icon = icon("edit"))
    ),
    tags$style(HTML("
      .skin-blue .main-header .logo {
        background-color: #009688;
        font-size: 18px;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #00897b;
      }
      .skin-blue .main-header .navbar {
        background-color: #009688;
      }
      .skin-blue .main-sidebar {
        background-color: #2c3b41;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #00796b;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        font-size: 18px;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #004d40;
      }
      body {
        font-size: 16px;
      }
      .box-title {
        font-size: 20px;
      }
      h1, h2, h3, h4 {
        font-size: 120%;
      }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter {
        font-size: 16px;
      }
      .selectize-input {
        font-size: 16px;
      }
      .control-label {
        font-size: 16px;
      }
      .leaflet-container {
        font-size: 16px;
      }
      .leaflet-popup-content {
        font-size: 15px;
      }
      /* Hide hamburger menu */
      .sidebar-toggle {
        display: none !important;
      }
    "))
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .box.box-primary {
          border-top-color: #009688;
        }
        .btn-primary {
          background-color: #009688;
          border-color: #00796b;
        }
        .btn-primary:hover, .btn-primary:active, .btn-primary:focus {
          background-color: #00796b;
          border-color: #004d40;
        }
        .pagination>.active>a, .pagination>.active>a:focus, 
        .pagination>.active>a:hover, .pagination>.active>span, 
        .pagination>.active>span:focus, .pagination>.active>span:hover {
          background-color: #009688;
          border-color: #009688;
        }
        .leaflet-container a.leaflet-popup-close-button {
          color: #009688;
        }
        .tabbable > .nav > li > a {
          background-color: #e0f2f1;
          color: #004d40;
        }
        .tabbable > .nav > li[class=active] > a {
          background-color: #009688;
          color: #fff;
        }
        .landing-content {
          text-align: center;
          padding: 20px;
        }
        .landing-content h2 {
          color: #009688;
          margin-bottom: 30px;
        }
        .landing-content p {
          font-size: 18px;
          line-height: 1.6;
          margin-bottom: 20px;
          max-width: 800px;
          margin-left: auto;
          margin-right: auto;
        }
        .eefi-logo {
          max-width: 300px;
          margin-bottom: 20px;
        }
      "))
    ),
    tabItems(
      # Landing tab
      tabItem(
        tabName = "landing",
        fluidRow(
          box(
            title = "",
            width = 12,
            div(class = "landing-content",
                # Placeholder for EEFI logo - replace with actual logo path
                img(src = "eefi_logo.png", class = "eefi-logo", alt = "EEFI Logo"),
                h2("European Ecological Forecasting Initiative"),
                p("The European Ecological Forecasting Initiative (EEFI) Chapter is developing the first comprehensive database of ecological forecasting applications in Europe. This app showcases ongoing projects, models, and observatories with forecasting potential. You can explore existing submissions through interactive table and map views—or contribute your own!"),
                p(tags$strong("Website: "), tags$a("https://euro-ecoforecast.wordpress.com/", href = "https://euro-ecoforecast.wordpress.com/", target = "_blank")),
                p(tags$strong("Email: "), tags$a("info@ecoforecast.org", href = "mailto:info@ecoforecast.org")),
                p(tags$strong("Connect: "), tags$a("https://euro-ecoforecast.wordpress.com/connect/", href = "https://euro-ecoforecast.wordpress.com/connect/", target = "_blank"))
           )
          )
        )
      ),
      
      # Glossary tab
      tabItem(
        tabName = "glossary",
        fluidRow(
          box(
            title = "Glossary and Frequently Asked Questions",
            width = 12,
            div(
              h3("What is an ecological forecast?"),
              p("An ecological forecast is a specific, quantitative prediction about a future ecological state, preferably including an uncertainty estimate—such as species distributions, population sizes, or ecosystem functions—based on data and models. These forecasts help inform decision-making under changing environmental conditions."),
              
              h3("What is a near-term iterative forecast?"),
              p("A near-term iterative forecast is a prediction made for the coming days, weeks, months, or years that is updated regularly as new data become available. These forecasts are typically evaluated and refined over time, improving their accuracy and utility for decision-making in real time."),
              
              h3("What is a modeling application with forecasting potential?"),
              p("This refers to any ecological model that could be used to make predictions about the future, even if it hasn't yet been used operationally for forecasting. For a modeling application to become a forecasting application, it typically requires open-access models, accessible input data, and a continuous or regular flow of updated data to generate and refine forecasts over time."),
              
              h3("What is an observatory with forecasting potential?"),
              p("An observatory with forecasting potential is a monitoring system or data-collection network that generates ecological or environmental data which could support future forecasts. This includes long-term ecological research sites, sensor networks, citizen science platforms, or remote sensing programs. If the data collected can be regularly updated and linked to models, the observatory can contribute to near-term iterative forecasting efforts."),
              
              h3("What is a forecasting horizon?"),
              p("The forecasting horizon refers to the time period over which a forecast is made. It indicates how far into the future the model or prediction aims to project."),
              
              h3("Who is EEFI?"),
              p("The European Ecological Forecasting Initiative (EEFI; ", tags$a("https://euro-ecoforecast.wordpress.com/", href = "https://euro-ecoforecast.wordpress.com/", target = "_blank"), ") is an open, collaborative network of researchers and practitioners working to advance ecological forecasting in Europe. Our goal is to promote transparency, coordination, and innovation across forecasting efforts and to build a central hub for sharing knowledge and resources."),
              
              h3("How will my form responses be used?"),
              p("The information you provide will be reviewed and, if approved, included in EEFI's public database. This database will be featured on our website to highlight the diversity of ecological forecasting efforts across Europe. You can choose to limit what information is shared (see consent question in the form)."),
              
              h3("Can I change the form responses that I provided?"),
              p("Yes! After submission, you can request to update or correct your entry at any time. Simply contact us via ", tags$strong(tags$a("info@ecoforecast.org", href = "mailto:info@ecoforecast.org"))),
              
              h3("Can I delete the form responses that I provided?"),
              p("Absolutely. If you wish to withdraw your data from the database, simply contact us via ", tags$strong(tags$a("info@ecoforecast.org", href = "mailto:info@ecoforecast.org")))
            )
          )
        )
      ),
      
      # Map view tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "EEFI European Ecological Forecasting Application Database",
            width = 12,
            leafletOutput("applicationsMap", height = 700)
          )
        )
      ),
      
      # Table view tab with filters
      tabItem(
        tabName = "view",
        fluidRow(
          box(
            title = "Filter Options",
            width = 12,
            column(
              width = 3,
              selectInput("projectTypeFilter", "Project Type:", choices = NULL, multiple = TRUE)
            ),
            column(
              width = 3,
              selectInput("modelTypeFilter", "Model Type:", choices = NULL, multiple = TRUE)
            ),
            column(
              width = 3,
              selectInput("categoryFilter", "Target Category:", choices = NULL, multiple = TRUE)
            ),
            column(
              width = 3,
              selectInput("levelFilter", "Target Level:", choices = NULL, multiple = TRUE)
            ),
            column(
              width = 12,
              actionButton("resetFilters", "Reset Filters", class = "btn-primary", 
                           style = "background-color: #009688; border-color: #00796b;")
            )
          )
        ),
        fluidRow(
          box(
            title = "Applications Database",
            width = 12,
            DTOutput("applicationsTable")
          )
        )
      ),
      
      # Submit application tab
      tabItem(
        tabName = "submit",
        fluidRow(
          box(
            title = "Application Form",
            width = 12,
            # Embed Google Form
            tags$iframe(
              id = "googleform",
              src = FORM_ID,
              width = "100%",
              height = 800,
              frameborder = 0,
              marginheight = 0
            )
          )
        )
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  
  # Reactive function to load data from Google Sheets and apply the transformation
  allApplicationData <- reactive({
    # Try to read data from Google Sheet
    tryCatch({
      raw_data <- read_sheet(SHEET_ID)
      
      # Apply the data_library transformation function
      if(!is.null(raw_data) && nrow(raw_data) > 0) {
        transformed_data <- data_library(raw_data)
        return(transformed_data)
      } else {
        # For demonstration, create sample data if no connection
        sample_data <- data.frame(
          timestamp = as.POSIXct(c("2025-04-01 10:30:00", "2025-04-02 14:45:00", "2025-04-03 09:15:00", "2025-04-04 15:20:00")),
          project_name = c("Alpine Biodiversity", "Mediterranean Forest Health", "African Savanna Wildlife", "Coastal Ecosystem Dynamics"),
          project_description = c("Tracking alpine plant biodiversity changes", 
                                  "Monitoring forest health in Mediterranean regions", 
                                  "Wildlife population dynamics in savanna ecosystems",
                                  "Coastal ecosystem responses to climate change"),
          project_type = c("Forecasting application", "Modeling application with forecasting potential", "Observatory with forecasting potential", "Forecasting application"),
          model_type = c("Machine Learning, Statistical", "Process-based, Statistical", "Machine Learning", "Process-based, Machine Learning"),
          target_variable = c("plant functional traits", "forest health indices", "wildlife population sizes", "coastal biodiversity metrics"),
          target_category = c("Plants", "Plants, Ecosystem functions", "Animals", "Plants, Animals, Ecosystem properties"),
          target_level = c("Species-level", "Community-level", "Population-level", "Individual-level, Community-level"),
          target_unit = c("traits", "health indices", "population counts", "abundance metrics"),
          principal_investigator = c("Dr. Anna Schmidt", "Prof. Marco Rossi", "Dr. Jamal Okafor", "Dr. Sophia Chen"),
          contact_institute = c("Alpine Research Institute", "Mediterranean Ecology Center", "African Wildlife Foundation", "Coastal Research Lab"),
          contact_email = c("a.schmidt@alpine.org", "m.rossi@medecocenter.eu", "j.okafor@awf.org", "s.chen@coastal.edu"),
          project_url = c("https://alpine-biodiversity.org", "https://medforest-health.eu", "https://savanna-wildlife.org", "https://coastal-ecosystems.org"),
          publications = c("10.1038/s41559-021-01584-1", "10.1016/j.ecolind.2022.109358", "10.1111/1365-2656.13795", "10.1038/s41598-022-15475-3"),
          latitude = c(46.900614, 41.902782, -1.292066, 35.632118),
          longitude = c(9.450042, 12.496366, 36.821946, -5.291232),
          data_consent = c(TRUE, TRUE, FALSE, TRUE),
          approved = c(TRUE, TRUE, TRUE, FALSE)
        )
        return(sample_data)
      }
    }, error = function(e) {
      # Return sample data if there's an error
      sample_data <- data.frame(
        timestamp = as.POSIXct(c("2025-04-01 10:30:00", "2025-04-02 14:45:00", "2025-04-03 09:15:00", "2025-04-04 15:20:00")),
        project_name = c("Alpine Biodiversity", "Mediterranean Forest Health", "African Savanna Wildlife", "Coastal Ecosystem Dynamics"),
        project_description = c("Tracking alpine plant biodiversity changes", 
                                "Monitoring forest health in Mediterranean regions", 
                                "Wildlife population dynamics in savanna ecosystems",
                                "Coastal ecosystem responses to climate change"),
        project_type = c("Forecasting application", "Modeling application with forecasting potential", "Observatory with forecasting potential", "Forecasting application"),
        model_type = c("Machine Learning, Statistical", "Process-based, Statistical", "Machine Learning", "Process-based, Machine Learning"),
        target_variable = c("plant functional traits", "forest health indices", "wildlife population sizes", "coastal biodiversity metrics"),
        target_category = c("Plants", "Plants, Ecosystem functions", "Animals", "Plants, Animals, Ecosystem properties"),
        target_level = c("Species-level", "Community-level", "Population-level", "Individual-level, Community-level"),
        target_unit = c("traits", "health indices", "population counts", "abundance metrics"),
        principal_investigator = c("Dr. Anna Schmidt", "Prof. Marco Rossi", "Dr. Jamal Okafor", "Dr. Sophia Chen"),
        contact_institute = c("Alpine Research Institute", "Mediterranean Ecology Center", "African Wildlife Foundation", "Coastal Research Lab"),
        contact_email = c("a.schmidt@alpine.org", "m.rossi@medecocenter.eu", "j.okafor@awf.org", "s.chen@coastal.edu"),
        project_url = c("https://alpine-biodiversity.org", "https://medforest-health.eu", "https://savanna-wildlife.org", "https://coastal-ecosystems.org"),
        publications = c("10.1038/s41559-021-01584-1", "10.1016/j.ecolind.2022.109358", "10.1111/1365-2656.13795", "10.1038/s41598-022-15475-3"),
        latitude = c(46.900614, 41.902782, -1.292066, 35.632118),
        longitude = c(9.450042, 12.496366, 36.821946, -5.291232),
        data_consent = c(TRUE, TRUE, FALSE, TRUE),
        approved = c(TRUE, TRUE, TRUE, FALSE)
      )
      return(sample_data)
    })
  })
  
  # Filter for only approved and consented applications
  applicationData <- reactive({
    data <- allApplicationData()
    
    # Only include applications with consent and approval
    filtered_data <- data %>%
      filter(data_consent == TRUE & approved == TRUE)
    
    return(filtered_data)
  })
  
  # Update filter choices when data is loaded
  observe({
    data <- applicationData()
    
    # Update project type filter
    updateSelectInput(session, "projectTypeFilter", 
                      choices = c("All", sort(unique(na.omit(data$project_type)))),
                      selected = "All")
    
    # Update model type filter
    all_models <- unique(unlist(lapply(na.omit(data$model_type), function(x) {
      trimws(strsplit(x, ",")[[1]])
    })))
    updateSelectInput(session, "modelTypeFilter", 
                      choices = c("All", sort(all_models)),
                      selected = "All")
    
    # Update category filter - split categories into individual options
    all_categories <- unique(unlist(lapply(na.omit(data$target_category), function(x) {
      trimws(strsplit(x, ",")[[1]])
    })))
    updateSelectInput(session, "categoryFilter", 
                      choices = c("All", sort(all_categories)),
                      selected = "All")
    
    # Update level filter - split levels into individual options
    all_levels <- unique(unlist(lapply(na.omit(data$target_level), function(x) {
      trimws(strsplit(x, ",")[[1]])
    })))
    updateSelectInput(session, "levelFilter", 
                      choices = c("All", sort(all_levels)),
                      selected = "All")
  })
  
  # Filter the data based on user inputs
  filteredData <- reactive({
    data <- applicationData()
    
    # Filter by project type
    if(!is.null(input$projectTypeFilter) && !("All" %in% input$projectTypeFilter) && length(input$projectTypeFilter) > 0) {
      data <- data[data$project_type %in% input$projectTypeFilter, ]
    }
    
    # Filter by model type
    if(!is.null(input$modelTypeFilter) && !("All" %in% input$modelTypeFilter) && length(input$modelTypeFilter) > 0) {
      # Handle multiple model types in a single field (comma-separated)
      has_any_model <- function(models_str) {
        if(is.na(models_str)) return(FALSE)
        models <- trimws(strsplit(models_str, ",")[[1]])
        return(any(input$modelTypeFilter %in% models))
      }
      
      # Apply the filter
      data <- data %>%
        filter(sapply(model_type, has_any_model))
    }
    
    # Filter by target category
    if(!is.null(input$categoryFilter) && !("All" %in% input$categoryFilter) && length(input$categoryFilter) > 0) {
      # Create a function to check if any selected category is present in a string
      has_any_category <- function(categories_str) {
        if(is.na(categories_str)) return(FALSE)
        categories <- trimws(strsplit(categories_str, ",")[[1]])
        return(any(input$categoryFilter %in% categories))
      }
      
      # Apply the filter
      data <- data %>%
        filter(sapply(target_category, has_any_category))
    }
    
    # Filter by target level
    if(!is.null(input$levelFilter) && !("All" %in% input$levelFilter) && length(input$levelFilter) > 0) {
      # Create a function to check if any selected level is present in a string
      has_any_level <- function(levels_str) {
        if(is.na(levels_str)) return(FALSE)
        levels <- trimws(strsplit(levels_str, ",")[[1]])
        return(any(input$levelFilter %in% levels))
      }
      
      # Apply the filter
      data <- data %>%
        filter(sapply(target_level, has_any_level))
    }
    
    return(data)
  })
  
  # Reset filters button
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "projectTypeFilter", selected = "All")
    updateSelectInput(session, "modelTypeFilter", selected = "All")
    updateSelectInput(session, "categoryFilter", selected = "All")
    updateSelectInput(session, "levelFilter", selected = "All")
  })
  
  # Render the applications data table
  output$applicationsTable <- renderDT({
    data <- filteredData()
    
    # Select columns to display in the table
    display_data <- data %>%
      select(project_name, project_description, project_type, model_type,
             target_variable, target_category, target_level, target_unit,
             principal_investigator, contact_institute, contact_email,
             project_url, publications)
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Render the applications map
  output$applicationsMap <- renderLeaflet({
    data <- filteredData()
    
    # Create the base map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%  # Centered on Europe
      addProviderTiles("CartoDB.Positron")  # Clean, light basemap
    
    # Check if we have location data
    if(nrow(data) > 0 && "latitude" %in% colnames(data) && "longitude" %in% colnames(data)) {
      # Remove rows with NA coordinates
      data_with_coords <- data %>%
        filter(!is.na(latitude) & !is.na(longitude))
      
      # Add markers for each application
      if(nrow(data_with_coords) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = data_with_coords,
            lng = ~longitude,
            lat = ~latitude,
            popup = ~paste(
              "<div style='min-width: 250px;'>",
              "<h4 style='color: #009688; margin-top: 0;'>", project_name, "</h4>",
              "<strong>Description:</strong> ", project_description, "<br>",
              "<strong>Project Type:</strong> ", project_type, "<br>",
              "<strong>Model Type:</strong> ", model_type, "<br>",
              "<strong>Target Variable:</strong> ", target_variable, "<br>",
              "<strong>Category:</strong> ", target_category, "<br>",
              "<strong>Level:</strong> ", target_level, "<br>",
              "<strong>PI:</strong> ", principal_investigator, "<br>",
              "<strong>Institute:</strong> ", contact_institute, "<br>",
              "<strong>Contact:</strong> ", contact_email, "<br>",
              ifelse(!is.na(project_url) & project_url != "", 
                     paste0("<strong>URL:</strong> <a href='", project_url, "' target='_blank'>Project Website</a><br>"), ""),
              ifelse(!is.na(publications) & publications != "", 
                     paste0("<strong>References:</strong> ", publications), ""),
              "</div>"
            ),
            layerId = ~rownames(data_with_coords),
            radius = 8,
            color = "#009688",
            stroke = TRUE,
            fillOpacity = 0.8,
            weight = 2,
            fillColor = "#009688"
          )
      }
    }
    
    return(map)
  })
}

# Run the application
shinyApp(ui = ui, server = server)