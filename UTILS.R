data_library <- function(data) {
  # First, rename the basic columns that are common across all project types
  transformed_data <- data %>%
    rename(
      timestamp = Timestamp,
      project_name = `What is the name of the project?`,
      project_description = `Short project description (max 50 words)`,
      project_type = `My project is a:`
    ) 
  
  # Create new columns based on project type with flexible matching
  transformed_data <- transformed_data %>%
    mutate(
      # Handle what is being forecasted/modeled/monitored - with exact column names from the Excel file
      target_variable = case_when(
        grepl("Forecasting", project_type) ~ `What is being forecasted?`,
        grepl("Modeling", project_type) ~ `What is being modeled? (What are the response variables?)`,
        grepl("Observatory", project_type) ~ `In a potential forecasting application, what would you like to forecast?`,
        TRUE ~ NA_character_
      ),
      
      # New fields for category, level, and unit with exact column names
      target_category = case_when(
        grepl("Forecasting", project_type) ~ `Can you specify the category of what is being forecasted? (if applicable)`,
        grepl("Modeling", project_type) ~ `Can you specify the category of what is being modeled (if applicable)`,
        grepl("Observatory", project_type) ~ `Can you specify the category of what is being monitored (if applicable)`,
        TRUE ~ NA_character_
      ),
      
      target_level = case_when(
        grepl("Forecasting", project_type) ~ `Can you specify the level of what is being forecasted? (if applicable)\n(This would be the output of the model, e.g. population abundance from an IBM would be “Population-level”)`,
        grepl("Modeling", project_type) ~ `Can you specify the level of what is being modeled? (if applicable)\n(This would be the output of the model, e.g. population abundance from an IBM would be “Population-level”)`,
        grepl("Observatory", project_type) ~ `Can you specify the level of what is being monitored? (if applicable)`,
        TRUE ~ NA_character_
      ),
      
      target_unit = case_when(
        grepl("Forecasting", project_type) ~ `Can you specify the unit of what is being forecasted? (if applicable)`,
        grepl("Modeling", project_type) ~ `Can you specify the unit of what is being modeled? (if applicable)`,
        grepl("Observatory", project_type) ~ `Can you specify the unit of what is being monitored? (if applicable)`,
        TRUE ~ NA_character_
      ),
      
      # Explanatory variables
      explanatory_variables = case_when(
        grepl("Forecasting", project_type) ~ `What are the explanatory/input variables of the forecasting model?`,
        grepl("Modeling", project_type) ~ `What are the explanatory/input variables of the model?`,
        grepl("Observatory", project_type) ~ `What variables are currently being monitored?`,
        TRUE ~ NA_character_
      ),
      
      # Model type - note the \n in column names
      model_type = case_when(
        grepl("Forecasting", project_type) ~ `What type of model is used?`,
        grepl("Modeling", project_type) ~ `What type of model is used in your application?`,
        TRUE ~ NA_character_
      ),
      
      model_type = case_when(
        model_type == "Statistical models (e.g., species distribution models [SDMs], joint species distribution models [JSDMs])"~"Statistical models",
        model_type == "Machine learning / AI models (e.g., deep learning, random forests)" ~"Machine learning / AI",
        model_type == "Process-based models (e.g., dynamic vegetation models like LPJ-GUESS)" ~ "Process-based models",
        is.na(model_type) ~ NA,
        .default = model_type
      ),
      
      # Start year - with explicit type conversion
      start_year = case_when(
        grepl("Forecasting", project_type) ~ as.character(`What is the start year of data collection for your forecasting application?`),
        grepl("Modeling", project_type) ~ as.character(`What is the start year of data collection for your modeling application?`),
        grepl("Observatory", project_type) ~ as.character(`What is the start year of data collection for your observatory?`),
        TRUE ~ NA_character_
      ),
      
      # Data collection frequency
      data_frequency = case_when(
        grepl("Forecasting", project_type) ~ `How frequently are data collected for your forecasting application?`,
        grepl("Modeling", project_type) ~ `How frequently are data collected for your modeling application?`,
        grepl("Observatory", project_type) ~ `How frequently are data collected in your observatory?`,
        TRUE ~ NA_character_
      ),
      
      # Use case
      use_case = case_when(
        grepl("Forecasting", project_type) ~ `Please provide an example use case for the forecasts (max 50 words). (e.g., use of forecasts to inform authorities, decision-making, conservation strategies, management)`,
        grepl("Modeling", project_type) ~ `Please provide an example use case for a potential forecasting application (max 50 words). (e.g., underderstanding the ecological processes, advancing the ecological theory, use of forecasts to inform authorities, decision-making, conservation strategies, management)`,
        TRUE ~ NA_character_
      ),
      
      # Forecasting suitability
      forecasting_suitability = case_when(
        grepl("Forecasting", project_type) ~ `What makes your modeling application suitable for ecological forecasting?`,
        grepl("Modeling", project_type) ~ `What makes your modeling application suitable for ecological forecasting?`,
        grepl("Observatory", project_type) ~ `What makes your observatory suitable for ecological forecasting?`,
        TRUE ~ NA_character_
      ),
      
      # Forecasting horizon
      forecasting_horizon = if_else(
        grepl("Forecasting", project_type), 
        as.character(`What is the forecasting horizon? (choose closest)`), 
        NA_character_
      ),
      
      # Challenges (only for Observatory)
      forecasting_challenges = if_else(
        grepl("Observatory", project_type), 
        as.character(`What are the main challenges you face in developing a forecasting application?\n(This will help us identify potential collaborators to support you.)`), 
        NA_character_
      )
    )
  
  # Handle location and contact information
  transformed_data <- transformed_data %>%
    mutate(
      research_location = `Please provide the coordinates of the research location (Please use Google Maps: right-click on location on the map and copy coordinates).\n(e.g. 60.294264172757536, 22.390893932124552)\nThese coordinates will be used to visualise your project on the map view, so provide coordinates that you think captures the project the best.`,
      forecast_location = `Please provide the forecast coordinates (if different).`,
      project_url = `Project URL (if available)`,
      interactive_url = `Interactive website URL (if available)`,
      principal_investigator = `Who is the Principal Investigator (PI) of the project?`,
      contact_person = `Who is the main contact person for this form response? (If different from PI)`,
      contact_institute = `Contact institute`,
      contact_email = `Contact email`,
      share_email = `Would you like us to share this contact email on our website, database and ShinyApp?`,
      publications = `If any, associated publications (DOIs only)`,
      data_consent = `Data use consent:\nIf you choose the second option, please specify below what information you would prefer not to be shared.`,
      not_share_info = `Please specify what information you do not want us to share:`,
      approved = approved
    )
  
  # Extract latitude and longitude from the coordinates
  transformed_data <- transformed_data %>%
    mutate(
      # Trim whitespace and convert to character
      research_location_clean = trimws(as.character(research_location)),
      
      # Initialize latitude and longitude with NA
      latitude = NA_real_,
      longitude = NA_real_
    )
  
  # Process coordinates row by row with error handling
  for (i in 1:nrow(transformed_data)) {
    if (!is.na(transformed_data$research_location_clean[i]) && 
        transformed_data$research_location_clean[i] != "") {
      
      coords <- strsplit(transformed_data$research_location_clean[i], ",\\s*")[[1]]
      
      if (length(coords) >= 1) {
        transformed_data$latitude[i] <- tryCatch(
          as.numeric(trimws(coords[1])),
          error = function(e) NA_real_
        )
      }
      
      if (length(coords) >= 2) {
        transformed_data$longitude[i] <- tryCatch(
          as.numeric(trimws(coords[2])),
          error = function(e) NA_real_
        )
      }
    }
  }
  
  # Remove the temporary cleaning column
  transformed_data <- transformed_data %>% 
    select(-research_location_clean)
  
  # Select the final columns in a logical order
  transformed_data <- transformed_data %>%
    select(
      timestamp, project_name, project_description, project_type,
      target_variable, target_category, target_level, target_unit,
      explanatory_variables, model_type,
      start_year, data_frequency, use_case, forecasting_suitability,
      forecasting_horizon, forecasting_challenges,
      research_location, forecast_location, latitude, longitude,
      project_url, interactive_url, principal_investigator, contact_person,
      contact_institute, contact_email, share_email, publications,
      data_consent, not_share_info, approved
    )
  transformed_data = 
    transformed_data %>%
    mutate(data_consent = ifelse(data_consent == "I consent to all the information I provided being shared on EEFI websites.",
                                 TRUE, FALSE
                                 ))
  
  return(transformed_data)
}