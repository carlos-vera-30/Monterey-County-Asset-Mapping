library(tidyverse)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets4)
library(leaflet.providers)
library(httr)
library(bslib)
library(tidygeocoder)
library(geosphere) # For distance calculations
library(shinyjs) # For showing/hiding elements

# Define %||% operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Define color mapping for categories - MOVED TO TOP FOR CONSISTENCY
category_colors <- c(
  "Adult Disability Resources" = "#1E88E5",        # bright blue
  "Benefits" = "#4CAF50",                          # bright green
  "Community Resources" = "#FF7043",               # bright orange
  "Child Care/ Early Education" = "#8E24AA",       # bright purple
  "Credit Bureaus/ Agencias de Informacion Crediticia" = "#E91E63", # bright pink
  "Crisis" = "#F44336",                            # bright red
  "Child Development and Special Needs" = "#29B6F6", # light blue
  "Emergency Shelters" = "#C62828",                # dark red
  "Early Childhood Programs" = "#42A5F5",          # light blue
  "Education (Adult/Children)" = "#2E7D32",        # dark green
  "Education - School Districts" = "#FDD835",      # yellow
  "Employment" = "#7B1FA2",                        # dark purple
  "Farmworker Assistance" = "#66BB6A",             # light green
  "Grief and Loss" = "#3F51B5",                    # indigo
  "Health - Physical Health" = "#E53935",          # red
  "Health - Mental Health" = "#FB8C00",            # orange
  "Home Visiting Programs" = "#757575",            # gray
  "Housing" = "#424242",                           # dark gray
  "Immigration/Citizenship" = "#FFD600",           # bright yellow
  "Infant Resources" = "#F8BBD0",                  # light pink
  "Legal Assistance" = "#1976D2",                  # blue
  "Libraries (Books/Libros, Public Internet Access/ Acceso Publico a La Interenet)" = "#388E3C", # green
  "Parenting Education" = "#81C784",               # light green
  "Parent Support Groups" = "#64B5F6",             # light blue
  "Playgroups" = "#EF5350",                        # light red
  "Recreation" = "#81C784",                        # light green
  "Religious Services" = "#9C27B0",                # purple
  "Seniors" = "#9E9E9E",                           # gray
  "Substance use support/treatment/services" = "#673AB7", # deep purple
  "Children with Special Needs/Autism" = "#4FC3F7", # light blue
  "Teen Programs" = "#D4E157",                     # light green
  "Transportation" = "#26A69A",                    # teal
  "Utility Assistance" = "#2196F3",                # blue
  "Veterans" = "#FF5722",                          # deep orange
  "Victims of Crime Resources" = "#FF9800"         # orange
)

# FIXED: Define the missing get_category_color function
get_category_color <- function(category) {
  color <- category_colors[category]
  # Return default color if category not found
  if (is.na(color) || is.null(color)) {
    return("#607D8B")  # blue-gray default
  }
  return(color)
}

# Google Sheets auth - using a more robust approach
options(gargle_oauth_email = TRUE)
tryCatch({
  # First try with cache
  gs4_auth(cache = ".secrets")
}, error = function(e) {
  message("Trying alternative authentication method...")
  # If cache fails, try interactive authentication
  tryCatch({
    gs4_auth()
  }, error = function(e2) {
    message("Interactive authentication also failed: ", e2$message)
    # If all auth methods fail, we'll use the fallback data in the error handler
  })
})

# Function to read and process data from specific sheet
process_asset_data <- function() {
  tryCatch({
    # The direct link to the spreadsheet
    sheet_url <- "https://docs.google.com/spreadsheets/d/1FekJ3_fj5DZOy9XSgvnnW3dAmqPCNE6SNXEN0qN6Tbs"
    sheet_name <- "By Category/ Por categoria"
    
    # Try to read the sheet by URL first
    message("Attempting to read Google Sheet...")
    df <- read_sheet(sheet_url, sheet = sheet_name)
    
    # List of known categories
    categories <- c(
      "Adult Disability Resources", "Benefits", "Community Resources",
      "Child Care/ Early Education", "Credit Bureaus/ Agencias de Informacion Crediticia",
      "Crisis", "Child Development and Special Needs", "Emergency Shelters",
      "Early Childhood Programs", "Education (Adult/Children)", "Education - School Districts",
      "Employment", "Farmworker Assistance", "Grief and Loss", "Health - Physical Health",
      "Health - Mental Health", "Home Visiting Programs", "Housing", "Immigration/Citizenship",
      "Infant Resources", "Legal Assistance",
      "Libraries (Books/Libros, Public Internet Access/ Acceso Publico a La Interenet)",
      "Parenting Education", "Parent Support Groups", "Playgroups", "Recreation",
      "Religious Services", "Seniors", "Substance use support/treatment/services",
      "Children with Special Needs/Autism", "Teen Programs", "Transportation",
      "Utility Assistance", "Veterans", "Victims of Crime Resources"
    )
    
    # Rename first column to be consistent
    colnames(df)[1] <- "Raw"
    
    # Remove rows where all values are NA or blank
    df <- df %>%
      filter(!if_all(everything(), ~ is.na(.) | . == ""))
    
    # Track and assign categories and agencies
    current_category <- NA
    
    # Create new columns
    df <- df %>%
      mutate(
        Category = NA_character_,
        Agency = NA_character_
      )
    
    # Loop through rows and tag category/agency
    for (i in seq_len(nrow(df))) {
      raw_val <- df$Raw[i]
      
      if (raw_val %in% categories) {
        current_category <- raw_val
      } else if (!is.na(current_category)) {
        df$Category[i] <- current_category
        df$Agency[i] <- raw_val
      }
    }
    
    # Filter to just the rows that are agencies and clean addresses
    cleaned_data <- df %>%
      filter(!is.na(Agency)) %>%
      select(Category, Agency, everything(), -Raw) %>%
      filter(!is.na(`Address/ Domicilio`) | `Address/ Domicilio` != "Online" | `Address/ Domicilio` != "online", 
             `Address/ Domicilio` != "Online") %>%
      filter(!is.na(`Address/ Domicilio`) & grepl("^[0-9]", `Address/ Domicilio`))
    
    # Geocode the addresses
    cleaned_data_geocoded <- cleaned_data %>%
      rename(address = `Address/ Domicilio`) %>%  # rename for compatibility
      geocode(address = address, method = 'osm', lat = latitude, long = longitude)
    
    # Prepare final dataset format to match app expectations
    final_data <- cleaned_data_geocoded %>%
      rename(
        Asset_Name = Agency,
        Asset_Category = Category,
        Latitude = latitude,
        Longitude = longitude
      ) 
    
    # Make sure we have the expected columns for display
    if(!"Contact person" %in% colnames(final_data)) {
      final_data$`Contact person` <- NA_character_
    }
    
    if(!"Phone" %in% colnames(final_data)) {
      final_data$Phone <- NA_character_
    }
    
    if(!"Email" %in% colnames(final_data)) {
      final_data$Email <- NA_character_
    }
    
    if(!"Website" %in% colnames(final_data)) {
      final_data$Website <- NA_character_
    }
    
    if(!"Location Tag" %in% colnames(final_data)) {
      final_data$`Location Tag` <- NA_character_
    }
    
    # Create a combined contact info field
    final_data <- final_data %>%
      mutate(Contact_Information = paste0(
        ifelse(!is.na(`Contact person`) & `Contact person` != "", paste0(`Contact person`, "<br>"), ""),
        ifelse(!is.na(Phone) & Phone != "", paste0("Phone: ", Phone, "<br>"), ""),
        ifelse(!is.na(Email) & Email != "", paste0("Email: ", Email, "<br>"), "")
      ))
    
    # FIXED: Use the function to assign colors consistently
    final_data$color <- sapply(final_data$Asset_Category, get_category_color)
    
    return(final_data)
  }, error = function(e) {
    message("Error processing data: ", e$message)
    
    # Add a more detailed error message for troubleshooting
    message("Google Sheets error details: ")
    print(e)
    
    # Try an alternative approach - using a publicly shared CSV if available
    message("Attempting to load from a backup CSV file if available...")
    tryCatch({
      # If you've exported the sheet to a CSV and stored it locally, try to load it
      if(file.exists("monterey_assets.csv")) {
        backup_data <- read_csv("monterey_assets.csv")
        # FIXED: Use the function to assign colors consistently
        backup_data$color <- sapply(backup_data$Asset_Category, get_category_color)
        message("Successfully loaded backup CSV file.")
        return(backup_data)
      }
    }, error = function(e2) {
      message("Backup CSV also failed to load: ", e2$message)
    })
    
    # If all else fails, return sample data
    message("Using sample data as fallback.")
    sample_data <- data.frame(
      Asset_Name = c("Example Agency 1", "Example Agency 2", "Example Agency 3"),
      Asset_Category = c("Housing", "Education (Adult/Children)", "Health - Mental Health"),
      address = c("123 Main St, Monterey, CA", "456 Oak Ave, Salinas, CA", "789 Pine Blvd, Carmel, CA"),
      Latitude = c(36.6002, 36.6777, 36.5552),
      Longitude = c(-121.8947, -121.6555, -121.9233),
      `Contact person` = c("John Doe", "Jane Smith", "Sam Johnson"),
      Email = c("info@example1.org", "info@example2.org", "info@example3.org"),
      Phone = c("555-1234", "555-5678", "555-9012"),
      Website = c("example1.org", "example2.org", "example3.org"),
      `Location Tag` = c("Monterey", "Salinas", "Carmel"),
      Contact_Information = c("John Doe<br>Phone: 555-1234<br>Email: info@example1.org", 
                              "Jane Smith<br>Phone: 555-5678<br>Email: info@example2.org", 
                              "Sam Johnson<br>Phone: 555-9012<br>Email: info@example3.org"),
      stringsAsFactors = FALSE
    )
    
    # FIXED: Use the function to assign colors consistently
    sample_data$color <- sapply(sample_data$Asset_Category, get_category_color)
    
    return(sample_data)
  })
}

# Function to calculate distance between points in miles
calc_distance <- function(lat1, lon1, lat2, lon2) {
  if(any(is.na(c(lat1, lon1, lat2, lon2)))) {
    return(NA)
  }
  distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine) / 1609.34 # Convert meters to miles
}

# Function to determine appropriate zoom level based on distance filter
get_zoom_level <- function(distance) {
  if(distance <= 1) return(15)
  if(distance <= 2) return(14)
  if(distance <= 5) return(13)
  return(11) # For "All assets" option
}

# IMPROVED: Geocode address function with better address handling
geocode_address <- function(address) {
  if (is.null(address) || address == "") {
    return(NULL)
  }
  
  # Clean the address - remove extra spaces, standardize formatting
  address <- trimws(gsub("\\s+", " ", address))
  
  # Extract potential zip code for better geocoding
  zip_code <- str_extract(address, "\\d{5}")
  
  # Add California context if not already present
  has_ca_context <- grepl("CA|California|Monterey County|Salinas|Monterey|Carmel", address, ignore.case = TRUE)
  
  # Create address variations to try, from most specific to least specific
  address_attempts <- c()
  
  # If address already has CA context, try it first
  if (has_ca_context) {
    address_attempts <- c(address_attempts, address)
  }
  
  # Try with Monterey County context
  if (!grepl("Monterey County", address, ignore.case = TRUE)) {
    address_attempts <- c(address_attempts, paste(address, "Monterey County, CA"))
  }
  
  # Try with specific cities
  if (!grepl("Salinas", address, ignore.case = TRUE)) {
    address_attempts <- c(address_attempts, paste(address, "Salinas, CA"))
  }
  
  if (!grepl("Monterey", address, ignore.case = TRUE)) {
    address_attempts <- c(address_attempts, paste(address, "Monterey, CA"))
  }
  
  if (!grepl("Carmel", address, ignore.case = TRUE)) {
    address_attempts <- c(address_attempts, paste(address, "Carmel, CA"))
  }
  
  # If we have a zip code, try with just that
  if (!is.na(zip_code)) {
    address_attempts <- c(address_attempts, paste(address, zip_code))
  }
  
  # Add the original address as a last resort if not already included
  if (!address %in% address_attempts) {
    address_attempts <- c(address_attempts, address)
  }
  
  # Try to geocode each address version
  for (addr in unique(address_attempts)) {
    tryCatch({
      # Print message for debugging
      message(paste("Trying to geocode:", addr))
      
      # Try OSM geocoding first
      result <- geocode(addr, method = 'osm')
      
      if (nrow(result) > 0 && !is.na(result$lat) && !is.na(result$long)) {
        message("Found coordinates with OSM!")
        return(list(lat = result$lat, lng = result$long))
      }
      
      # Try with Census if OSM fails (US addresses only)
      result <- geocode(addr, method = 'census')
      
      if (nrow(result) > 0 && !is.na(result$lat) && !is.na(result$long)) {
        message("Found coordinates with Census!")
        return(list(lat = result$lat, lng = result$long))
      }
      
      # Try with Nominatim as a last resort
      result <- geocode(addr, method = 'nominatim')
      
      if (nrow(result) > 0 && !is.na(result$lat) && !is.na(result$long)) {
        message("Found coordinates with Nominatim!")
        return(list(lat = result$lat, lng = result$long))
      }
      
    }, error = function(e) {
      message("Geocoding error with address attempt: ", addr, " - ", e$message)
    })
  }
  
  # If we reach here, all attempts failed
  message("All geocoding attempts failed")
  return(NULL)
}

# Add a startup message for debugging the data load
message("Starting Monterey County Asset Map app...")
message("Attempting to load asset data...")

# Create a more robust data loading mechanism
data_loaded <- FALSE
Monterey_County_Assets <- NULL

# First try: Process from Google Sheets
tryCatch({
  message("Loading data from Google Sheets...")
  Monterey_County_Assets <- process_asset_data()
  if(!is.null(Monterey_County_Assets) && nrow(Monterey_County_Assets) > 0) {
    message("Successfully loaded ", nrow(Monterey_County_Assets), " assets from Google Sheets.")
    data_loaded <- TRUE
  }
}, error = function(e) {
  message("Error loading from Google Sheets: ", e$message)
})

# Second try: Load from local CSV if Google Sheets failed
if(!data_loaded) {
  tryCatch({
    message("Attempting to load from local CSV backup...")
    if(file.exists("monterey_assets.csv")) {
      Monterey_County_Assets <- read_csv("monterey_assets.csv")
      # FIXED: Use the function to assign colors consistently
      Monterey_County_Assets$color <- sapply(Monterey_County_Assets$Asset_Category, get_category_color)
      message("Successfully loaded ", nrow(Monterey_County_Assets), " assets from local CSV.")
      data_loaded <- TRUE
    } else {
      message("No local CSV backup found.")
    }
  }, error = function(e) {
    message("Error loading from local CSV: ", e$message)
  })
}

# Final fallback: Use sample data
if(!data_loaded || is.null(Monterey_County_Assets)) {
  message("Using sample data as fallback.")
  Monterey_County_Assets <- data.frame(
    Asset_Name = c("Example Agency 1", "Example Agency 2", "Example Agency 3"),
    Asset_Category = c("Housing", "Education (Adult/Children)", "Health - Mental Health"),
    address = c("123 Main St, Monterey, CA", "456 Oak Ave, Salinas, CA", "789 Pine Blvd, Carmel, CA"),
    Latitude = c(36.6002, 36.6777, 36.5552),
    Longitude = c(-121.8947, -121.6555, -121.9233),
    `Contact person` = c("John Doe", "Jane Smith", "Sam Johnson"),
    Email = c("info@example1.org", "info@example2.org", "info@example3.org"),
    Phone = c("555-1234", "555-5678", "555-9012"),
    Website = c("example1.org", "example2.org", "example3.org"),
    `Location Tag` = c("Monterey", "Salinas", "Carmel"),
    Contact_Information = c("John Doe<br>Phone: 555-1234<br>Email: info@example1.org", 
                            "Jane Smith<br>Phone: 555-5678<br>Email: info@example2.org", 
                            "Sam Johnson<br>Phone: 555-9012<br>Email: info@example3.org"),
    stringsAsFactors = FALSE
  )
  
  # FIXED: Use the function to assign colors consistently
  Monterey_County_Assets$color <- sapply(Monterey_County_Assets$Asset_Category, get_category_color)
}

# Define UI
ui <- page_sidebar(
  title = "Monterey County Asset Map",
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  
  sidebar = sidebar(
    width = 350,
    
    # Logo and title
    div(
      style = "text-align: center; margin-bottom: 15px;",
      img(src = "https://www.co.monterey.ca.us/Home/ShowPublishedImage/2263/637165318035470000", 
          height = "80px"),
      h3("Community Resources")
    ),
    
    # Category filter
    pickerInput(
      inputId = "category_filter",
      label = "Filter by Category",
      choices = sort(unique(Monterey_County_Assets$Asset_Category)),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, 
                     `selected-text-format` = "count > 3",
                     `live-search` = TRUE)
    ),
    
    # IMPROVED: Search by address with better UI
    div(
      style = "margin-top: 15px;",
      textInput("search_address", "Search by Address", 
                placeholder = "123 Main St, Salinas, CA"),
      helpText("For best results, include city and state"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("search_btn", "Search", class = "btn-primary", 
                     style = "flex: 1; margin-right: 5px;"),
        actionButton("current_location_btn", "Use Current Location", 
                     class = "btn-info", style = "flex: 1;")
      )
    ),
    
    # Distance filter dropdown (shown after address search)
    conditionalPanel(
      condition = "input.search_btn > 0 || input.current_location_btn > 0",
      div(
        style = "margin-top: 15px;",
        selectInput("distance_filter", "Show Resources Within:",
                    choices = c("1 mile" = 1, 
                                "2 miles" = 2, 
                                "5 miles" = 5, 
                                "10 miles" = 10,
                                "All resources" = 999))
      )
    ),
    
    # Reset button
    div(
      style = "margin-top: 15px;",
      actionButton("reset_btn", "Reset Map", class = "btn-secondary btn-block")
    ),
    
    # Legend
    div(
      style = "margin-top: 25px;",
      h4("Category Legend"),
      uiOutput("dynamic_legend")
    )
  ),
  
  # Main panel with map and results
  mainPanel(
    fluidRow(
      column(12, 
             div(
               style = "height: 600px;",
               leafletOutput("map", height = "100%")
             )
      )
    ),
    
    # Results panel
    div(
      style = "margin-top: 20px;",
      h4("Results"),
      div(
        style = "max-height: 400px; overflow-y: auto;",
        tableOutput("results_table")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    search_coords = NULL,
    filtered_data = Monterey_County_Assets,
    selected_marker = NULL,
    current_distance = 999 # Default to "All assets"
  )
  
  # Initialize filtered data based on all filters
  observe({
    # Start with all data
    filtered <- Monterey_County_Assets
    
    # Apply category filter if selected
    if (!is.null(input$category_filter) && length(input$category_filter) > 0) {
      filtered <- filtered %>%
        filter(Asset_Category %in% input$category_filter)
    }
    
    # Apply distance filter if search coordinates exist
    if (!is.null(rv$search_coords) && !is.null(input$distance_filter)) {
      # Convert to numeric to make sure
      distance_value <- as.numeric(input$distance_filter)
      rv$current_distance <- distance_value
      
      # Only apply distance filter if not "All assets" (999)
      if (distance_value < 999) {
        filtered <- filtered %>%
          rowwise() %>%
          mutate(distance = calc_distance(
            Latitude, Longitude, 
            rv$search_coords$lat, rv$search_coords$lng
          )) %>%
          ungroup() %>%
          filter(distance <= distance_value) %>%
          arrange(distance)
      } else if (!is.null(rv$search_coords)) {
        # For "All assets", still calculate distance for sorting
        filtered <- filtered %>%
          rowwise() %>%
          mutate(distance = calc_distance(
            Latitude, Longitude, 
            rv$search_coords$lat, rv$search_coords$lng
          )) %>%
          ungroup() %>%
          arrange(distance)
      }
    }
    
    # FIXED: Ensure color column is maintained after filtering
    if (nrow(filtered) > 0 && !"color" %in% colnames(filtered)) {
      filtered$color <- sapply(filtered$Asset_Category, get_category_color)
    }
    
    rv$filtered_data <- filtered
  })
  
  # IMPROVED: Geocode address on search button click with better error handling
  observeEvent(input$search_btn, {
    req(input$search_address)
    
    # Show processing message
    showNotification("Searching for address...", type = "message", id = "search_notif")
    
    # Trim the address input to remove trailing/leading whitespace
    clean_address <- trimws(input$search_address)
    
    # Geocode the address with improved function
    coords <- geocode_address(clean_address)
    
    if (!is.null(coords)) {
      # Update search coordinates
      rv$search_coords <- coords
      
      # Set zoom level based on current distance filter
      zoom_level <- get_zoom_level(as.numeric(input$distance_filter))
      
      # Update map
      leafletProxy("map") %>%
        setView(lng = coords$lng, lat = coords$lat, zoom = zoom_level)
      
      # Show the distance filter
      shinyjs::show("distance_filter")
      
      # Remove the notification
      removeNotification("search_notif")
      showNotification(paste("Address found! Showing results within", 
                             input$distance_filter, "miles."), 
                       type = "message", duration = 5)
    } else {
      removeNotification("search_notif")
      showNotification(paste("Unable to find the address:", clean_address, 
                             ". Please try adding city/state or use a different address."), 
                       type = "error", duration = 7)
    }
  })
  
  # NEW: Handle current location button
  observeEvent(input$current_location_btn, {
    # Show processing message
    showNotification("Getting your current location...", type = "message", id = "location_notif")
    
    # Add JavaScript to get current location
    session$sendCustomMessage(type = "getLocation", message = list())
  })
  
  # NEW: Handle receiving location from JavaScript
  observeEvent(input$current_location, {
    removeNotification("location_notif")
    
    if (!is.null(input$current_location)) {
      # Update search coordinates
      rv$search_coords <- list(
        lat = input$current_location$lat,
        lng = input$current_location$lng
      )
      
      # Set zoom level based on current distance filter
      zoom_level <- get_zoom_level(as.numeric(input$distance_filter))
      
      # Update map
      leafletProxy("map") %>%
        setView(lng = rv$search_coords$lng, lat = rv$search_coords$lat, zoom = zoom_level)
      
      # Show the distance filter
      shinyjs::show("distance_filter")
      
      showNotification("Using your current location.", type = "message", duration = 3)
    } else {
      showNotification("Could not get your location. Please try again or use an address search.", 
                       type = "error", duration = 5)
    }
  })
  
  # Reset map and filters
  observeEvent(input$reset_btn, {
    rv$search_coords <- NULL
    rv$current_distance <- 999
    updatePickerInput(session, "category_filter", selected = character(0))
    updateTextInput(session, "search_address", value = "")
    updateSelectInput(session, "distance_filter", selected = 999)
    shinyjs::hide("distance_filter")
    
    # Reset map view
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = -121.8947, lat = 36.6002, zoom = 10)  # Center on Monterey
  })
  
  # Update zoom level when distance filter changes
  observeEvent(input$distance_filter, {
    if (!is.null(rv$search_coords)) {
      distance_value <- as.numeric(input$distance_filter)
      zoom_level <- get_zoom_level(distance_value)
      
      leafletProxy("map") %>%
        setView(lng = rv$search_coords$lng, lat = rv$search_coords$lat, zoom = zoom_level)
    }
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light Map", "Street Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -121.8947, lat = 36.6002, zoom = 10) %>% # Center on Monterey
      # Add JavaScript to handle location detection
      htmlwidgets::onRender("
        function(el, x) {
          // Add listener for custom Shiny messages
          Shiny.addCustomMessageHandler('getLocation', function(message) {
            if (navigator.geolocation) {
              navigator.geolocation.getCurrentPosition(
                function(position) {
                  Shiny.setInputValue('current_location', {
                    lat: position.coords.latitude,
                    lng: position.coords.longitude
                  });
                },
                function(error) {
                  console.error('Error getting location:', error);
                  Shiny.setInputValue('current_location', null);
                },
                {
                  enableHighAccuracy: true,
                  timeout: 10000,
                  maximumAge: 0
                }
              );
            } else {
              console.error('Geolocation is not supported by this browser.');
              Shiny.setInputValue('current_location', null);
            }
          });
        }")
  })
  
  # FIXED: Update markers using the color column from data
  observe({
    # Clear existing markers
    leafletProxy("map") %>%
      clearMarkers()
    
    # Add search location marker if it exists
    if (!is.null(rv$search_coords)) {
      leafletProxy("map") %>%
        addMarkers(
          lng = rv$search_coords$lng,
          lat = rv$search_coords$lat,
          popup = "Your search location",
          icon = makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
            shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
            iconWidth = 25, iconHeight = 41,
            iconAnchorX = 12, iconAnchorY = 41,
            shadowWidth = 41, shadowHeight = 41
          )
        )
    }
    
    # Add asset markers
    if (nrow(rv$filtered_data) > 0) {
      for (i in 1:nrow(rv$filtered_data)) {
        asset <- rv$filtered_data[i, ]
        
        # Skip if coordinates are missing
        if (is.na(asset$Latitude) || is.na(asset$Longitude)) next
        
        # Create popup content
        popup_content <- paste0(
          "<strong>", asset$Asset_Name, "</strong><br>",
          "<em>", asset$Asset_Category, "</em><br>",
          ifelse(!is.na(asset$address) && asset$address != "", 
                 paste0("Address: ", asset$address, "<br>"), ""),
          ifelse(!is.na(asset$Contact_Information) && asset$Contact_Information != "", 
                 asset$Contact_Information, ""),
          ifelse(!is.na(asset$Website) && asset$Website != "", 
                 paste0("<a href='http://", asset$Website, "' target='_blank'>Website</a><br>"), ""),
          ifelse(!is.null(asset$distance) && !is.na(asset$distance), 
                 paste0("Distance: ", round(asset$distance, 2), " miles"), "")
        )
        
        leafletProxy("map") %>%
          addCircleMarkers(
            lng = asset$Longitude,
            lat = asset$Latitude,
            popup = popup_content,
            radius = 8,
            fillColor = asset$color,
            color = "white",
            weight = 2,
            opacity = 1,
            fillOpacity = 0.8,
            layerId = paste0("marker_", i)
          )
      }
    }
  })
  
  # Dynamic legend based on filtered categories
  output$dynamic_legend <- renderUI({
    if (nrow(rv$filtered_data) > 0) {
      categories <- unique(rv$filtered_data$Asset_Category)
      legend_items <- lapply(categories, function(cat) {
        div(
          style = "margin-bottom: 5px; display: flex; align-items: center;",
          div(
            style = paste0("width: 15px; height: 15px; border-radius: 50%; background-color: ", 
                           get_category_color(cat), "; margin-right: 8px; border: 2px solid white;")
          ),
          span(cat, style = "font-size: 12px;")
        )
      })
      do.call(tagList, legend_items)
    } else {
      p("No categories to display", style = "font-style: italic; color: #666;")
    }
  })
  
  # Results table
  output$results_table <- renderTable({
    if (nrow(rv$filtered_data) > 0) {
      # Prepare display data
      display_data <- rv$filtered_data %>%
        select(Asset_Name, Asset_Category, address, `Contact person`, Phone, Email, Website)
      
      # Add distance column if search was performed
      if (!is.null(rv$search_coords) && "distance" %in% colnames(rv$filtered_data)) {
        display_data <- rv$filtered_data %>%
          select(Asset_Name, Asset_Category, address, `Contact person`, Phone, Email, Website, distance) %>%
          mutate(distance = ifelse(!is.na(distance), paste(round(distance, 2), "miles"), "")) %>%
          rename(`Distance` = distance)
      }
      
      # Clean up column names for display
      colnames(display_data) <- gsub("_", " ", colnames(display_data))
      colnames(display_data) <- tools::toTitleCase(colnames(display_data))
      
      display_data
    } else {
      data.frame(Message = "No resources found matching your criteria.")
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Handle marker clicks
  observeEvent(input$map_marker_click, {
    rv$selected_marker <- input$map_marker_click$id
  })
}

# Run the application
shinyApp(ui = ui, server = server)