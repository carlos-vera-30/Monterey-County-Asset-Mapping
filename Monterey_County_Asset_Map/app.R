# Packages loaded
library(tidyverse)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets4)
library(leaflet.providers)

# Data from Google Drive, API connected
options(gargle_oauth_email = TRUE)
Monterey_County_Assets <- read_sheet("https://docs.google.com/spreadsheets/d/1uCG9b9D4YtGuN6c_aW9gbM-RkQ9pSjbjipuU306T4xo/edit#gid=0")

# Define the side panel UI and server
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("sci"),
      label = "Community Asset", 
      choices = unique(Monterey_County_Assets$Asset_Name),
      selected = unique(Monterey_County_Assets$Asset_Name)  # Select all assets initially
    ),
    actionButton(ns("action"), "Submit")
  )
}

sideServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Define a reactiveVal to store filtered data
      filtered_data <- reactiveVal(Monterey_County_Assets)  # Initially, set to all assets
      
      observe({
        # Update filtered_data whenever action button is pressed or asset selection changes
        req(input$action)
        filtered_data(subset(Monterey_County_Assets, Asset_Name %in% input$sci))
      })
      
      # Return the reactiveVal
      return(filtered_data)
    }
  )
}

# Define the UI and server functions for the map
mapUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"))
}

mapServer <- function(id, Asset_Location) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet({
        data <- Asset_Location()
        leaflet(data = data) %>% 
          addProviderTiles("OpenStreetMap") %>% 
          addMarkers(
            lat = ~Latitude,
            lng = ~Longitude,
            popup = ~as.character(Asset_Location()$Asset_Name), 
            label = ~as.character(Asset_Location()$Asset_Name)
          )
      })
    }
  )
}

# Build ui & server and then run
ui <- dashboardPage(
  dashboardHeader(title = "Monterey County Asset Map", titleWidth = 350),
  dashboardSidebar(width = 350, sideUI("side")),
  dashboardBody(
    fluidPage(
      mapUI("mapUSA")
    )
  )
)

server <- function(input, output, session) {
  # use the reactive in another module
  Asset_Location_input <- sideServer("side")
  mapServer("mapUSA", Asset_Location_input)  
}

shinyApp(ui, server)