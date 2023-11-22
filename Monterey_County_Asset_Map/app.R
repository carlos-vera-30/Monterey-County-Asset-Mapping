#Packages loaded 
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets4)



# Data from Google Drive, API connected 
options(gargle_oauth_email = TRUE)
Monterey_County_Assets <- read_sheet("https://docs.google.com/spreadsheets/d/1uCG9b9D4YtGuN6c_aW9gbM-RkQ9pSjbjipuU306T4xo/edit#gid=0")

# Define the side panel UI and server
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("sci"),
      label = "Community Resource", 
      choices = unique(Monterey_County_Assets$`Asset Name`),
      selected = unique(Monterey_County_Assets$`Asset Name`)[1] 
      
    ),
    actionButton(ns("action"),"Submit")
  )
  
}

sideServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # define a reactive and return it
      react<-eventReactive(input$action,{
        
        omited <-subset(Monterey_County_Assets, Monterey_County_Assets$`Asset Name` %in% isolate(input$sci))
      })
      
      return(react)
      
    })
}
# In this case this server not needed but using uiOuput/renderUI in real case
# sideServer <- function(id) { moduleServer(id,function(input, output, session) { })}

# Define the UI and server functions for the map
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}

mapServer <- function(id, city) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map<-renderLeaflet({
        
        leaflet(Monterey_County_Assets = city()) %>% addTiles() %>%
          addMarkers(~Latitude, ~Longitude, popup = ~as.character(Monterey_County_Assets$`Asset Name`), label = ~as.character(Monterey_County_Assets$`Asset Name`))
      })
    })
}

# Build ui & server and then run
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sideUI("side")),
  dashboardBody(mapUI("mapUK"))
)
server <- function(input, output, session) {
  
  # use the reactive in another module
  city_input <- sideServer("side")
  mapServer("mapUK", city_input)
  
}
shinyApp(ui, server)

