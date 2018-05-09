library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
source("~/R-stuff/bikeshare-2/code/bike_pred.R")
stations <- read_csv("~/R-stuff/bikeshare-2/data/stations.csv")
raw_counts <- read_csv("~/R-stuff/bikeshare-2/data/raw_trips.csv")
trip_counts <- raw_counts  %>%
  group_by(start_id) %>%
  summarise(sum(count))

stations <- stations %>%
  select(start_id, lon, lat, StartHub)

names(stations) <- c("id", "lon", "lat", "name")


names(trip_counts) <- c("id", "total")
stations_counts <- inner_join(stations, trip_counts)

# Creating basic map without lines, just circle markers at each station.
map_basic <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(lng=stations_counts$lon, 
                   lat=stations_counts$lat, 
                   radius = sqrt(stations_counts$total)/10, 
                   fillOpacity = .6, 
                   color = "orange", 
                   layerId = stations_counts$id, 
                   stroke = F, 
                   label = stations_counts$name) 

# Function to construct top outgoing stations for a given hub
make_top_stations <- function(hub, n) {
  top_stations <-  raw_counts %>%
    filter(start_id == hub, end_id  != hub) %>%
    arrange(desc(count)) %>%
    head(n) %>% 
    inner_join(stations, 
               by = c("start_id" = "id")) %>%
    inner_join(stations, 
               by = c("end_id" = "id"), 
               suffix = c("_start", "_end")) %>%
    mutate(color = "maroon")
  return(top_stations)
}
# Function to construct top outgoing stations for a given hub

make_in_top_stations <- function(hub, n) {
  top_stations <-  raw_counts %>%
    filter(end_id == hub, start_id  != hub) %>%
    arrange(desc(count)) %>%
    head(n) %>% 
    inner_join(stations, 
               by = c("start_id" = "id")) %>%
    inner_join(stations, 
               by = c("end_id" = "id"), 
               suffix = c("_start", "_end")) %>%
    mutate(color = "navy")
  return(top_stations)
}

# Function that adds lines between stations. Represents relative frequency with line weigth  
make_line_map <- function(map, dta, n) {
  i <- 0
  while (i <= n) {
    map <- map %>%
      addPolylines(lng = c(dta$lon_start[i], 
                           dta$lon_end[i]), 
                   lat = c(dta$lat_start[i], 
                           dta$lat_end[i]),
                   label = paste("Destination: ", 
                                 dta$name_end[i], ", Total Trips: ", as.character(dta$count[i])),
                   weight = n*(dta$count[i]/sum(dta$count)), 
                   color = dta$color, 
                   opacity = 1,
                   group = "line")
    i = i + 1
  }
  return(map)
}

# Shiny app starts here

ui <- dashboardPage(
  dashboardHeader(title = "Biketown"),
  dashboardSidebar(sidebarMenu(
    menuItem("Explore", tabName = "explore"),
    menuItem("Predict", tabName = "predict"))),
  dashboardBody(tabItems(tabItem(tabName = "explore", 
                                 fluidRow(column(width = 9,
                                                 leafletOutput("mymap", height = "700px")),
                                          column(width = 3, box(width = "100px", actionButton("reset", "Reset"),
                                                                checkboxInput("out", "Top Outgoing Stations", value = TRUE),
                                                                checkboxInput("incoming", "Top Incoming Stations", value = FALSE),
                                                                sliderInput("slide", "Number of top stations to display", 1, 40, 10))))),
                         tabItem(tabName = "predict",
                                 fluidRow(column(width = 9, leafletOutput("predmap", height = "700px")),
                                          column(width = 3, box(width = "100px", 
                                                                sliderInput("predtime", "Hours ahead to predict", 1, 16, 1),
                                                                sliderInput("mintemp", "Minimum Temperature (F)", 0, 100, 50),
                                                                sliderInput("maxtemp", "Maximum Temperature (F)", 0, 100, 50),
                                                                sliderInput("rain", "Rainfall (1/100 inches)", 0, 300, 0),
                                                                sliderInput("hour", "Hour of day (24H time)", 1, 23, 12),
                                                                dateInput("date", "Date", format = "yyyy-mm-dd", startview = "month", weekstart = 7,
                                                                          language = "en", width = NULL),
                                                                actionButton("predict1", "Predict")))))))
)




server <- function(input, output, session) {
  output$mymap <- renderLeaflet({map_basic})
  # Removes lines
  observeEvent(input$reset, {output$mymap <- renderLeaflet({map_basic})}) 
  
  
  # Creates lines between station you clicked on
  observeEvent(input$mymap_marker_click, {
    proxy <- leafletProxy("mymap") %>%
      clearGroup(group = "line")
    click <- input$mymap_marker_click
    if (input$out == TRUE) {
      top <- make_top_stations(click$id, input$slide)
      make_line_map(proxy, top, input$slide)
    }
    
    if (input$incoming == TRUE) {
      top <- make_in_top_stations(click$id, input$slide)
      make_line_map(proxy, top, input$slide)
    }
    
    
  })
  output$predmap <- renderLeaflet(leaflet() %>% 
                                    addProviderTiles(providers$Stamen.Terrain) %>%
                                    setView(lng = -122.6765, lat= 45.5231, zoom = 12))
  
  observeEvent(input$predict1, {
    pred_data <- pred_distribution(input$hour, input$date, input$maxtemp, input$mintemp,input$rain, 
                                   input$predtime)
    
    proxy2 <- leafletProxy("predmap") %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      clearMarkers() %>%
      addCircleMarkers(lng=pred_data$lon, 
                       lat=pred_data$lat,
                       radius = sqrt(20*((abs(pred_data$diff) + 1))), 
                       stroke = F,
                       fillOpacity = .6,
                       color = if_else(pred_data$diff > 0, "green", "red"),
                       popup = paste("Predicted Change in Bikes: ", round(pred_data$diff)))
    
    
    
    
    
  }
  
  )
  
  
  
  
}

shinyApp(ui, server)














