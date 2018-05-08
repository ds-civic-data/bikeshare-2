library(shiny)
library(leaflet)
library(tidyverse)
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

make_top_stations <- function(hub) {
  top_stations <-  raw_counts %>%
    filter(start_id == hub, end_id  != hub) %>%
    arrange(desc(count)) %>%
    head(10) %>% 
    inner_join(stations, 
               by = c("start_id" = "id")) %>%
    inner_join(stations, 
               by = c("end_id" = "id"), 
               suffix = c("_start", "_end"))
  return(top_stations)
}

make_in_top_stations <- function(hub) {
  top_stations <-  raw_counts %>%
    filter(end_id == hub, start_id  != hub) %>%
    arrange(desc(count)) %>%
    head(10) %>% 
    inner_join(stations, 
               by = c("start_id" = "id")) %>%
    inner_join(stations, 
               by = c("end_id" = "id"), 
               suffix = c("_start", "_end"))
  return(top_stations)
}

  
make_line_map <- function(map, dta) {
  i <- 1
  while (i <= 9) {
    map <- map %>%
      addPolylines(lng = c(dta$lon_start[i], 
                           dta$lon_end[i]), 
                   lat = c(dta$lat_start[i], 
                           dta$lat_end[i]),
                   label = paste("Destination: ", 
                                 dta$name_end[i], ", Total Trips: ", as.character(dta$count[i])),
                   weight = 10*(dta$count[i]/sum(dta$count)), 
                   color = "purple", 
                   opacity = 1,
                   group = "line")
    i = i + 1
  }
  return(map)
}



ui <- fluidPage(
  leafletOutput("mymap", width = "500px", height = "500px"),
  p(),
  actionButton("reset", "Reset"),
  checkboxInput("out", "Top Outgoing Stations", value = TRUE),
  checkboxInput("incoming", "Top Incoming Stations", value = FALSE)
  
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({map_basic})
  
  observeEvent(input$reset, {output$mymap <- renderLeaflet({map_basic})}) 
  
  
  
  observeEvent(input$mymap_marker_click, {
    proxy <- leafletProxy("mymap") %>%
      clearGroup(group = "line")
    click <- input$mymap_marker_click
    if (input$out == TRUE) {
      top <- make_top_stations(click$id)
      
      make_line_map(proxy, top)
    }
   
    if (input$incoming == TRUE) {
      top <- make_in_top_stations(click$id)
      make_line_map(proxy, top)
    }
    
    
})
  
  

}

shinyApp(ui, server)

















