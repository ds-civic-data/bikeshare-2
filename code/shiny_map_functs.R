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