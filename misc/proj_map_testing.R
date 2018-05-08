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
top_stations <-  raw_counts %>%
  filter(start_id == "hub_1600", end_id  != "hub_1600") %>%
  arrange(desc(count)) %>%
  head(5) %>% 
  inner_join(stations, by = c("start_id" = "id")) %>%
  inner_join(stations, by = c("end_id" = "id"), suffix = c("_start", "_end"))
  
end_stations <-  raw_counts %>%
  filter(end_id == "hub_1600", start_id  != "hub_1600") %>%
  arrange(desc(count)) %>%
  head(5) %>% 
  inner_join(stations, by = c("start_id" = "id")) %>%
  inner_join(stations, by = c("end_id" = "id"), suffix = c("_start", "_end"))

shared_stations <- inner_join(end_stations, top_stations, by = c("start_id" = "end_id", "end_id" = "start_id"))

  
names(trip_counts) <- c("id", "total")
stations_counts <- inner_join(stations, trip_counts)


  m <- leaflet() %>%
    
    addCircleMarkers(lng=stations_counts$lon, lat=stations_counts$lat, radius = sqrt(stations_counts$total)/10, 
                     fillOpacity = .6, color = "orange", stroke = F) %>%
    addPolylines(lng = c(rbind(top_stations$lon_start, top_stations$lon_end)), 
                 lat = c(rbind(top_stations$lat_start, top_stations$lat_end)), 
                 weight = 1, color = "#FFFF99", opacity = 1)

    
  m 
  
  
