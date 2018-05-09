library(leaflet)
library(tidyverse)
stations <- read_csv("~/R-stuff/bikeshare-2/data/stations.csv")
raw_counts <- read_csv("~/R-stuff/bikeshare-2/data/raw_trips.csv")
trip_counts <- raw_counts  %>%
  group_by(start_id) %>%
  summarise(sum(count))


# This file was mostly just testing so I won't comment it. Much of what was successful is also found in
# the shiny.R file
stations <- stations %>%
  select(start_id, lon, lat, StartHub)

names(stations) <- c("id", "lon", "lat", "name")

top_stations <-  raw_counts %>%
  filter(start_id == "hub_1569", end_id  != "hub_1569") %>%
  arrange(desc(count)) %>%
  head(10) %>% 
  inner_join(stations, by = c("start_id" = "id")) %>%
  inner_join(stations, by = c("end_id" = "id"), suffix = c("_start", "_end"))


shared_stations <- inner_join(end_stations, top_stations, by = c("start_id" = "end_id", "end_id" = "start_id"))


names(trip_counts) <- c("id", "total")
stations_counts <- inner_join(stations, trip_counts)


m2 <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(lng=stations_counts$lon, lat=stations_counts$lat, 
                   radius = sqrt(stations_counts$total)/10, 
                   fillOpacity = .6, color = "orange", layerId = stations_counts$num_id, 
                   stroke = F, label = stations_counts$name) 

i <- 1
while (i <= 10) 
{
  
  m2 <- m2 %>%
    addPolylines(lng = c(top_stations$lon_start[i], top_stations$lon_end[i]), 
                 lat = c(top_stations$lat_start[i], top_stations$lat_end[i]),
                 label = paste("Destination: ", top_stations$name_end[i], ", Total Trips: ",as.character(top_stations$count[i])),
                 weight = 20*(top_stations$count[i]/sum(top_stations$count)), 
                 color = "black", opacity = 1)
i = i + 1

}
 

m2



