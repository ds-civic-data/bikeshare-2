library(leaflet)
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(lng=stations$Long, lat=stations$Lat, radius = 2, opacity = 1, color = "orange")
m  # Print the map

