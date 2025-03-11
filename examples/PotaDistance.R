
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(DT)
library(leaflet)


# get distance of 100 km from home QTH 
    
qth <- IronMaiden("EM83aw") |>
  st_point() |>
  st_sfc(crs = 4326)

circleDist <- qth |> st_buffer(dist = 1E5, max_cells = 10000) 


bbox <- circleDist |>
  st_bbox()

parks_sf <- getParks(bbox[2], bbox[1], bbox[4], bbox[3]) |>
  rowwise() |>
  mutate(longitude = geometry.coordinates[1],
         latitude = geometry.coordinates[2]) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

parks_filtered <- parks_sf[st_within(parks_sf, circleDist, sparse = FALSE), ] |>
  mutate(distance_km = as.numeric(st_distance(geometry, qth)) / 1000) |> 
  mutate(distance_mi = round(distance_km * 0.621371)) |>
  arrange(distance_mi) |>
  select(properties.reference, properties.name, distance_mi)



leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = circleDist, color = "red", weight = 2, fill = FALSE) %>%
  addCircleMarkers(data = parks_filtered, color = "blue", radius = 5, popup = ~properties.name)


datatable(parks_filtered, options = list(pageLength = -1))
