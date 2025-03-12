# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(DT)
library(leaflet)

route <- st_read("./Directions from Lawrenceville, GA to Mansfield.kml")

buffer <- route |> st_buffer(dist = 5E3, max_cells = 10000) 

bbox <- buffer |>s
	st_bbox()

parks_sf <- getParks(bbox[2], bbox[1], bbox[4], bbox[3]) |>
	rowwise() |>
	mutate(longitude = geometry.coordinates[1],
		   latitude = geometry.coordinates[2]) |>
	st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

parks_filtered <- parks_sf[st_within(parks_sf, buffer[1,], sparse = FALSE), ] |>
	mutate(distance_km = as.numeric(st_distance(geometry, route[2,])) / 1000) |> 
	mutate(distance_mi = round(distance_km * 0.621371)) |>
	arrange(distance_mi) |>
	select(properties.reference, properties.name, distance_mi)


leaflet() %>%
	addProviderTiles(providers$OpenStreetMap) %>%
	addPolygons(data = buffer, color = "red", weight = 2, fill = FALSE) %>%
	addCircleMarkers(data = parks_filtered, color = "blue", radius = 5, popup = ~properties.name)

datatable(parks_filtered, options = list(pageLength = -1))
