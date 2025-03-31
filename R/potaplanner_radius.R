#' Creates a leaflet map showing if parks in radius of a Maidenhead have been worked by a given hamcall.
#' 
#' @param hamcall is hamcall to check 
#' @param grid is maidenhead grid (6 digits)
#' @param distance is distance in kilometers to check parks
#' 
#' @return A leaflet map
#' @export
#'
#' @examples
#' 
#' 
#' hamcall <- "KE4MKG" 
#' maidenhead <- "EM83aw"
#' radius <- 100
#' 
#' potaplanner_radius(hamcall, maidenhead, radius)

potaplanner_radius = function(
    hamcall = "KE4MKG",
    grid = "EM83aw",
    distance = 100){
  

dfx = ParksRad(grid, distance)
#library(leaflet)

hamcall <- toupper(hamcall)

parksRefs <- dfx$parks_filtered$properties.reference

activity_dfx <- 
  lapply(parksRefs, function(x) getWorkCount(hamcall, x)) |>
  dplyr::bind_rows()

activity_dfx <- activity_dfx |> dplyr::rowwise() |>
  dplyr::mutate(total = cw + data + phone,
                activated = activations > 0)

dfx$parks_filtered <- merge(dfx$parks_filtered, activity_dfx)

leaflet::leaflet(width = 700) |>
  leaflet::addProviderTiles("OpenStreetMap.Mapnik") |>
  leaflet::addCircleMarkers(
    data = dfx$parks_filtered[dfx$parks_filtered$activated,],
    color = "#377eb8",
    fillOpacity = 0.6,
    radius = ~sqrt(activations) + 3,
    popup = ~ paste0(properties.name, " activations = ", activations)
  ) |>
  leaflet::addCircleMarkers(
    data = dfx$parks_filtered[!dfx$parks_filtered$activated,],
    color = "#e41a1c",
    radius = 4,
    fillOpacity = 0.6,
    popup = ~ properties.name
  ) |>
  leaflet::addPolygons(
    data = dfx$circleDist,
    color = "orange",
    weight = 2,
    fill = FALSE
  )}