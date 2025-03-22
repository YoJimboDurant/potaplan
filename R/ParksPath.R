#' Return data.frame of parks from POTA API
#'
#' @param route 3 features read from kml path 
#' @param buffer distance in kilometers
#' 
#' @return A sf data.frame of parks
#' @export
#'
#' @examples
#' 
#' x <- sf::st_read(system.file("extdata", "directions.kml", package = "potaplan"))
#' ParksPath(x)
#' 



ParksPath = function(route, buffer = 5){
  stopifnot(inherits(route, "sf"))
  
  buffer <- route |> sf::st_buffer(dist = buffer * 1E3, max_cells = 10000) 
  
  bbox <- buffer |>
  sf::st_bbox()
  
  parks_sf <- getParks(bbox[2], bbox[1], bbox[4], bbox[3]) |>
    dplyr::rowwise() |>
    dplyr::mutate(longitude = geometry.coordinates[1],
           latitude = geometry.coordinates[2]) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  parks_filtered <- parks_sf[sf::st_within(parks_sf, buffer[1,], sparse = FALSE), ] |>
    dplyr::mutate(distance_km = as.numeric(sf::st_distance(geometry, route[2,])) / 1000) |> 
    dplyr::arrange(distance_km) |>
    dplyr::select(properties.reference, properties.name, distance_km)
  
  return(list(parks_filtered = parks_filtered, buffer = buffer))
}