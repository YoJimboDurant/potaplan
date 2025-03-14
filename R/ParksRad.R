#' Return data.frame of parks from POTA API
#'
#' @param x valid 6 digit Maidenhead 
#' @param buffer distance in kilometers
#' 
#' @return A sf data.frame of parks
#' @export
#'
#' @examples
#' ParksRad("EM83aw")

ParksRad = function(x, buffer = 100){
  if (inherits(x, "character")){
    
    stopifnot(stringr::str_detect(x, "^[A-Ra-r]{2}\\d{2}(\\d{2})?([A-Xa-x]{2})?$"))
    
    yx <- maidenhead(x)
    
  }else{
    if (class(x) == numeric && length(x) == 2){
      yx <-x 
    }
  }
    qth <- yx |>
    sf::st_point() |>
    sf::st_sfc(crs = 4326)
  
  circleDist <- qth |> sf::st_buffer(dist = 1E3 * buffer, max_cells = 100000) 
  
  
  bbox <- circleDist |>
    sf::st_bbox()
  
  parks_sf <- getParks(bbox[2], bbox[1], bbox[4], bbox[3]) |>
    dplyr::rowwise() |>
    dplyr::mutate(longitude = geometry.coordinates[1],
           latitude = geometry.coordinates[2]) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  parks_filtered <- parks_sf[sf::st_within(parks_sf, circleDist, sparse = FALSE), ] |>
    dplyr::mutate(distance_km = as.numeric(sf::st_distance(geometry, qth)) / 1000) |> 
    dplyr::arrange(distance_km) |>
    dplyr::select(properties.reference, properties.name, distance_km)
  
  return(parks_filtered)
}