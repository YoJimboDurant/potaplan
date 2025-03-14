#' Return data.frame of parks from POTA API
#'
#' @param llat Lower left latitude coordinate (decimal degrees)
#' @param llon Lower left longitude coordinate
#' @param ulat upper right latitude coordinate
#' @param ulon upper right longitude coordinate
#' @param url is default to (https://api.pota.app/park/grids)
#' 
#' @return A data.frame of parks
#' @export
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' 
#' x <- "EM83aw"
#' qth <- maidenhead("EM83aw") |>
#' st_point() |>
#' st_sfc(crs = 4326)
#' 
#' circleDist <- qth |> st_buffer(dist = 1E5, max_cells = 10000) 
#' bbox <- circleDist |>
#' st_bbox()
#' parks_sf <- getParks(bbox[2], bbox[1], bbox[4], bbox[3]) |>
#' rowwise() |>
#' mutate(longitude = geometry.coordinates[1],
#'          latitude = geometry.coordinates[2]) |>
#'            st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



getParks = function(llat, llon, ulat, ulon, url = "https://api.pota.app/park/grids"){
   potaURL <- paste0(
    paste(url,llat,llon,ulat,ulon,0, sep = "/"), ".json"
  )
  
  response <- httr::GET(potaURL)
  if (httr::status_code(response) == 200) {
    
    json_data <- httr::content(response, "text")
    
    # Convert JSON to a data frame
    parks_df <- jsonlite::fromJSON(json_data, flatten = TRUE)
    
    return(parks_df$feature)
  } else {
    stop(paste("API request failed with status code", status_code(response)))
  }
}