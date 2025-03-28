#' Return data.frame of activations from POTA API
#'
#' @param park park identifier to be queried
#' @param url is default to (https://api.pota.app/profile)
#' 
#' @return A list of data.frames for activator
#' @export
#'
#' @examples
#' 
#' 
#' park <- "US-2177" 
#' getParkHist(park)

getParkHist <- function( park, url = "https://api.pota.app/park/activations/" ){
  
  
  parkURL <- paste0("https://api.pota.app/park/activations/", park, 
                    "?count=all")
  
  
  response <- httr::GET(parkURL)
  if (httr::status_code(response) == 200) {
    
    json_data <- httr::content(response, "text")
    
    # Convert JSON to a data frame
    park_df <- jsonlite::fromJSON(json_data, flatten = TRUE)
    return(park_df) 
  } else {
    stop(paste("Error code", response))
  }  
  
}

