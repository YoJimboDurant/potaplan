#' Return list of data.frames of activator activities from POTA API
#'
#' @param call ham radio call to be queried
#' @param url is default to (https://api.pota.app/profile)
#' 
#' @return A list of data.frames for activator
#' @export
#'
#' @examples
#' call <- "ke4mkg" ;
#' activatorGet(call)



activatorGet <- function(call, url = "https://api.pota.app/profile/"){
  
  stationURL <- paste0(url,  call)
  
  response <- httr::GET(stationURL)
  if (httr::status_code(response) == 200) {
    
    json_data <- httr::content(response, "text")
    
    # Convert JSON to a data frame
    activator_df <- jsonlite::fromJSON(json_data, flatten = TRUE)
   return(activator_df) 
  } else {
    stop(paste("Error code", response))
  }

}

  