getParks = function(llat, llon, ulat, ulon, url = "https://api.pota.app/park/grids"){
  require(httr)
  require(jsonlite)
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

