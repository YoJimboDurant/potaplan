
IronMaiden = function(x) {
  
  # check for non bogus maidenhead.
  # EM83aw = OK
  
  stopifnot(stringr::str_detect(x, "^[A-Ra-r]{2}\\d{2}(\\d{2})?([A-Xa-x]{2})?$")) 

  lonfield <- stringr::str_extract(x, "^[A-Ra-r]")
  latfield <- stringr::str_extract(x, "(?<=^.)[A-Ra-r]")
  lonsquare <- stringr::str_extract(x, "(?<=^[A-Ra-r]{2})\\d{1}")
  latsquare <- stringr::str_extract(x, "(?<=^...)\\d{1}")
  lonsubsquare <- stringr::str_extract(x, "(?<=\\d{2})[A-Xa-x]")
  latsubsquare <- stringr::str_extract(x, "(?<=^.....)[A-Xa-x]{1}")
  
  lon = (as.numeric((utf8ToInt(
    toupper(lonfield)
  )) - 65) * 20) - 180 + (as.numeric(lonsquare) * 2)
  lat = (as.numeric(utf8ToInt((
    toupper(latfield)
  )) - 65) * 10) - 90 + as.numeric(latsquare)
  
  if (!is.na(lonsubsquare)) {
    lon <- lon + ((as.numeric(utf8ToInt(
      toupper(lonsubsquare)
    )) - 65)
    * (2 / 24))
  }
  if (!is.na(latsubsquare)) {
    lat <- lat + ((as.numeric(utf8ToInt(
      toupper(latsubsquare)
    )) - 65) * (1 / 24))
  }
  
  return(c(Longitude = lon, Latitude = lat))
}
