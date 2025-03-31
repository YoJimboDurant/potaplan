
#' get and check park history against call sign and return a data.frame of number of attempts, number of successful activations, and number of qso's.
#'
#' @param park park identifier to be queried
#' @param call is call to check
#' 
#' @return A list of data.frames for activator
#' @export
#'
#' @examples
#' 
#' 
#' park <- "US-2177" 
#' getParkHist(park)


getWorkCount = function(call = "KE4MKG", park = "US-2177") {
  call <- toupper(call)
  dfx <- getParkHist(park)
  dfx1 <- dfx[which(dfx$activeCallsign == call), ]
  attempts = dim(dfx1)[[1]]
  activations = sum(dfx1$totalQSOs > 9)
  cwqso = sum(dfx1$qsosCW)
  dataqso = sum(dfx1$qsosDATA)
  phoneqso = sum(dfx1$qsosPHONE)
  
  data.frame(
    call,
    properties.reference = park,
    attempts,
    activations,
    cw = cwqso,
    data = dataqso,
    phone = phoneqso
  )
}