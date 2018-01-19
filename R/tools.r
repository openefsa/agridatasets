# Compute distance in km between two points: http://en.wikipedia.org/wiki/Haversine_formula
#' @export distance_haversine

distance_haversine <- function(lat1, lon1, lat2, lon2, r = 6378.137){
  radians <- pi/180
  lat2 <- lat2 * radians
  lat1 <- lat1 * radians
  lon2 <- lon2 * radians
  lon1 <- lon1 * radians
  dLat <- (lat2 - lat1)
  dLon <- (lon2 - lon1)
  a <- (sin(dLat/2)^2) + (cos(lat1) * cos(lat2)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}
