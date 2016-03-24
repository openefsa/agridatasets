# Compute distance between two points: http://en.wikipedia.org/wiki/Haversine_formula
#' @export distance_haversine

distance_haversine <- function(lat1, lon1, lat2, lon2) {
  
  # Earth radius in km
  R <- 6371
  delta_lon <- (lon2*pi/180 - lon1*pi/180) # decimal degrees to radians
  delta_lat <- (lat2*pi/180 - lat1*pi/180) # decimal degrees to radians
  a <- sin(delta_lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon/2)^2
  c <- 2 * asin(pmin(1,sqrt(a)))
  d = R * c # distance in km
  return(d)
}
