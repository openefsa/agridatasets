
# get meteo-france or climbox station ID
#' @export get_station_id

get_station_id <- function(latitude, longitude, data=station_index, network="climbox", n=1) {
  
  results <- station_index[[network]] %>% 
    mutate(distance=distance_haversine(lat, lon, latitude, longitude)) %>%
    arrange(distance) %>%
    slice(1:n) %>%
    select(id_station, lat, lon, distance, commune, type)
  
  return(results)
}

# get soil parameters for sunflo from ESDB
#' @export get_soil_esdb

get_soil_esdb <- function(lat, lon, data=soil_france){
  # get closer grid position
  index <- which.min(sqrt((data$lat-lat)^2 + (data$lon-lon)^2))
  
  # build rsunflo soil parameter list (assume a fixed wilting point)
  data_soil <- data %>% 
    slice(index) %>%
    mutate(
      awc=awc_ptf,
      wilting_point_1=10,
      field_capacity_1=(awc_ptf_t/(density_t*300) + wilting_point_1/100)*100,
      wilting_point_2=10,
      field_capacity_2=(awc_ptf_s/(density_s*(depth-300)) + wilting_point_2/100)*100,
      stone_content=mean(gravel_t, gravel_s)/100
    ) %>%
    select(
      awc,
      root_depth=depth,
      field_capacity_1,
      wilting_point_1,
      field_capacity_2,
      wilting_point_2,
      soil_density_1=density_t,
      soil_density_2=density_s,
      stone_content
    )
  
  return(data_soil)
}
