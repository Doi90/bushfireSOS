background_points <- function(){

  ## Generate background points

  background_points <- dismo::randomPoints(env_data[[1]],
                                           10000)

  background_points$value <- 0

  colnames(background_points) <- c("lon",
                                   "lat",
                                   "value")

  spp_data <- rbind(spp_data,
                    cbind(background_points,
                          unique(spp_data$species)))

  ## Extract raster data at point locations

  env_values <- raster::extract(env_data,
                                spp_data[ , c("lon", "lat")])

  spp_data <- rbind(spp_data,
                    env_values)




}
