#' Remove points
#'
#' @param spp_data species data
#' @param long longitude vector (x)
#' @param lat latitude vector (y)
#' @param buffer the distance of buffer in metres
#' @param crs coordinate system
#'
#' @return the same species data with removed points
#' @export
#'
#' @examples
remove_points <- function(spp_data, long, lat, buffer = 250, crs = 4326){
  if(crs == 4326){
    if(any(lat > 0)) stop("Check the input latitudes. Australia has no postive latitude.")
  }
  dt <- spp_data$data
  occurrence <- sf::st_as_sf(dt, coords = c("Longitude", "Latitude"), crs = crs)
  # points to remove
  xy <- data.frame(x = long, y = lat)
  pt <- sf::st_as_sf(xy, coords = 1:2, crs = crs)
  # create buffer in metres, no matter what crs is
  buff <- raster::buffer(sf::as_Spatial(pt), buffer)
  # select the points within the buffer
  crcs <- sf::st_coordinates(sf::st_intersection(occurrence, st_as_sf(buff)))
  rms <- which(dt$Longitude %in% crcs[, 1] & dt$Latitude %in% crcs[, 2])
  spp_data$data <- spp_data$data[-rms, ]
  return(spp_data)
}
