#' Map species data
#'
#' @param spp_data species data
#'
#' @return
#' @export
#'
#' @examples
map_sp_data <- function(spp_data, crs = 3577){
  occurrence <- sf::st_as_sf(spp_data$data, coords = c("Longitude", "Latitude"), crs = crs)
  mapview::mapview(occurrence)
}
