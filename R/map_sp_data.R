#' Map species data
#'
#' @param spp_data species data
#' @param crs coordinate system
#' @param zcol column name to show in colour; set NULL if not needed.
#' @param only_presences Show only presence points
#'
#' @return
#' @export
#'
#' @examples
map_sp_data <- function(spp_data, crs = 3577, zcol = "Value", only_presences = FALSE){
  dt <- spp_data$data
  if(only_presences)
    dt <- dplyr::filter(dt, Value == 1)
  occurrence <- sf::st_as_sf(dt, coords = c("Longitude", "Latitude"), crs = crs)
  mapview::mapview(occurrence, zcol = zcol)
}
