#' Mask environmental data
#'
#' @param region
#' @param env_data
#'
#' @return
#' @export
#'
#' @examples
#'

mask_data <- function(env_data,
                      region,
                      crop = FALSE){

  ## Don't forget the ACT

  # if("NSW" %in% region){
  #
  #   region <- c(region, "ACT")
  #
  # }
  #
  # ## Convert region character
  #
  # region <- gsub("VIC", "Victoria", region)
  # region <- gsub("NSW", "New South Wales", region)
  # region <- gsub("QLD", "Queensland", region)
  # region <- gsub("SA", "South Australia", region)
  # region <- gsub("WA", "Western Australia", region)
  # region <- gsub("NT", "Northern Territory", region)
  # region <- gsub("TAS", "Tasmania", region)
  # region <- gsub("ACT", "Australian Capital Territory", region)

#
#   ## Get state polygon data and make it sf
#
#   AUS.shapes <- rnaturalearth::ne_states("australia",
#                                          returnclass = "sp")
#
#   AUS.shapes <- sp::spTransform(AUS.shapes,
#                                 CRSobj = raster::crs(env_data))
#
#   AUS.shapes <- AUS.shapes[AUS.shapes$name %in% region, ]

  ## Add ACT to NSW
  if("NSW" %in% region){
    if(!"ACT" %in% region)
      region <- c(region, "ACT")
  }

  ## read the borders and filter them
  aus <- rnaturalearth::ne_states("australia",
                                  returnclass = "sf")
  aus <- aus[-c(6,9,10), ]
  aus$states <- substr(aus$iso_3166_2, 4, nchar(aus$iso_3166_2))
  aus <- sf::st_transform(aus, crs = raster::crs(env_data))
  AUS.shapes <- aus[aus$states %in% region, ]

  if(crop){
    env_data <- raster::crop(env_data, AUS.shapes)
  }

  env_data <- raster::mask(env_data,
                             mask = AUS.shapes)

  return(env_data)

}
