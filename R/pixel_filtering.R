#' Filter records by pixel
#'
#' @param data
#' @param raster
#'
#' @return
#' @export
#'
#' @examples

pixel_filtering <- function(data,
                            raster){

  samplecellID <- raster::cellFromXY(raster,
                                     data[ , c("Longitude", "Latitude")])

  dup <- duplicated(samplecellID)

  data <- data[!dup, ]

  return(data)

}
