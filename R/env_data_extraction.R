#' Environmental Data Extraction
#'
#' @param spp_data List of species data
#' @param env_data Stack of rasters
#'
#' @return
#' @export
#'
#' @examples

env_data_extraction <- function(spp_data,
                                env_data){

  env_extract <- raster::extract(env_data,
                                 spp_data$data[,c("Longitude","Latitude")])

  spp_data$data <- cbind(spp_data$data,
                         env_extract)

  spp_data$data <- spp_data$data[complete.cases(spp_data$data[, 14:ncol(spp_data$data)]), ]

  return(spp_data)

}
