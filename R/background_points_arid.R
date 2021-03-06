#' Fast(er) random probability sampling from raster files
#'
#' @param r raster file
#' @param size sample size
#'
#' @return coordinate matrix
#' @export
#'
#' @examples
fastRandomPoints <- function(r, size = 1e4) {
  if(raster::nlayers(r) > 1) r <- r[[1]]
  val <- raster::getValues(r)
  val_notNA <- which(!is.na(val))
  x <- sample(val_notNA, size, prob = val[val_notNA])
  pts <- raster::xyFromCell(r, x)
  return(pts)
}



#' Generate background points
#'
#' @param species Charaacter vector of species name
#' @param spp_data List of species data
#' @param env_data Rasters tack
#' @param guild Character vector
#' @param region Character vector
#' @param background_group Character vector. One of: vertebrates, plants, insects
#'
#' @return
#' @export
#'
#' @examples
background_points_arid <- function(species,
                                   spp_data,
                                   bias_layer,
                                   n_samples,
                                   dismo_sampling = FALSE){

    ## Generate background points

    if(dismo_sampling){
      bg_dismo <- dismo::randomPoints(bias_layer,
                                      n_samples,
                                      prob = TRUE)
    } else{
      bg_dismo <- fastRandomPoints(bias_layer,
                                   size = n_samples)
    }


    bg <- data.frame(ID = NA,
                     Origin = NA,
                     Species = NA,
                     Longitude = bg_dismo[ , 1],
                     Latitude = bg_dismo[ , 2],
                     Date = NA,
                     Basis.of.Record = NA,
                     Locality = NA,
                     Institute = NA,
                     Collection = NA,
                     Coordinate.Uncertainty.in.Metres =NA,
                     Guild = NA,
                     Value = 0,
                     stringsAsFactors = FALSE)

    type <- "random background"

  ## Filter presence points by pixel

  spp_data$data <- pixel_filtering(data = spp_data$data,
                                   raster = raster::raster(bias_layer))

  ## Combine spp and bg dfs

  spp_data$data$Value <- 1

  spp_data$data$Guild <- NA

  spp_data$data <- rbind(spp_data$data,
                         bg)

  message(sprintf("BG type: %s",
                  type))

  return(spp_data)

}

