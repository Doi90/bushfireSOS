#' Ensure range maps are in the correct output format
#'
#' @param file
#' @param region
#'
#' @return
#' @export
#'
#' @examples

range_map <- function(file,
                      region){

  ## Read in existing range map from file

  ras <- raster::raster(file)

  ## Ensure correct projection

  if(raster::crs(ras) != ""){

    ras <- raster::projectRaster(from = ras,
                                 crs = "")
  }

  ## Ensure correct resolution
  ### Wait to see what resolution we use


  ## Add buffer
  ### Skipping this step for the time being


  ## Mask to fire-affected area

  fire_mask <- raster::raster()

  ras <- raster::mask(ras,
                      fire_mask)

  return(ras)

}
