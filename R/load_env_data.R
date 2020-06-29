#' Load Environmental Data
#'
#' @param stack_file Character. The directory to the raster_tile folder
#' @param region
#'
#' @return
#' @export
#'
#' @examples
#'

load_env_data <- function(stack_file,
                          region){

  ## make a temp file for the vrt
  tmp <- tempdir(check = FALSE)
  outfile <- file.path(tmp, "bushfire_raster.vrt")

  ## select the regions
  infile <- list.files(stack_file, pattern = ".tif$")
  inname <- substr(infile, 1, nchar(infile) - 4)
  infile <- infile[which(inname %in% region)]

  ## make the virtual tiles
  gdalUtils::gdalbuildvrt(gdalfile = file.path(stack_file, infile),
                          output.vrt = outfile,
                          vrtnodata = -9999,
                          overwrite = TRUE)

  ## read the tiles
  tempraster <- raster::brick(outfile)

  return(tempraster)

}
