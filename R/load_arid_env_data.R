#' Load Environmental Data
#'
#' @param file Character. File path for raster stack
#'
#' @return
#' @export
#'
#' @examples
#'

load_arid_env_data <- function(file){

  ras <- raster::stack(file)

  return(ras)

}
