#' Title
#'
#' @param guild
#' @param region
#'
#' @return
#' @export
#'
#' @examples
#'

load_env_data <- function(stack_file,
                          region){

  ## Load file

  stack <- raster::stack(file)

  ## Mask to region

  stack <- mask_data(env_data = stack,
                     region = region)

  return(stack)

}
