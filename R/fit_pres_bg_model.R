
#' Fit a Presence-Background Species Distribution Model
#'
#' @param spp_data
#' @param env_data
#'
#' @return
#' @export
#'
#' @examples

fit_pres_bg_model <- function(spp_data,
                              env_data){

  ## Generate background points

  background_points <- dismo::randomPoints(env_data[[1]],
                                           10000)

  background_points$value <- 0

  colnames(background_points) <- c("lon",
                                   "lat",
                                   "value")

  spp_data <- rbind(spp_data,
                    cbind(background_points,
                          unique(spp_data$species)))

  ## Extract raster data at point locations

  env_values <- raster::extract(env_data,
                                spp_data[ , c("lon", "lat")])

  spp_data <- rbind(spp_data,
                    env_values)

  ## Estimate the tuned regularization parameter

  ### Set output filepath

  maxFilePath <- sprintf("outputs/models/maxent/%s",
                         unique(spp_data$species))

  if(!dir.exists(maxFilePath)){
    dir.create(maxFilePath)
  }

  bestRegMult <- regularisedMaxent(data = spp_data,
                                   kf = 3,
                                   filepath = maxFilePath)

  ## Fit MaxEnt model

  best_mod <- dismo::maxent(x = spp_data[ , 5:ncol(spp_data)],
                            p = spp_data$value,
                            removeDuplicates = FALSE,
                            path = maxFilePath, # path to save maxent files
                            args = c(paste0("betamultiplier=", bestRegMult)))


  return(best_mod)

}
