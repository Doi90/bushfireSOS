
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
