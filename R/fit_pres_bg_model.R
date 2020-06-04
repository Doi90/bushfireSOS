#' Fit a Presence-Background Species Distribution Model
#'
#' @param spp_data
#' @param tuneParam Logical. Whether to tune the regularisation multiplier
#' @param k Integer. If tuneParam = TRUE, specify the number cross-validation folds
#'
#' @return
#' @export
#'
#' @examples

fit_pres_bg_model <- function(spp_data,
                              tuneParam = TRUE,
                              k = 5,
                              parallel = TRUE,
                              ncors = 4){

  ## Estimate the tuned regularization parameter

  if(tuneParam){

    k <- ifelse(sum(spp_data$value) <= k,
                sum(spp_data$value),
                k)

    val <- which(names(spp_data) == "value")

    bestRegMult <- regularisedMaxent(data = spp_data[ , c(val, 5:ncol(spp_data))],
                                     kf = k,
                                     parallel = parallel,
                                     ncors = ncors)

  } else {

    bestRegMult <- 1

  }

  ## Fit MaxEnt model

  best_mod <- maxnet::maxnet(p = spp_data$value,
                             data = spp_data[ , 5:ncol(spp_data)],
                             regmult = bestRegMult,
                             maxnet::maxnet.formula(p = spp_data$value,
                                                    data = spp_data[ , 5:ncol(spp_data)],
                                                    classes = "default"))

  return(best_mod)

}

