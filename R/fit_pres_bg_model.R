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

  ncors <- min(ncors,
               parallel::detectCores() - 1)

  df <- spp_data$data

  ## Estimate the tuned regularization parameter

  if(tuneParam){

    k <- ifelse(sum(df$Value) <= k,
                sum(df$Value),
                k)

    val <- which(names(df) == "Value")

    bestRegMult <- regularisedMaxent(data = df[ , c(val, 14:ncol(df))],
                                     kf = k,
                                     parallel = parallel,
                                     ncors = ncors)

  } else {

    bestRegMult <- 1

  }

  ## Fit MaxEnt model

  best_mod <- maxnet::maxnet(p = df$Value,
                             data = df[ , 14:ncol(df)],
                             regmult = bestRegMult,
                             maxnet::maxnet.formula(p = df$Value,
                                                    data = df[ , 14:ncol(df)],
                                                    classes = "default"))

  return(best_mod)

}

