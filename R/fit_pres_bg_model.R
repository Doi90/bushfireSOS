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
                              filepath,
                              # features = c("default", "lqp", "lqh", "lq", "l"),
                              parallel = TRUE,
                              ncors = 4){

  # features <- match.arg(features)
  ncors <- min(ncors, parallel::detectCores() - 1)

  df <- spp_data$data

  ## Estimate the tuned regularization parameter

  if(tuneParam){

    k <- ifelse(sum(df$Value) <= k,
                sum(df$Value),
                k)

    val <- which(names(df) == "Value")

    best_params <- regularisedMaxent(data = df[ , c(val, 14:ncol(df))],
                                     kf = k,
                                     filepath = filepath,
                                     parallel = parallel,
                                     ncors = ncors)

  } else {

    best_params <- c("betamultiplier=1", "nothreshold")

  }

  ## Fit MaxEnt model

  presences <- df$Value
  covariates <- df[, 14:ncol(df)]

  maxmod <- dismo::maxent(x = covariates,
                          p = presences,
                          removeDuplicates = FALSE,
                          path = filepath,
                          args = c(best_params, "-J", "-P"))

  return(best_mod)

}

