#' Fit a Presence-Absence Species Distribution Model
#'
#' @param spp_data
#' @param env_data
#'
#' @return
#' @export
#'
#' @examples

fit_pres_abs_model <- function(spp_data,
                               env_data){

  ## Extract raster data at point locations

  env_values <- raster::extract(env_data,
                                spp_data[ , c("lon", "lat")])

  spp_data <- rbind(spp_data,
                    env_values)

  ## Calculations for tree complexity

  nPres <- sum(spp_data$value,
               na.rm = TRUE)

  ## Fit BRT

  brt <- dismo::gbm.step(data = spp_data,
                         gbm.x = 5:ncol(spp_data), # column indices for covariates
                         gbm.y = "value", # column name for response
                         family = "bernoulli",
                         tree.complexity = ifelse(nPres < 50, 1, 5),
                         learning.rate = 0.001,
                         bag.fraction = 0.75,
                         max.trees = 10000,
                         n.trees = 50,
                         n.folds = 5, # 5-fold cross-validation
                         silent = TRUE) # avoid printing the cv results

  return(brt)
♥
}
