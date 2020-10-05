#' Fit a Presence-Absence Species Distribution Model
#'
#' @param spp_data
#'
#' @return
#' @export
#'
#' @examples

fit_pres_abs_model <- function(spp_data){

  ## Calculations for tree complexity

  df <- spp_data$data

  nPres <- sum(df$Value, na.rm = TRUE)

  ## Fit BRT

  brt <- dismo::gbm.step(data = df,
                         gbm.x = 14:ncol(df), # column indices for covariates
                         gbm.y = "Value", # column name for response
                         family = "bernoulli",
                         tree.complexity = ifelse(nPres < 50, 1, 5),
                         learning.rate = 0.001,
                         bag.fraction = 0.75,
                         max.trees = 10000,
                         n.trees = 50,
                         n.folds = 5, # 5-fold cross-validation
                         silent = FALSE) # avoid printing the cv results

  return(brt)

}
