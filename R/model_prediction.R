#' Model Prediction
#'
#' @param model
#' @param env_data
#' @param mask
#' @param parallel
#' @param ncors
#'
#' @return
#' @export
#'
#' @examples
#'

model_prediction <- function(model,
                             env_data,
                             mask,
                             parallel = TRUE,
                             ncors = 4){

  ncors <- min(ncors,
               parallel::detectCores() - 1)

  ## Perform prediction over entire region

  modtype <- class(model)[1]

  outtype <- ifelse(modtype == "maxnet",
                    "cloglog",
                    "response")

  if(parallel){

    raster::beginCluster(n = ncors,
                         type = "SOCK")

    preds <- raster::clusterR(env_data,
                              raster::predict,
                              args = list(model = model,
                                          type = outtype))

    raster::endCluster()

  } else {

    preds <- raster::predict(env_data,
                             model,
                             type = outtype)

  }

  ## Mask prediction to burnt areas

  # burnt_preds <- raster::crop(preds, mask)

  burnt_preds <- raster::mask(preds, mask)

  return(raster::stack(preds,
                       burnt_preds))

}
