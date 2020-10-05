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

  ncors <- min(ncors, parallel::detectCores() - 1)

  # ## Mask
  #
  # mask <- sf::st_as_sf(rgdal::readOGR(mask))
  #
  # mask <- sf::st_transform(mask,
  #                          crs = 3577)

  ## Perform prediction over entire region

  modtype <- class(model)[1]

  # outtype <- ifelse(modtype == "maxnet",
  #                   "cloglog",
  #                   "response")

  if(parallel){

    raster::beginCluster(n = ncors,
                         type = "SOCK")

    if(modtype == "MaxEnt"){
      preds <- raster::clusterR(env_data,
                                raster::predict,
                                args = list(model = model,
                                            args = "outputformat=cloglog"))
    } else{
      preds <- raster::clusterR(env_data,
                                raster::predict,
                                args = list(model = model,
                                            n.trees = model$gbm.call$best.trees,
                                            type = "response"))
    }

    raster::endCluster()

  } else {

    if(modtype == "MaxEnt"){
      preds <- raster::predict(env_data,
                               model,
                               args = "outputformat=cloglog")
    } else{
      preds <- raster::predict(env_data,
                               model,
                               n.trees = model$gbm.call$best.trees,
                               type = "response")
    }

  }

  ## Mask prediction to burnt areas

  # burnt_preds <- raster::crop(preds, mask)

  # burnt_preds <- raster::mask(preds, mask)

  # return(raster::stack(preds,
  #                      burnt_preds))

  return(preds)

}
