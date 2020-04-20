#' Model Prediction
#'
#' @param model
#' @param env_data
#' @param mask
#'
#' @return
#' @export
#'
#' @examples
#'

model_prediction <- function(model,
                             env_data,
                             mask){

  ## Perform prediction over entire region

  preds <- predict(model,
                   env_data)

  ## Mask prediction to burnt areas

  burnt_preds <- mask(preds,
                      mask)

  return(stack(preds,
               burnt_preds))

}
