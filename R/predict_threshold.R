#' Threshold prediction
#'
#' @param pred_ras
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples

predict_threshold <- function(pred_ras,
                              threshold){

 thresh_pred <- raster::reclassify(pred_ras,
                                   rcl = c(-Inf, threshold, 0, threshold, Inf, 1))

 return(thresh_pred)

}

