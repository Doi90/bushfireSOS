#' Tune Maxent Regularisation Multiplier
#'
#' @param data
#' @param kf
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples

regularisedMaxent <- function(data,
                              kf = 3,
                              filepath){

  ms <- c(0.25, 0.5, 1, 2, 3, 4)

  folds <- caret::createFolds(data$occ,
                              k = kf)

  n <- 0

  AUCs <- c()

  for(m in ms){

    n <- n + 1

    modAUC <- c()

    for(k in seq_len(kf)){

      trainSet <- unlist(folds[-k])

      testSet <- unlist(folds[k])

      me1 <- dismo::maxent(x = data[trainSet, 2:ncol(data)],
                           p = data$occ[trainSet],
                           path = filepath,
                           args = c(paste0("betamultiplier=", m)))

      modpre <- prediction(predict(me1,
                                   data[testSet, 2:ncol(data)]),
                           data$occ[testSet])

      modperf <- performance(modpre,
                             "auc")

      modAUC[k] <- as.numeric(modperf@y.values)

    }

    AUCs[n] <- mean(modAUC)

  }

  bestModel <- ms[which.max(AUCs)]

  return(bestModel)

}
