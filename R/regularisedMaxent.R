#' Tune Maxent Regularisation Multiplier
#'
#' @param data
#' @param kf Integer. Number of cross-validation folds
#'
#' @return
#' @export
#'
#' @examples

regularisedMaxent <- function(data,
                              kf = 4,
                              parallel = TRUE,
                              ncors = 4){

  ncors <- min(ncors,
               detectCores() - 1)

  ms <- c(0.25, 0.5, 1, 2, 3, 4)

  folds <- caret::createFolds(data$value,
                              k = kf)

  presences <- data$value

  covariates <- data[ , names(data) != "value"]

  n <- 0

  AUCs <- c()

  kf <- ifelse(sum(data$value) <= kf,
               sum(data$value),
               kf)

  for(m in ms){

    n <- n + 1

    modAUC <- c()

    if(parallel){

      require(foreach)

      ## Make a parallel computing cluster

      cluster <- snow::makeCluster(ncors,
                                   type = "SOCK")

      doSNOW::registerDoSNOW(cluster)

      modAUC <- foreach::foreach(k = seq_len(kf),
                                 .packages = c('maxnet', 'precrec')) %dopar% {

                                   trainSet <- unlist(folds[-k])

                                   testSet <- unlist(folds[k])

                                   mxnet <- maxnet::maxnet(p = presences[trainSet],
                                                           data = covariates[trainSet, ],
                                                           regmult = m, # regularisation multiplier
                                                           maxnet::maxnet.formula(p = presences[trainSet],
                                                                                  data = covariates[trainSet, ],
                                                                                  classes = "default"))

                                   prediction <- as.vector(predict(mxnet,
                                                                   covariates[testSet, ],
                                                                   type = "cloglog"))

                                   modAUC <- precrec::auc(precrec::evalmod(scores = prediction,
                                                                           labels = presences[testSet]))[1, 4]

                                 }

      snow::stopCluster(cluster)

      foreach::registerDoSEQ()

    } else {

      for(k in seq_len(kf)){

        trainSet <- unlist(folds[-k])

        testSet <- unlist(folds[k])

        mxnet <- maxnet::maxnet(p = presences[trainSet],
                                data = covariates[trainSet, ],
                                regmult = m, # regularisation multiplier
                                maxnet::maxnet.formula(p = presences[trainSet],
                                                       data = covariates[trainSet, ],
                                                       classes = "default"))

        prediction <- as.vector(predict(mxnet,
                                        covariates[testSet, ],
                                        type = "cloglog"))

        modAUC[k] <- precrec::auc(precrec::evalmod(scores = prediction,
                                                   labels = presences[testSet]))[1, 4]

      }
    }

    AUCs[n] <- mean(unlist(modAUC))

  }

  bestModel <- ms[which.max(AUCs)]

  return(bestModel)

}
