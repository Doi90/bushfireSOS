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
                              kf = 5,
                              # features = c("default", "lqp", "lqh", "lq", "l"),
                              # parallel = TRUE,
                              # ncors = 4,
                              filepath){
  require(dismo)
  require(caret)
  require(precrec)

  # features <- match.arg(features)
  # ncors <- min(ncors, parallel::detectCores() - 1)


  folds <- caret::createFolds(y = as.factor(data$Value), k = kf)

  presences <- data$Value
  covariates <- data[ , names(data) != "Value"]

  n <- 0
  AUCs <- c()

  kf <- ifelse(sum(data$Value) <= kf,
               sum(data$Value),
               kf)

  # create the paramter grid for tuning
  ms <- c(0.5, 1, 2, 3, 4)
  grid <- expand.grid(regmult = paste0("betamultiplier=", ms),
                      features = list(c("noautofeature", "linear=true", "quadratic=true", "hinge=true", "product=true", "threshold=false"), # LQHP
                                      c("noautofeature", "linear=true", "quadratic=true", "hinge=true", "product=false", "threshold=false"), # LQH
                                      c("noautofeature", "linear=true", "quadratic=true", "hinge=false", "product=true", "threshold=false"), # LQP
                                      c("noautofeature", "linear=true", "quadratic=true", "hinge=false", "product=false", "threshold=false"), # LQ
                                      c("noautofeature", "linear=false", "quadratic=false", "hinge=true", "product=false", "threshold=false"), # H
                                      c("noautofeature", "linear=true", "quadratic=false", "hinge=false", "product=false", "threshold=false")), # L
                      stringsAsFactors = FALSE)

  for(n in seq_along(grid[, 1])){

    n <- n + 1

    modAUC <- c()

    # if(parallel){
    #
    #   require(foreach)
    #
    #   ## Make a parallel computing cluster
    #
    #   cluster <- snow::makeCluster(ncors, type = "SOCK")
    #
    #   doSNOW::registerDoSNOW(cluster)
    #
    #   modAUC <- foreach::foreach(k = seq_len(kf),
    #                              .packages = c('dismo', 'precrec')) %dopar% {
    #
    #                                trainSet <- unlist(folds[-k])
    #
    #                                testSet <- unlist(folds[k])
    #
    #                                # mxnet <- maxnet::maxnet(p = presences[trainSet],
    #                                #                         data = covariates[trainSet, ],
    #                                #                         regmult = m, # regularisation multiplier
    #                                #                         maxnet::maxnet.formula(p = presences[trainSet],
    #                                #                                                data = covariates[trainSet, ],
    #                                #                                                classes = features))
    #
    #                                if(inherits(try(
    #                                  maxmod <- dismo::maxent(x = covariates[trainSet, ],
    #                                                          p = presences[trainSet],
    #                                                          removeDuplicates = FALSE,
    #                                                          path = filepath,
    #                                                          args = as.character(unlist(grid[n, ]))
    #                                  )
    #                                ), "try-error")){
    #                                  next
    #                                }
    #
    #                                prediction <- as.vector(predict(maxmod,
    #                                                                covariates[testSet, ],
    #                                                                args = "outputformat=cloglog"))
    #
    #                                modAUC <- precrec::auc(precrec::evalmod(scores = prediction,
    #                                                                        labels = presences[testSet]))[1, 4]
    #
    #                              }
    #
    #   snow::stopCluster(cluster)
    #
    #   foreach::registerDoSEQ()
    #
    # } else {

    for(k in seq_len(kf)){

      trainSet <- unlist(folds[-k])

      testSet <- unlist(folds[k])

      # mxnet <- maxnet::maxnet(p = presences[trainSet],
      #                         data = covariates[trainSet, ],
      #                         regmult = m, # regularisation multiplier
      #                         maxnet::maxnet.formula(p = presences[trainSet],
      #                                                data = covariates[trainSet, ],
      #                                                classes = features))

      if(inherits(try(
        maxmod <- dismo::maxent(x = covariates[trainSet, ],
                                p = presences[trainSet],
                                removeDuplicates = FALSE,
                                path = filepath,
                                args = as.character(unlist(grid[n, ]))
        )
      ), "try-error")){
        next
      }

      prediction <- as.vector(predict(maxmod,
                                      covariates[testSet, ],
                                      args = "outputformat=cloglog"))

      modAUC[k] <- precrec::auc(precrec::evalmod(scores = prediction,
                                                 labels = presences[testSet]))[1, 4]

    }
    # }
    AUCs[n] <- mean(unlist(modAUC))
  }

  best_param <- as.character(unlist(grid[which.max(AUCs), ]))

  return(best_param)

}
