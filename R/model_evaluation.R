#' Evaluate models predictive performance by cross-validation
#'
#' @param spp_data Species data
#' @param type Choose "po" for presence-only species data and "pa" for presence-absence
#' @param k Integer. Number of cross-validation folds (default = 5)
# @param parallel Logical. To run cross-validation in parallel
# @param ncors Integer. Number of CPU cores to use
#' @param tuneParam_CV Logical. Whether to tune the regularisation multiplier
#' @param filepath The path to Maxent files. Note that the path should be complete (i.e. it does not accept path like this: "~/Desktop/output")
#'
#' @return Returns mean and print mean and standard deviation of model's AUC
#' @export
#'
#' @examples

cross_validate <- function(spp_data,
                           type = c("po", "pa"),
                           k = 5,
                           filepath,
                           # features = c("default", "lqp", "lqh", "lq", "l"),
                           # parallel = FALSE,
                           # parallel_tuning = FALSE,
                           # ncors = 4,
                           tuneParam_CV = TRUE){

  # features <- match.arg(features)

  df <- spp_data$data

  ## Check if the arguments are correct

  type <- match.arg(type)

  k <- as.integer(k)

  # ncors <- as.integer(ncors)

  ## Stratified cross-validation folds
  folds <- caret::createFolds(y = as.factor(df$Value), k)

  # if(parallel){
  #
  #   require(foreach)
  #   ncors <- min(ncors,
  #                parallel::detectCores() - 1)
  #   ## Make a parallel computing cluster
  #
  #   cluster <- snow::makeCluster(ncors,
  #                                type = "SOCK")
  #
  #   doSNOW::registerDoSNOW(cluster)
  #
  #   pp <- foreach::foreach(ks = seq_len(k),
  #                          .inorder = TRUE,
  #                          .export = c("fit_pres_bg_model",
  #                                      "regularisedMaxent",
  #                                      "fit_pres_abs_model"),
  #                          .packages = c('precrec',
  #                                        # 'maxnet',
  #                                        'ecospat',
  #                                        'dismo')) %dopar% {
  #
  #                                          trainSet <- unlist(folds[-ks])
  #
  #                                          testSet <- unlist(folds[ks])
  #
  #                                          if(type == "po"){
  #
  #                                            ## Fit a maxent
  #
  #                                            mxnt <- fit_pres_bg_model(spp_data = list(data = df[trainSet, ]),
  #                                                                      tuneParam = tuneParam_CV,
  #                                                                      filepath = filepath,
  #                                                                      k = k,
  #                                                                      # features = features,
  #                                                                      parallel = FALSE) # parallel must be FALSE here
  #
  #                                            prediction <- as.vector(predict(mxnt,
  #                                                                            df[testSet, 14:ncol(df)],
  #                                                                            args = "outputformat=cloglog"))
  #
  #                                          } else {
  #
  #                                            ## fit a brt
  #
  #                                            brt <- fit_pres_abs_model(spp_data = list(data = df[trainSet, ]))
  #
  #                                            prediction <- predict(brt,
  #                                                                  df[testSet , 14:ncol(df)],
  #                                                                  n.trees = brt$gbm.call$best.trees,
  #                                                                  type = "response")
  #
  #                                          }
  #
  #                                          ## Calculate the AUC
  #
  #                                          aucs <- precrec::auc(precrec::evalmod(scores = prediction,
  #                                                                                labels = df$Value[testSet]))[1, 4]
  #
  #                                          # Calculate Boyce index
  #
  #                                          pb_test <- df$Value[testSet]
  #                                          pres_indx <- which(pb_test == 1)
  #
  #                                          byc <- ecospat::ecospat.boyce(fit = prediction,
  #                                                                        obs = prediction[pres_indx],
  #                                                                        PEplot = FALSE)$Spearman.cor
  #
  #                                          # calculate the best threshold
  #                                          thedata <- data.frame(id = 1:length(prediction),
  #                                                                obs = df$Value[testSet],
  #                                                                pred = prediction)
  #
  #                                          thr <- PresenceAbsence::optimal.thresholds(DATA = thedata,
  #                                                                                     threshold = 101,
  #                                                                                     which.model = 1,
  #                                                                                     opt.methods = "MaxSens+Spec")
  #                                          outauc <- data.frame(roc = aucs,
  #                                                               boyce = byc,
  #                                                               threshold = thr$pred[1])
  #
  #                                        }
  #
  #   snow::stopCluster(cluster)
  #
  #   foreach::registerDoSEQ()
  #
  #   aucboth <- do.call(rbind.data.frame,
  #                      pp)
  #
  # } else {

  # pp <- vector(mode = "numeric", length = k)

  aucboth <- data.frame(roc = rep(0, k),
                        boyce = 0,
                        threshold = NA)

  for(ks in seq_len(k)){

    trainSet <- unlist(folds[-ks])

    testSet <- unlist(folds[ks])

    if(type == "po"){

      ## fit a maxent
      # mxnt <- fit_pres_bg_model(list(data = df[trainSet, ]),
      #                           tuneParam = tuneParam_CV,
      #                           parallel = parallel_tuning,
      #                           ncors = ncors,
      #                           features = features)
      mxnt <- fit_pres_bg_model(spp_data = list(data = df[trainSet, ]),
                                tuneParam = tuneParam_CV,
                                filepath = filepath,
                                k = k)
                                # parallel = FALSE) # parallel must be FALSE here

      prediction <- as.vector(predict(mxnt,
                                      df[testSet, 14:ncol(df)],
                                      args = "outputformat=cloglog"))

    } else {

      # fit a brt

      brt <- fit_pres_abs_model(list(data = df[trainSet, ]))

      prediction <- predict(brt,
                            df[testSet , 14:ncol(df)],
                            n.trees = brt$gbm.call$best.trees,
                            type = "response")

    }

    ## Calculate the AUC

    aucboth$roc[ks] <- precrec::auc(precrec::evalmod(scores = prediction,
                                                     labels = df$Value[testSet]))[1, 4]

    # Calculate Boyce index

    pb_test <- df$Value[testSet]
    pres_indx <- which(pb_test == 1)

    aucboth$boyce[ks] <- ecospat::ecospat.boyce(fit = prediction,
                                                obs = prediction[pres_indx],
                                                PEplot = FALSE)$Spearman.cor

    # calculate the best threshold
    thedata <- data.frame(id = 1:length(prediction),
                          obs = df$Value[testSet],
                          pred = prediction)

    thr <- PresenceAbsence::optimal.thresholds(DATA = thedata,
                                               threshold = 101,
                                               which.model = 1,
                                               opt.methods = "MaxSens+Spec")

    aucboth$threshold[ks] <- thr$pred[1]
  }
  # }

  cat("Summary of the evaluation:\n")

  cat(sprintf("AUC-ROC score: %s ; SE = %s \n",
              round(mean(aucboth$roc), 4),
              round(sd(aucboth$roc) / sqrt(k), 4)))

  cat(sprintf("Boyce index: %s ; SE = %s \n",
              round(mean(aucboth$boyce), 4),
              round(sd(aucboth$boyce) / sqrt(k), 4)))

  cat(sprintf("Best threshold: %s ; SE = %s \n",
              round(mean(aucboth$threshold), 4),
              round(sd(aucboth$threshold) / sqrt(k), 3)))

  return(c("AUC" = round(mean(aucboth$roc), 4),
           "Boyce" = round(mean(aucboth$boyce), 4),
           "Threshold" = round(mean(aucboth$threshold), 3)))

}
