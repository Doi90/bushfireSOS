#' Evaluate models predictive performance by cross-validation
#'
#' @param spp_data Species data
#' @param type Choose "po" for presence-only species data and "pa" for presence-absence
#' @param k Integer. Number of cross-validation folds (default = 5)
#' @param parallel Logical. To run cross-validation in parallel
#' @param ncors Integer. Number of CPU cores to use
#'
#' @return Returns mean and print mean and standard deviation of model's AUC
#' @export
#'
#' @examples

cross_validate <- function(spp_data,
                           type = c("po", "pa"),
                           k = 5,
                           parallel = TRUE,
                           ncors = 4){

  require(foreach)

  ncors <- min(ncors,
               detectCores() - 1)

  df <- spp_data$data

  ## Check if the arguments are correct

  type <- match.arg(type)

  k <- as.integer(k)

  ncors <- as.integer(ncors)

  ## Cross-validation folds

  folds <- caret::createFolds(df$Value,
                              k)
  if(parallel){

    ## Make a parallel computing cluster

    cluster <- snow::makeCluster(ncors,
                                 type = "SOCK")

    doSNOW::registerDoSNOW(cluster)

    pp <- foreach::foreach(ks = seq_len(k),
                           .inorder = TRUE,
                           .export = c("fit_pres_bg_model",
                                       "regularisedMaxent",
                                       "fit_pres_abs_model"),
                           .packages = c('maxnet',
                                         'precrec')) %dopar% {

                                           trainSet <- unlist(folds[-ks])

                                           testSet <- unlist(folds[ks])

                                           if(type == "po"){

                                             ## Fit a maxent

                                             mxnt <- fit_pres_bg_model(df[trainSet, ],
                                                                       tuneParam = TRUE,
                                                                       parallel = FALSE) # parallel must be FALSE here

                                             prediction <- predict(mxnt,
                                                                   df[testSet, 14:ncol(df)],
                                                                   type = "cloglog")

                                           } else {

                                             ## fit a brt

                                             brt <- fit_pres_abs_model(df[trainSet, ])

                                             prediction <- predict(brt,
                                                                   df[testSet , 14:ncol(df)],
                                                                   n.trees = brt$gbm.call$best.trees, type = "response")

                                           }

                                           ## Calculate the AUC

                                           aucs <- precrec::auc(precrec::evalmod(scores = prediction,
                                                                                 labels = df$Value[testSet]))[1, 4]

                                           auprg <- prg::calc_auprg(prg::create_prg_curve(labels = df$Value,
                                                                                          pos_scores = prediction))

                                           outauc <- data.frame(roc = aucs,
                                                                prg = auprg)

                                         }

    snow::stopCluster(cluster)

    foreach::registerDoSEQ()

    aucboth <- do.call(rbind.data.frame,
                       pp)

  } else {

    # pp <- vector(mode = "numeric", length = k)

    aucboth <- data.frame(roc = rep(0, k),
                          prg = 0)

    for(ks in seq_len(k)){

      trainSet <- unlist(folds[-ks])

      testSet <- unlist(folds[ks])

      if(type == "po"){

        ## fit a maxent

        mxnt <- fit_pres_bg_model(df[trainSet, ],
                                  tuneParam = TRUE,
                                  parallel = FALSE, # can be TRUE
                                  ncors = ncors)

        prediction <- predict(mxnt,
                              df[testSet, 14:ncol(df)],
                              type = "cloglog")

      } else {

        # fit a brt

        brt <- fit_pres_abs_model(df[trainSet, ])

        prediction <- predict(brt,
                              df[testSet , 14:ncol(df)],
                              n.trees = brt$gbm.call$best.trees,
                              type = "response")

      }

      ## Calculate the AUC

      aucboth$roc[ks] <- precrec::auc(precrec::evalmod(scores = prediction,
                                                       labels = df$Value[testSet]))[1, 4]

      aucboth$prg[ks] <- prg::calc_auprg(prg::create_prg_curve(labels = df$Value,
                                                               pos_scores = prediction))

    }
  }

  cat("Summary of the evaluation:\n")

  cat(sprintf("AUC-ROC score: %s ; SE = %s \n",
              round(mean(aucboth$roc), 4),
              round(sd(aucboth$roc) / sqrt(k), 4)))

  cat(sprintf("AUC-PRG score: %s ; SE = %s \n",
              round(mean(aucboth$prg), 4),
              round(sd(aucboth$prg) / sqrt(k), 4)))

  return(c("ROC" = round(mean(aucboth$roc), 4),
           "PRG" = round(mean(aucboth$prg), 4)))

}
