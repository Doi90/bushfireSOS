background_points <- function(species,
                              spp_data,
                              env_data,
                              guild,
                              region){

  ## Read in full target group background data

  all_background <- readsRDS("")

  ## Filter background points

  filter_bg <- all_background[all_background$Guild == guild &
                                all_background$Species != species, ]

  #TODO Add region filter here

  filter_bg <- filter_bg


  ## Sample from remaining background points
  ##TODO Set number of samples based on number of presences?
  ##TODO Column subsetting

  if(nrow(filter_bg) >= 1000){

    bg <- filter_bg[sample(seq_len(nrow(filter_bg)),
                           1000,
                           replace = FALSE), ]

  } else {

    ## Generate background points

    bg <- dismo::randomPoints(env_data[[1]],
                              10000)

    bg$value <- 0

    colnames(bg) <- c("lon",
                      "lat",
                      "value")

  }

  ## Combine spp and bg dfs


}
