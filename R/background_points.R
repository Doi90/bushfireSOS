background_points <- function(species,
                              spp_data,
                              env_data,
                              guild,
                              region){

  ## Read in full target group background data
  #TODO filepath and/or make it a package data object

  all_background <- readRDS("bg.rds")

  ## Filter background points

  filter_bg <- all_background[all_background$Guild == guild &
                                all_background$Species != species, ]

  #TODO Add region filter here

  filter_bg <- mask_species_data(filter_bg,
                                 region)

  ## Remove observations without dates

  filter_bg <- filter_bg[!is.na(filter_bg$Date), ]

  ## Remove old observations

  filter_bg <- filter_bg[filter_bg$Date >= "1970-01-01", ]


  ## Sample from remaining background points
  ##TODO Set number of samples based on number of presences?

  if(nrow(filter_bg) >= 1000){

    bg <- filter_bg[sample(seq_len(nrow(filter_bg)),
                           1000,
                           replace = FALSE), ]

    bg$value <- 0

  } else {

    ## Generate background points

    bg_dismo <- dismo::randomPoints(env_data[[1]],
                                    10000)

    bg <- data.frame(ID = NA,
                     Origin = NA,
                     Species = NA,
                     Longitude = bg_dismo[ , 1],
                     Latitude = bg_dismo[ , 2],
                     Date = NA,
                     Basis.of.Record = NA,
                     Locality = NA,
                     Institute = NA,
                     Collection = NA,
                     Coordinate.Uncertainty.in.Metres =NA,
                     Guild = NA,
                     Value = 0,
                     stringsAsFactors = FALSE)

  }

  ## Combine spp and bg dfs

  spp_data$processed.data$Value <- 1

  spp_data$processed.data <- rbind(spp_data$processed.data,
                                   bg)

  return(spp_data)

}

