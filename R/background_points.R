#' Generate background points
#'
#' @param species Charaacter vector of species name
#' @param spp_data List of species data
#' @param env_data Rasters tack
#' @param guild Character vector
#' @param region Character vector
#' @param background_group Character vector. One of: vertebrates, plants, insects
#'
#' @return
#' @export
#'
#' @examples

background_points <- function(species,
                              spp_data,
                              guild,
                              region,
                              background_group,
                              bias_layer,
                              sample_min){

  ## Region formatting

  region <- dplyr::case_when(region == "VIC" ~ "Victoria",
                             region == "NSW" ~ "New South Wales",
                             region == "QLD" ~ "Queensland",
                             region == "SA" ~ "South Australia",
                             region == "WA" ~ "Western Australia",
                             region == "TAS" ~ "Tasmania")

  ## Read in full target group background data

  if(background_group == "vertebrates"){
    all_background <- background_vertebrates
  }

  ## Filter background points

  filter_bg <- all_background[all_background$Guild == guild, ]

  ## Filter by region

  filter_bg <- mask_species_data(filter_bg,
                                 region)

  ## Remove observations without dates

  filter_bg <- filter_bg[!is.na(filter_bg$Date), ]

  ## Remove old observations

  filter_bg <- filter_bg[filter_bg$Date >= "1970-01-01", ]

  ## Remove uncertain observations

  filter_bg <- filter_bg[filter_bg$Coordinate.Uncertainty.in.Metres < 10000, ]


  ## Sample from remaining background points
  ##TODO Set number of samples based on number of presences?

  n_samples <- max(sample_min,
                   length(region) * 1000)

  if(nrow(filter_bg) >= n_samples){

    bg <- filter_bg[sample(seq_len(nrow(filter_bg)),
                           n_samples,
                           replace = FALSE), ]

    bg$Value <- 0

  } else {

    ## Generate background points

    bg_dismo <- dismo::randomPoints(raster::raster(bias_layer),
                                    10000,
                                    prob = TRUE)

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

  ## Filter background points by pixel

  bg <- pixel_filtering(data = bg,
                        raster = raster::raster(bias_layer))

  ## Combine spp and bg dfs

  spp_data$data$Value <- 1

  spp_data$data$Guild <- guild

  spp_data$data <- rbind(spp_data$data,
                         bg)

  return(spp_data)

}

