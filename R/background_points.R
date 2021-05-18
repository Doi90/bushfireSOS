#' Fast(er) random probability sampling from raster files
#'
#' @param r raster file
#' @param size sample size
#'
#' @return coordinate matrix
#' @export
#'
#' @examples
fastRandomPoints <- function(r, size = 1e4) {
  if(raster::nlayers(r) > 1) r <- r[[1]]
  val <- raster::getValues(r)
  val_notNA <- which(!is.na(val))
  x <- sample(val_notNA, size, prob = val[val_notNA])
  pts <- raster::xyFromCell(r, x)
  return(pts)
}



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
                              sample_min,
                              dismo_sampling = FALSE,
                              KI_mask = FALSE){

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

  if(background_group == "vertebrates_1990"){
    all_background <- background_vertebrates_1990
  }

  if(background_group == "invertebrates"){
    all_background <- background_vertebrates_1990
    all_background$Guild <- "invertebrates"
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

  ## Filter background points by pixel

  filter_bg <- pixel_filtering(data = filter_bg,
                               raster = raster::raster(bias_layer))

  ## Sample from remaining background points
  ##TODO Set number of samples based on number of presences?

  n_samples <- max(sample_min,
                   length(region) * 1000)

  if(nrow(filter_bg) >= n_samples){

    bg <- filter_bg

    bg$Value <- 0

    type <- "target group background"

  } else {

    # mask the bias layer based on region

    bias_inv <- mask_data(env_data = raster::raster(bias_layer),
                          region = region,
                          crop = TRUE)


    # add KI mask
    if(KI_mask == TRUE){
      bias_inv <- raster::crop(bias_inv,raster::extent(406038,553809,-3970872,-3892487))
    }

    ## invert the values
    bias_inv <- raster::setMinMax(bias_inv)
    bias_inv <- (raster::maxValue(bias_inv) - bias_inv) / (raster::maxValue(bias_inv) - raster::minValue(bias_inv))

    ## Generate background points

    if(dismo_sampling){
      bg_dismo <- dismo::randomPoints(bias_inv,
                                      10000,
                                      prob = TRUE)
    } else{
      bg_dismo <- fastRandomPoints(bias_inv, size = 10000)
    }


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

    type <- "random background"

  }

    ## Filter presence points by pixel

  spp_data$data <- mask_species_data(spp_data$data,
                                     region)

  spp_data$data <- pixel_filtering(data = spp_data$data,
                                   raster = raster::raster(bias_layer))

  ## Combine spp and bg dfs

  spp_data$data$Value <- 1

  spp_data$data$Guild <- guild

  spp_data$data <- rbind(spp_data$data,
                         bg)

  message(sprintf("BG type: %s",
                  type))

  return(spp_data)

}

