#' Load presence records from Invertebrate database
#'
#' @param file Character vector of filepath
#' @param species Character vector of species scientific name
#'
#' @return
#' @export
#'
#' @examples
#'
#'


load_pres_inverts <- function(file,
                              species,
                              date.cutoff = "1970-01-01",
                              uncertainty.cutoff = 1000){

  #TODO Check coordinate projections

  #######################
  ### Name Processing ###
  #######################

  ## Check if name is properly formed

  # if(any(grepl("sp. ",species, fixed = TRUE),
  #        grepl("'",species, fixed = TRUE),
  #        grepl("\"",species, fixed = TRUE)
  #        #add any other exceptions here
  # )){
  #   stop("Not run: scientific name not properly formed!")
  # }
  #
  ## Clean name of bracket suffix
  ### - assuming the brackets denote subpopulation, and not taxonomic notations

  # species <- stringr::str_remove(species,
  #                                "\\(.*\\)")
  #
  ## Remove trailing whitespace

  species <- stringr::str_squish(species)

  #################
  ### Load Data ###
  #################

  ## Load in invert data from file

  data <- read.csv(file,
                   stringsAsFactors = FALSE)

  data <- data[data$scientificName == species, ]

  if(nrow(data) == 0){
    stop("Not run: no records found")
  }

  ## Format data

  df <- data.frame("ID" = seq_len(nrow(data)),
                   "Origin" = "Inverts",
                   "Species" = species,
                   "Longitude" = data$longitude,
                   "Latitude" = data$latitude,
                   #add date for duplicate processing
                   "Date" = lubridate::as_date(as.character(data$year),
                                               format = "%Y"),
                   "Basis.of.Record" = NA,
                   "Locality" = NA,
                   "Institute" = NA,
                   "Collection" = NA,
                   "Coordinate.Uncertainty.in.Metres" = 1,
                   stringsAsFactors = FALSE)

  #####################
  ### Data Cleaning ###
  #####################

  ## Remove spatial duplicates (other duplicate types may matter, think later)

  df$Longitude <- as.numeric(df$Longitude)

  df$Latitude <- as.numeric(df$Latitude)

  df <- df[!duplicated(df[ , c("Longitude", "Latitude")]), ]

  ## Get rid of missing or incomplete long and lats

  df <- df[!is.na(df$Longitude) | !is.na(df$Latitude), ]

  ## Get rid of unusable long lat vals
  ###  (Roozbeh says can save some data here will look into it later)

  df <- df[df$Longitude > -180 &
             df$Longitude < 180 &
             df$Latitude > -90 &
             df$Latitude < 90, ]

  ## Date checks

  df <- df[df$Date > lubridate::as_date(date.cutoff) |
             is.na(df$Date), ]

  ## Coordinate uncertainty

  df$Coordinate.Uncertainty.in.Metres <- as.numeric(df$Coordinate.Uncertainty.in.Metres)

  df <- df[df$Coordinate.Uncertainty.in.Metres <= uncertainty.cutoff & !is.na(df$Coordinate.Uncertainty.in.Metres), ]

  ## Remove weird NAs

  df <- df[!is.na(df$ID), ]

  ## Check if any record left

  if(nrow(df) == 0){
    stop("Not run: no data with legitimate coordinates found")
  }

  # ## Clean records using coord cleaner
  #
  # df <- CoordinateCleaner::clean_coordinates(df,
  #                                            lon = "Longitude",
  #                                            lat = "Latitude",
  #                                            species = "Species",
  #                                            tests = c("capitals",
  #                                                      "centroids",
  #                                                      "equal",
  #                                                      "gbif",
  #                                                      "institutions",
  #                                                      "seas",
  #                                                      "zeros"),
  #                                            #skip urban test - keeps giving proj4string errors, will look into later
  #                                            # urban_ref = as_Spatial(read_sf("Data/GIS/ne_50m_urban_areas/ne_50m_urban_areas.shp")),
  #                                            seas_ref =  NULL, #as_Spatial(read_sf("Data/GIS/ne_50m_land/ne_50m_land.shp")),
  #
  #                                            #ignore outliers for now
  #                                            # outliers_method = "distance",
  #                                            # outliers_td = 1500, #outlier bit probably needs tweaking, its curently set to be very conservative
  #                                            value = "clean")

  ## Check if duplicate long or lat - could be signal of rounding

  suspect.rounding <- ifelse(any(anyDuplicated(df$df),
                                 anyDuplicated(df$Latitude)),
                             "duplicate long/lat found - suspect rounding",
                             NA)

  ##reproject data from GDA94 to WGS84
  sp.sf <- sf::st_as_sf(df,
                        coords = (4:5),
                        crs = 4283)
  sp.sf <- sf::st_transform(sp.sf, 4326)
  df[,4:5] <- sf::st_coordinates(sp.sf)



  # ####################
  # ### Plot Records ###
  # ####################
  #
  # ## Visualise those with fewer than 1k records
  # ### (can tweak this - I just think there isn't much point to manual input
  # ###  when looking at more than 1k data)
  #
  # if(nrow(df) <= 1000){
  #
  #
  #   sp.map <- mapview::mapview(sp.sf,
  #                              layer.name = species,
  #                              homebutton = FALSE)
  #
  #   if(save.map == TRUE){
  #
  #     map_filename <- sprintf("%s/%s.html",
  #                             map.directory,
  #                             gsub(" ",
  #                                  "_",
  #                                  species))
  #
  #     htmlwidgets::saveWidget(sp.map@map,
  #                             file = map_filename)
  #
  #     cat(paste0("Map is saved to ", map_filename), "\n")
  #
  #   }
  #
  # } else {
  #
  #   sp.map <- "more than 1k records, not mapped"
  #
  # }

  return(list(data = df,
              raw.data = data,
              rounding.comment = suspect.rounding))

}


