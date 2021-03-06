#' Load presence records from NSW database
#'
#' @param species Character vector of species scientific name
#' @param region
#' @param save.map Logical value to indicate saving the map to file or not
#' @param map.directory File path to output folder for saving map
#'
#' @return
#' @export
#'
#' @examples
#'
#'


load_pres_bg_data_NSW_improper <- function(dir_path,
                                           species,
                                           region = "all",
                                           save.map = TRUE,
                                           map.directory = "."){

  #TODO Check coordinate projections

  #######################
  ### Name Processing ###
  #######################

  # ## Check if name is properly formed
  #
  # if(any(grepl("sp. ",species, fixed = TRUE),
  #        grepl("'",species, fixed = TRUE),
  #        grepl("\"",species, fixed = TRUE)
  #        #add any other exceptions here
  # )){
  #   stop("Not run: scientific name not properly formed!")
  # }
  #
  # ## Clean name of bracket suffix
  ### - assuming the brackets denote subpopulation, and not taxonomic notations

  species <- stringr::str_remove(species,
                                 "\\(.*\\)")

  ## Remove trailing whitespace

  species <- stringr::str_squish(species)

  #################
  ### Load Data ###
  #################

  NSW_data <- data.table::fread(sprintf("%s/NSW_%s.txt",
                                        dir_path,
                                        gsub(" ",
                                             "_",
                                             tolower(species))),
                                stringsAsFactors = FALSE,
                                sep = "\t",
                                skip = 4,
                                header = TRUE,
                                fill = TRUE)

  if(nrow(NSW_data) == 0){
    stop("Not run: no records found")
  }

  ## Format data

  NSW_data$ObservationType <- ifelse(all(NSW_data$ObservationType == TRUE),
                                     "T",
                                     NSW_data$ObservationType)

  NSW_data$ObservationType <- ifelse(all(NSW_data$ObservationType == FALSE),
                                     "F",
                                     NSW_data$ObservationType)

  df <- data.frame("ID" = seq_len(nrow(NSW_data)),
                   "Origin" = NSW_data$DatasetName,
                   "Species" = NSW_data$ScientificName,
                   "Longitude" = NSW_data$Longitude_GDA94,
                   "Latitude" = NSW_data$Latitude_GDA94,
                   #add date for duplicate processing
                   "Date" = lubridate::as_date(NSW_data$DateFirst, format = "%d/%m/%Y", tz = "Australia/Sydney"),
                   "Basis.of.Record" = NSW_data$ObservationType,
                   "Locality" = NSW_data$Description,
                   "Institute" = NSW_data$DatasetName,
                   "Collection" = NSW_data$Observers,
                   "Coordinate.Uncertainty.in.Metres" = NSW_data$Accuracy,
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

  return(list(processed.data = df,
              raw.NSW.data = NSW_data,
              rounding.comment = suspect.rounding))

}


