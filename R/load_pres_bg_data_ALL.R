#' Download presence-only records for a species from all state and national databases
#'
#' @param species Character vector of species scientific name
#' @param region Character vector of state databases to search (all caps, 2/3 letter acronymns)
#' @param file.vic File path to the gdb folder
#' @param email Character. To access ALA records
#' @param save.map Logical value to indicate saving the map to file or not
#' @param map.directory File path to output folder for saving map
#'
#' @return
#' @export
#'
#' @examples
#'

load_pres_bg_data_AUS <- function(species,
                                  region = c("VIC","NSW","QLD"),
                                  file.vic,
                                  email,
                                  save.map,
                                  map.directory){

  ########################
  ### Get Species Data ###
  ########################

  ## Output data.frame

  df <- data.frame("ID" = numeric(),
                   "Origin" = character(),
                   "Species" = character(),
                   "Longitude" = numeric(),
                   "Latitude" = numeric(),
                   #add date for duplicate processing
                   "Date" = numeric(),
                   "Basis.of.Record" = character(),
                   "Locality" = character(),
                   "Institute" = character(),
                   "Collection" = character(),
                   "Coordinate.Uncertainty.in.Metres" = numeric(),
                   stringsAsFactors = FALSE)

  ## Output raw data list

  raw_data <- list()

  ## Get state data

  if("VIC" %in% region){

    df_tmp <- tryCatch(expr = load_pres_bg_data_VIC(file = file.vic,
                                                    species = species,
                                                    save.map = FALSE,
                                                    map.directory = map.directory),
                       error = function(err){
                         return(list(processed.data = data.frame("ID" = numeric(),
                                                                 "Origin" = character(),
                                                                 "Species" = character(),
                                                                 "Longitude" = numeric(),
                                                                 "Latitude" = numeric(),
                                                                 #add date for duplicate processing
                                                                 "Date" = lubridate::as_date(numeric()),
                                                                 "Basis.of.Record" = character(),
                                                                 "Locality" = character(),
                                                                 "Institute" = character(),
                                                                 "Collection" = character(),
                                                                 "Coordinate.Uncertainty.in.Metres" = numeric(),
                                                                 stringsAsFactors = FALSE)))
                       })

    df <- rbind(df,
                df_tmp$processed.data)

    if(is.list(df_tmp)){

      raw_data$VIC <- df_tmp$raw.VIC.data

    }

  }

  if("NSW" %in% region){

    df_tmp <- tryCatch(expr = load_pres_bg_data_NSW(species = species,
                                                    save.map = FALSE,
                                                    map.directory = map.directory),
                       error = function(err){
                         return(list(processed.data = data.frame("ID" = numeric(),
                                                                 "Origin" = character(),
                                                                 "Species" = character(),
                                                                 "Longitude" = numeric(),
                                                                 "Latitude" = numeric(),
                                                                 #add date for duplicate processing
                                                                 "Date" = lubridate::as_date(numeric()),
                                                                 "Basis.of.Record" = character(),
                                                                 "Locality" = character(),
                                                                 "Institute" = character(),
                                                                 "Collection" = character(),
                                                                 "Coordinate.Uncertainty.in.Metres" = numeric(),
                                                                 stringsAsFactors = FALSE)))
                       })

    df <- rbind(df,
                df_tmp$processed.data)

    if(is.list(df_tmp)){

      raw_data$NSW <- df_tmp$raw.NSW.data

    }

  }

  if("QLD" %in% region){

    df_tmp <- tryCatch(expr = load_pres_bg_data_QLD(species = species,
                                                    save.map = FALSE,
                                                    map.directory = map.directory),
                       error = function(err){
                         return(list(processed.data = data.frame("ID" = numeric(),
                                                                 "Origin" = character(),
                                                                 "Species" = character(),
                                                                 "Longitude" = numeric(),
                                                                 "Latitude" = numeric(),
                                                                 #add date for duplicate processing
                                                                 "Date" = lubridate::as_date(numeric()),
                                                                 "Basis.of.Record" = character(),
                                                                 "Locality" = character(),
                                                                 "Institute" = character(),
                                                                 "Collection" = character(),
                                                                 "Coordinate.Uncertainty.in.Metres" = numeric(),
                                                                 stringsAsFactors = FALSE)))
                       })

    df <- rbind(df,
                df_tmp$processed.data)

    if(is.list(df_tmp)){

      raw_data$QLD <- df_tmp$raw.QLD.data

    }

  }

  # if("SA" %in% region){
  #
  #   df_tmp <- tryCatch(expr = load_pres_bg_data_SA(species = species,
  #                                                  save.map = TRUE,
  #                                                  map.directory = map.directory),
  #                      error = function(err){data.frame("ID" = numeric(),
  #                                                       "Origin" = character(),
  #                                                       "Species" = character(),
  #                                                       "Longitude" = numeric(),
  #                                                       "Latitude" = numeric(),
  #                                                       #add date for duplicate processing
  #                                                       "Date" = numeric(),
  #                                                       "Basis.of.Record" = character(),
  #                                                       "Locality" = character(),
  #                                                       "Institute" = character(),
  #                                                       "Collection" = character(),
  #                                                       "Coordinate.Uncertainty.in.Metres" = numeric(),
  #                                                       stringsAsFactors = FALSE)})
  #
  #   df <- rbind(df,
  #               df_tmp$processed.data)
  #
  # }

  # if("WA" %in% region){
  #
  #   df_tmp <- tryCatch(expr = load_pres_bg_data_WA(species = species,
  #                                                  save.map = TRUE,
  #                                                  map.directory = map.directory),
  #                      error = function(err){data.frame("ID" = numeric(),
  #                                                       "Origin" = character(),
  #                                                       "Species" = character(),
  #                                                       "Longitude" = numeric(),
  #                                                       "Latitude" = numeric(),
  #                                                       #add date for duplicate processing
  #                                                       "Date" = numeric(),
  #                                                       "Basis.of.Record" = character(),
  #                                                       "Locality" = character(),
  #                                                       "Institute" = character(),
  #                                                       "Collection" = character(),
  #                                                       "Coordinate.Uncertainty.in.Metres" = numeric(),
  #                                                       stringsAsFactors = FALSE)})
  #
  #   df <- rbind(df,
  #               df_tmp$processed.data)
  #
  # }

  ## Get national data

  df_tmp <- tryCatch(expr = load_pres_bg_data(species = species,
                                              save.map = FALSE,
                                              map.directory = map.directory,
                                              email = email),
                     error = function(err){
                       return(list(processed.data = data.frame("ID" = numeric(),
                                                               "Origin" = character(),
                                                               "Species" = character(),
                                                               "Longitude" = numeric(),
                                                               "Latitude" = numeric(),
                                                               #add date for duplicate processing
                                                               "Date" = lubridate::as_date(numeric()),
                                                               "Basis.of.Record" = character(),
                                                               "Locality" = character(),
                                                               "Institute" = character(),
                                                               "Collection" = character(),
                                                               "Coordinate.Uncertainty.in.Metres" = numeric(),
                                                               stringsAsFactors = FALSE)))
                     })

  df <- rbind(df,
              df_tmp$processed.data)

  if(is.list(df_tmp)){

    raw_data$ala <- df_tmp$raw.ala.data
    raw_data$gbif <- df_tmp$raw.gbif.data

  }

  ## Check for duplicate records due to state database overlap

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

  ## Clean records using coord cleaner

  df <- CoordinateCleaner::clean_coordinates(df,
                                             lon = "Longitude",
                                             lat = "Latitude",
                                             species = "Species",
                                             tests = c("capitals",
                                                       "centroids",
                                                       "equal",
                                                       "gbif",
                                                       "institutions",
                                                       "seas",
                                                       "zeros"),
                                             #skip urban test - keeps giving proj4string errors, will look into later
                                             # urban_ref = as_Spatial(read_sf("Data/GIS/ne_50m_urban_areas/ne_50m_urban_areas.shp")),
                                             seas_ref =  NULL, #as_Spatial(read_sf("Data/GIS/ne_50m_land/ne_50m_land.shp")),

                                             #ignore outliers for now
                                             # outliers_method = "distance",
                                             # outliers_td = 1500, #outlier bit probably needs tweaking, its curently set to be very conservative
                                             value = "clean")

  # ## Check if duplicate long or lat - could be signal of rounding
  #
  # suspect.rounding <- ifelse(any(anyDuplicated(df$df),
  #                                anyDuplicated(df$Latitude)),
  #                            "duplicate long/lat found - suspect rounding",
  #                            NA)

  ####################
  ### Plot Records ###
  ####################

  ## Visualise those with fewer than 1k records
  ### (can tweak this - I just think there isn't much point to manual input
  ###  when looking at more than 1k data)

  if(nrow(df) <= 1000){

    sp.sf <- sf::st_as_sf(df,
                          coords = (4:5),
                          crs = sp::CRS("+proj=longlat +datum=WGS84"))#all ALA and GBIF coord should be in wgs84 - but this needs attention when adding more dataset in the future (and also some of ALA may be gda94 but incorrectly labelled according to Lee Belbin (I think?) - but this may be beyond our ability to fix)

    sp.map <- mapview::mapview(sp.sf,
                               layer.name = species,
                               homebutton = FALSE)

    if(save.map == TRUE){

      map_filename <- sprintf("%s/%s.html",
                              map.directory,
                              gsub(" ",
                                   "_",
                                   species))

      htmlwidgets::saveWidget(sp.map@map,
                              file = map_filename)

      cat(paste0("Map is saved to ", map_filename), "\n")

    }

  } else {

    sp.map <- "more than 1k records, not mapped"

  }

  return(list(data = df,
              raw.data = raw_data,
              map = sp.map))

}
