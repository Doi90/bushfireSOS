#' get state names containing species data
#'
#' @param spdata a dataframe containing columns Longitude and Latitude, in WGS84 projection
#' @param region string vector specifying fullname of state to subset to (with Capitalisation) - could probably implement a lookup table of three-letter codes to states, not sure if necessary (thinking in the future we will need to subset to other types of regions) - no longer needed
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#'

species_data_get_state_character <- function(spdata){

  ## Check if the inputs are in expected format

  if(!is.data.frame(spdata)){
    stop("input not in expected classes")
  }

  ## Check if expected column names exist

  if(any(c("Longitude","Latitude") %nin% colnames(spdata))){
    stop("Longitude and Latitude columns not found in species data, check column names")
  }

  ## Convert spdata to a sf object

  spdata.sf <- sf::st_as_sf(spdata,
                            coords = c("Longitude", "Latitude"),
                            crs = 4326)

  #reproject to Australian Albers
  spdata.sf <- sf::st_transform(spdata.sf,
                                crs = 3577)

  #override original long/lat coordinates
  #note that this makes column names incorrect
  spdata[,c("Longitude", "Latitude")] <- sf::st_coordinates(spdata.sf)

  ## Get state polygon data and make it sf

  AUS.shapes <- rnaturalearth::ne_states("australia",
                                         returnclass = "sf")

  AUS.shapes <- sf::st_transform(AUS.shapes,
                                 crs = 3577)

  ## Assign state to each record

  spdata.state.assignment <- unlist(sf::st_intersects(spdata.sf,
                                                      AUS.shapes))

  spdata.state.assignment <- AUS.shapes$name[spdata.state.assignment]


  ## Summarise states to character vectors

  region <- unique(spdata.state.assignment)


  ## Convert region character

  region <- gsub("Victoria","VIC", region)
  region <- gsub("New South Wales", "NSW", region)
  region <- gsub("Queensland", "QLD", region)
  region <- gsub("South Australia", "SA", region)
  region <- gsub("Western Australia", "WA", region)
  region <- gsub("Northern Territory", "NT", region)
  region <- gsub("Tasmania", "TAS", region)
  region <- gsub("Australian Capital Territory", "ACT", region)



  return(region)

}
