#' Mask species (both presence and backgrounds) data to states
#'
#' @param spdata a dataframe containing columns Longitude and Latitude, in WGS84 projection
#' @param region string vector specifying fullname of state to subset to (with Capitalisation) - could probably implement a lookup table of three-letter codes to states, not sure if necessary (thinking in the future we will need to subset to other types of regions)
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#'

mask_species_data <- function(spdata, region){

  ## Check if the inputs are in expected format

  if(!is.data.frame(spdata) | !is.character(region)){
    stop("input not in expected classes")
  }

  ## Don't forget the ACT

  if("NSW" %in% region){

    region <- c(region, "ACT")

  }

  ## Convert region character

  region <- gsub("VIC", "Victoria", region)
  region <- gsub("NSW", "New South Wales", region)
  region <- gsub("QLD", "Queensland", region)
  region <- gsub("SA", "South Australia", region)
  region <- gsub("WA", "Western Australia", region)
  region <- gsub("NT", "Northern Territory", region)
  region <- gsub("TAS", "Tasmania", region)
  region <- gsub("ACT", "Australian Capital Territory", region)

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

  ## Get state polygon data and make it sf

  AUS.shapes <- rnaturalearth::ne_states("australia",
                                         returnclass = "sf")

  AUS.shapes <- sf::st_transform(AUS.shapes,
                                 crs = 3577)

  ## Assign state to each record

  spdata.state.assignment <- unlist(sf::st_intersects(spdata.sf,
                                                      AUS.shapes))

  spdata.state.assignment <- AUS.shapes$name[spdata.state.assignment]

  ## Subset to just target state

  spdata.target.state <- spdata[spdata.state.assignment %in% region,]

  return(spdata.target.state)

}
