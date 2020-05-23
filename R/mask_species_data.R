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

      ##check if the inputs are in expected format
      if(!is.data.frame(spdata) | !is.character(region)){
        stop("input not in expected classes")
      }

      #check if expected column names exist
      if(any(c("Longitude","Latitude") %nin% colnames(spdata))){
        stop("Longitude and Latitude columns not found in species data, check column names")
      }

      #convert spdata to a sf object
      spdata.sf <- sf::st_as_sf(spdata,coords = c("Longitude","Latitude"),crs = 4326)

      #get state polygon data and make it sf
      AUS.shapes <- rnaturalearth::ne_states("australia",returnclass = "sf")

      #assign state to each record
      spdata.state.assignment <- unlist(sf::st_intersects(spdata.sf, AUS.shapes))
      spdata.state.assignment <- AUS.shapes$name[spdata.state.assignment]

      #subset to just target state
      spdata.target.state <- spdata[spdata.state.assignment %in% region,]

      return(spdata.target.state)
    }
