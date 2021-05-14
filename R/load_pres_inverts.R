### Function to read the invertebrates data provided by Payal into the SDMs workflow

load_pres_inverts <- function(file="") {
  
  # load Payal's data
  data <- read.csv(file)
  
  # change names of columns that are the same
  colnames(data)[1:3] <- c("Species", "Latitude", "Longitude")
  
  # filter species
  data <- data[data$Species %in% species,]
  
  data <- data[!is.na(data$year),]
  
  data <- data[data$year >= 1970, ]
  
  data$ID <- seq(1:nrow(data))
  
  # keep only necessary columns
  data <- data[,c(13,1,3,2)]
  
  spp_data <- list(data)
  names(spp_data)[1] <- "data"
  
  return(spp_data)
}

