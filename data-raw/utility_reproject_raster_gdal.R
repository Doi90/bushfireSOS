#reprojection utility function
#requires gdal on machine
#assumes Australian Albers projection, and using predefined ext, change these if required
#source crs should be in single quotation marks eg '+proj=longlat +ellps=GRS80 +no_defs'

reproject_AA_gdal <- function(source_spatial_file,
                              source_crs_override = NULL,
                              output_file){

  #define property for output
  res <- 250
  ext <- raster::extent(c(-1880371,2539811,-6124229,-1002843))
  proj <- sp::CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  if (is.null(source_crs_override)){
    system(paste0("gdalwarp -overwrite -tr ", paste(res, res),
                  " -r average -t_srs '", paste0(proj),"' -te ",
                  paste(ext[1], ext[3], ext[2], ext[4]), " ",
                  getwd(), source_spatial_file, " ", getwd(), output_file))
  }else{
    system(paste0("gdalwarp -overwrite -tr ", paste(res, res),
                  " -s_srs ",source_crs_override,
                  " -r average -t_srs '", paste0(proj),"' -te ",
                  paste(ext[1], ext[3], ext[2], ext[4]), " ",
                  getwd(), source_spatial_file, " ", getwd(), output_file))
  }

  return(print(paste0("output file saved as ",output_file)))

}
