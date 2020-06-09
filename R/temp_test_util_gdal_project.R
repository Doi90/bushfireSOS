
###test reproject
reproject_AA_gdal(source_spatial_file = "/data/test_stack_masked.tif",
                  source_crs_override = "'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs'",
                    output_file = "/data/test_stack_masked_reprojected.tif")

