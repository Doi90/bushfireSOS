library(bushfireSOS)
library(openxlsx)

df <- read.xlsx("bushfireResponse_data/misc/Species_existing_monitoring_updated_list.xlsx",
                sheet = "rawSpSheet")

df <- df[-1, c("Scientific.name", "Group")]

head(df)

df_list <- split(df, seq_len(nrow(df)))

data_list <- lapply(df_list,
                    function(x){

                      tmp <- tryCatch(expr = load_pres_bg_data_AUS(species = x$Scientific.name,
                                                                   file.vic = "bushfireResponse_data/spp_data_raw/VIC sensitive species data/FAUNA_requested_spp_ALL.gdb",
                                                                   email = "davidpw@student.unimelb.edu.au",
                                                                   save.map = FALSE,
                                                                   region = c("VIC", "NSW", "QLD", "SA", "NT", "WA", "TAS")),
                                      error = function(err){return(
                                        list(data = data.frame("ID" = numeric(),
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
                                                               stringsAsFactors = FALSE)))})

                      tmp <- tmp$data

                      tmp$Guild <- rep(x$Group, nrow(tmp))

                      return(tmp)

                    })

bg <- do.call(rbind.data.frame,
              data_list)

saveRDS(bg,
        "../bushfireSOS/bg.rds")


