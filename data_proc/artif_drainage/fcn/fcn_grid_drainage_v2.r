# /----------------------------------------------------------------------------#
#/   Get function that gets polygons for each row.
#    Sometimes same country multiple years

source("./scripts/data_proc/fcn/fcn_get_polygons_for_drainage.r")

# extract from raster stack
source("./scripts/data_proc/fcn/fcn_raster_in_poly_sum_area.r")



# /----------------------------------------------------------------------------#
#/    Function description: This function takes in a list of countries

# It then selects the shapefile for each country (a single shp for every year)
# It then extracts the LU area from the appropriate raster (crop, forest, peat).


# data = 
# draintype = 
# raster_stack
process_drainage_stats <- function(data, draintype, raster_stack){
  
  
  # /--------------------------------------------------------------------------#
  #/    Make shapefile of national drainage stats                       --------

  
  # filter drainage records by type
  # drained_type <-   drained %>% dplyr::filter(type == draintype)
  
  # make shapefile with a polygon for each record row (some repeat because data from multiple years)
  artdrain_type_nat <- sel_countries_in_shp(drained_type, countries, "iso_a3", "iso_a3", "rec_id")
  
  # join the data table to shapefile, using rec_id as key
  artdrain_type_nat@data <- merge(artdrain_type_nat@data,
                                  as.data.frame(drained),
                                  by="rec_id", all.x=T, all.y=F)
  

  
  
  
  
  # /--------------------------------------------------------------------------#
  #/    Extract loss-driver LU area from raster                          -------
  

  # run function that loops through polygons and extracts area from closest-yr raster
  year_vector <- seq(1700,2000,10)
  area_extract <- get_raster_closest_yr(draintype, artdrain_type_nat, raster_stack, year_vector)


  # select a few columns (to avoid the .x & .y  duplicated columns)
  area_extract <- area_extract[,c( "rec_id", draintype, "closest_lu_year")]
  area_extract[, "area_extr"] <- area_extract[,draintype]
  area_extract[, draintype] <- NULL
  area_extract["type"] <- draintype

  
  # exclude all useless columns from Shapefile
  cols_to_keep <- names(artdrain_type_nat)[c(1,66:76)]
  artdrain_type_nat@data <- artdrain_type_nat@data[,cols_to_keep]
  
  # merge new columns to polygons
  artdrain_type_nat <- merge(artdrain_type_nat, area_extract, by="rec_id")
  
  
  
  # /--------------------------------------------------------------------------#
  #/     CALCULATE PERCENTAGE                                            -------
  
  # # calculate percentage of cropla drained
  # artdrain_type_nat$f_drained <- (artdrain_type_nat$drained_area_tot) / artdrain_type_nat@data[,"area_extr"]
  # # impose ceiling of value=1 to countries where % is above it
  # artdrain_type_nat[ artdrain_type_nat$f_drained > 1, "f_drained"] <- 1
  
  
  
  # /--------------------------------------------------------------------------#
  #/  POLY MAP THE STATS                                                 -------

  # makes a country polygon map of the % drained
  # Note: Source'd code is evaluated in the global environment by default. 
  #       Set local=TRUE to evaluate the code in the calling environment.
  source('./scripts/plots/artif_drainage/map_percent_drained.r', local=TRUE)
  
  # return dataframe with percentatges
  return(as.data.frame(artdrain_type_nat@data))
  
}

