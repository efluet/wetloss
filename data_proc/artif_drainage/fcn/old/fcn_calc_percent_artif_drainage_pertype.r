# Description: This function takes in a list of countries. 
# It then selects the shapefile for each country (a single shp for every year)
# It then extracts the LU area from the appropriate raster (crop, forest, peat).
# Then it calculates the % of area.


process_drainage_stats <- function(data, draintype, raster_stack){
  
  
  #==============================================================================#
  ### make shapefile of national drainage stats ---------------------------------------
  #==============================================================================#
  # extract cropland area for year and country from HYDE to calculate % drained
  

  # filter drainage records by type
  drained_type <-   drained %>% dplyr::filter(type == draintype)
  
  
  # get function that gets polygons for each row, sometimes same country multiple years
  source("./scripts/data_proc/fcn/fcn_get_polygons_for_drainage.r")
  
  # create a shapefile with a polygon for each record row
  # (some repeat because data from multiple years)
  artdrain_type_nat <- sel_countries_in_shp(drained_type, countries, "iso_a3", "iso_a3", "rec_id")
  
  # join the data table to shapefile, using rec_id as key
  artdrain_type_nat@data <- merge(artdrain_type_nat@data, 
                                  as.data.frame(drained),
                                  by="rec_id", all.x=T, all.y=F)
  
  
  # ###==========================================================================#
  # ###   get hyde years                           -------------------------------
  # ###==========================================================================#
  # 
  # hyde_yrs <- readRDS("./output/results/hyde_yrs/hyde_yrs_since1700.rds")
  
  
  #============================================================================#
  ### Extract loss-driver LU area from raster    -------------------------------
  #============================================================================#
  
  # this if statement is a hack to differentiate the NetCDF cropland 
  # and raster stack from forestry and peatland
  # this should be replaced once the data format is standardized
  
  if (draintype != "cropland"){
    # this could be modified to also extract forest/peatlands... later
    source("./scripts/data_proc/fcn/fcn_raster_in_poly_sum_area.r")
    
    # run function  that loops through polygons
    year_vector <- c(1500,1600,1700,seq(1800,2000,10))
    area_extract <- get_raster_closest_yr(draintype, artdrain_type_nat, raster_stack, year_vector)
  
  } else {
  
    source("./scripts/data_proc/fcn/get_hydecroparea_in_polygons.r")
    
    # run function  that loops through polygons
    area_extract <- get_raster_closest_yr(draintype, artdrain_type_nat)
  }
  
  # select a few columns (to avoid the .x & .y  duplicated columns)
  area_extract <- area_extract[,c( "rec_id", draintype, "closest_lu_year")]
  area_extract[,"area_extr"] <- area_extract[,draintype]
  area_extract[,draintype] <- NULL
  area_extract["type"] <- draintype
  
  
  # artdrain_type_nat@data$area_extr <- artdrain_type_nat@data[, draintype]
  # artdrain_type_nat@data[,draintype] <- NULL 
  # artdrain_type_nat["type"] <- draintype 
  
  # exclude all useless columns from Shapefile
  cols_to_keep <- names(artdrain_type_nat)[c(1,66:76)]
  artdrain_type_nat@data <- artdrain_type_nat@data[,cols_to_keep]
  
  
  
  # merge new columns to polygons
  artdrain_type_nat <- merge(artdrain_type_nat, area_extract, by="rec_id")
  
  #============================================================================#
  ### CALCULATE PERCENTAGE                      --------------------------------
  #============================================================================#
  
  # calculate percentage of cropla drained
  artdrain_type_nat$f_drained <- (artdrain_type_nat$drained_area_tot) / artdrain_type_nat@data[,"area_extr"]
  
  # impose ceiling of value=1 to countries where % is above it
  artdrain_type_nat[ artdrain_type_nat$f_drained>1, "f_drained"] <- 1
  
  
  
  #============================================================================#
  ###  POLY MAP THE STATS                      ---------------------------------
  #============================================================================#
  
  # makes a country polygon map of the % drained
  source('./scripts/plots/artif_drainage/map_percent_drained.r')
  
  # return dataframe with percentatges
  return(as.data.frame(artdrain_type_nat@data))
  
}

