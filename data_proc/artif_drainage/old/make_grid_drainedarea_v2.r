# /----------------------------------------------------------------------------#
#/    Get fcn extracting drain types                                   --------- 
#     Different drain types are run separately because they rely on different raster sources
#     function:  process_drainage_stats

# Does order of intersect matter?  The order should exhaust the available wetland area in the overlay too.
# Rice > Cropland  > Pasture > Ag
# Using Random scenario to distribute- since it's middle of the road? Or Preference? (It shouldn't matter much in the distribution...)

# Inputs:
#   - Potential wetland grid (1 layer)
#   - Stack of wetloss area per driver (1700-2000), subsetted to random 

# /----------------------------------------------------------------------------#
#/   Get function calculating % drained
#source('./scripts/data_proc/artif_drainage/fcn/fcn_calc_percent_artif_drainage_pertype_v2.r')


# /----------------------------------------------------------------------------#
#/   Read interpolated drainage area table
# drainage <- read.csv("./output/results/artif_drainage/drained_wetcult_ha_sigmoidpred.csv")
drainage <- read.csv("./output/results/artif_drainage/drained_wetcult.csv") %>%
  # add country codes
  mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F))


#.. Read potential wetland GeoTiff       -----
potwet <- raster("./output/results/potwet/potwet.tif")
potwet[potwet<0] <- 0   # exclude negative pixel values


# Get grid of country isocodes
source("./scripts/data_proc/make_grid_isocodes.r")


# Open NCDF of overlayed LU
# looses the names when saved as TIFF; need to pass as list
h <- './output/results/hyde_resampled/overlay_lu_wet_05deg.nc'
overlap_Mk2_stack <- nc_open(h)


#.. Function that distributes the national statistics -----
source("./scripts/data_proc/overlay/fcn_distrib_drainage_stats.r")


# Make empty df
output_df <- data.frame(lu_type = as.character(),
                        year = as.numeric(),
                        sum_area_km2 = as.numeric())

# Make year list
year_ls <- seq(1700, 2000, 10)


# /----------------------------------------------------------------------------#
#/   Loop through years
for (y in 1:31){
  
  print(paste("year=", year_ls[y]))  # print ticker
  
  # /--------------------------------------------------------------------------#
  #/    Cropland                                                           -----
  cropland_drain_distrib <- distrib_drainage_stats(h, "cropland_wet_rdm", y, "cropland")

  
  # /--------------------------------------------------------------------------#
  #/    Wetland Cultivation                                                -----
  wetcult_drain_distrib <- distrib_drainage_stats(h, "cropland_wet_rdm", y, "Wetland Cultiv.")
   
  
  # /--------------------------------------------------------------------------#
  #/    Forestry                                                           -----
  forest_drain_distrib <- distrib_drainage_stats(h, "forest_harv_wet_rdm", y, "forestry")
  
  
  # /--------------------------------------------------------------------------#
  #/    Peatland                                                           -----
  peatland_drain_distrib <- distrib_drainage_stats(h, "peatland_wet_rdm", y, "peatland")
  
  
  # /----------------------------------------------------------------------------#
  #/     Rice 
  rice_rdm <- raster(h,  var="ir_rice_wet_pref", band=y)
  
  
  # /----------------------------------------------------------------------------#
  #/    Pasture 
  pasture_rdm <- raster(h,  var="pasture_wet_avoid", band=y)
  
  
  # /----------------------------------------------------------------------------#
  #/    Urban
  urban_rdm <- raster(h,  var="uopp__wet_pref", band=y)
  
  
  
  
  #.. SAVE SUMS      ------
  dist_rast_list <- c("cropland_drain_distrib", 
                      "wetcult_drain_distrib",
                      "forest_drain_distrib",
                      "peatland_drain_distrib",
                      "rice_rdm",
                      "pasture_rdm",
                      "urban_rdm")
  
  
  for ( r in dist_rast_list){
    # Save sums to dataframe
    output_df <- rbind( output_df,
                        data.frame(
                          lu_type     = unlist(strsplit(r,"_"))[1],
                          year        = year_ls[y],
                          sum_area_km2 = sum_raster(get(r))))

    ###  SAVE GRIDS IN NCDF
    
  }
  
}


  
# Save sum area to file
write.csv(output_df, "./output/results/artif_drainage/drained_wetcult_gridded_sum.csv")


  
# Stack 
driver_stack <- stack(cropland_drain_distrib, 
                      wetcult_drain_distrib,
                      forest_drain_distrib,
                      peatland_drain_distrib,
                      rice_rdm,
                      pasture_rdm,
                      urban_rdm)

names(driver_stack) <- c("Cropland",
                         "Wet.Cultiv.",
                         "Forestry",
                         "Peatland Extr.", 
                         "Irrig.Rice", 
                         "Pasture", 
                         "Urban")

sum_loss2000 <- sum(driver_stack, na.rm = TRUE)

maxdriver <- which.max(driver_stack)





# Output design: Save as multivariable NCDF
# Time dim; 1700-2000
# Variables: 
#       - Potential wetland
#       - Cropland (documented area)
#             - Irrigated
#             - Rainfed
#             - Rice
#       - Pastureland(10% drained; Pavelis1987)
#       - Forestry (documented area)
#       – Peat extraction (documented area)
#       – Urban areas (map overlay)





# Pin drained area wherever we have data, then use 
# - Reconstructed drainage expansion based on national drainage stats
#     - Null model: drainage expanded at the same rate as the LU (e.g. cropland, forestry).
#     - Full range of overlap: avoidance, random, preference




# to do list:
#   - test LUH  v2 forestry
#   - add in the subnational data
#   - grid the drainage onto natwetgrid
#   - separate spate irrig
#   - separate irrig vs rainfed 
