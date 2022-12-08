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
drainage <- read.csv("./output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb.csv") %>% 
  # add country codes
  mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F)) %>% 
  dplyr::select(-c(X.1, X)) %>% 
  unique()


# REMOVE DUPLICATE!! THIS HSOULD HAPPEN UPSTREAM OF THIS 
drainage <- drainage[!duplicated(drainage[,c('year','country_name','type')]),]


#.. Read potential wetland GeoTiff       -----
potwet <- raster("./output/results/potwet/potwet.tif")
potwet[potwet<0] <- 0   # exclude negative pixel values


# Get grid of country isocodes
source("./scripts/data_proc/make_grid_isocodes.r")


# Open NCDF of overlayed LU
# looses the names when saved as TIFF; need to pass as list
h <- './output/results/hyde_resampled/overlay_lu_wet_05deg.nc'
overlap_Mk2_stack <- nc_open(h)

# Make year list
year_ls <- seq(1700, 2000, 10)



#.. Function that distributes the national statistics -----
# source("./scripts/data_proc/overlay/fcn_distrib_drainage_stats.r")
source("./scripts/data_proc/overlay/fcn_distrib_drainage_stats_serial.r")


# Make empty df
output_df <- data.frame(lu_type = as.character(),
                        year = as.numeric(),
                        sum_area_km2 = as.numeric())

# Make stack
s_out <- stack()
s_name_ls <- c()

# Create NCDF output
source('./scripts/data_proc/artif_drainage/create_ncdf_output.r')


# Create empty obj
cropland_drain_distrib = NULL
wetcult_drain_distrib = NULL
forest_drain_distrib = NULL
peatland_drain_distrib = NULL



dist_rast_list <- c("cropland_drain_distrib", 
                    "wetcult_drain_distrib",
                    "forest_drain_distrib",
                    "peatland_drain_distrib",
                    "rice_rdm",
                    "pasture_rdm",
                    "urban_rdm")

# /----------------------------------------------------------------------------#
#/   Loop through years
for (y in 1:31){ 
  
  print(paste("year=", year_ls[y]))  # print ticker
  
  # /--------------------------------------------------------------------------#
  #/    Cropland                                                          ------
  cropland_drain_distrib <- distrib_drainage_stats(h, "cropland_wet_rdm", y, "cropland", cropland_drain_distrib)
  
  
  # /--------------------------------------------------------------------------#
  #/    Wetland Cultivation                                               ------
  wetcult_drain_distrib <- distrib_drainage_stats(h, "cropland_wet_rdm", y, "Wetland Cultiv.", wetcult_drain_distrib)
  
  
  # /--------------------------------------------------------------------------#
  #/    Forestry                                                          ------
  forest_drain_distrib <- distrib_drainage_stats(h, "forest_harv_wet_rdm", y, "forestry", forest_drain_distrib)
  
  
  # /--------------------------------------------------------------------------#
  #/    Peatland                                                          ------
  peatland_drain_distrib <- distrib_drainage_stats(h, "peatland_wet_rdm", y, "peatland", peatland_drain_distrib)
  
  
  # /--------------------------------------------------------------------------#
  #/     Rice                                                             ------
  rice_rdm <- raster(h,  var="ir_rice_wet_pref", band=y)
  
  
  # /--------------------------------------------------------------------------#
  #/    Pasture                                                           ------
  pasture_rdm <- raster(h,  var="pasture_wet_avoid", band=y)
  
  
  # /--------------------------------------------------------------------------#
  #/    Urban                                                             ------
  urban_rdm <- raster(h,  var="uopp__wet_pref", band=y)
  
  
  
  # /---------------------------------------------------------------------
  #/  SAVE OUTPUTS!!!!
  
  
  
  # Loop through rasters
  for ( r in dist_rast_list){
    
    # /------------------------------------------------------------------------#
    #/  SAVE SUMS TO DF
    output_df <- rbind( output_df,
                        data.frame(
                          lu_type     = unlist(strsplit(r,"_"))[1],
                          year        = year_ls[y],
                          sum_area_km2 = sum_raster(get(r))))
    
    
    # /------------------------------------------------------------------------#
    #/  SAVE GRIDS IN NCDF
    
    ncvar_put(outncdf,   # the output ncdf
              r,   # the variable string name
              values(flip(get(r), 'y')), #get(v),   # the raster to write
              start=c(1,1,y),     # the index start
              count=c(nx,ny,1))   # the index count/size
    
    s_name_ls <- c(s_name_ls, paste0(r,year_ls[y]))
    
    s_out <- stack(s_out, get(r))
    
  }
}

# close the file, writing data to disk
nc_close(outncdf)



# Save sum CSV file area to file
write.csv(output_df, "./output/results/artif_drainage/drained_wetcult_gridded_sum_serial.csv")


# Save raster stack as RDS
names(s_out) <- s_name_ls
saveRDS(s_out, './output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial.rds')



#.. SAVE Conditions in 2000 (for maps)
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

# get maximum value
sum_loss2000 <- sum(driver_stack, na.rm = TRUE)


writeRaster(sum_loss2000, 
            "./output/results/artif_drainage/sum_loss2000.csv",
            "GTiff",
            options=c("COMPRESS=NONE", "TFW=YES"))



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
