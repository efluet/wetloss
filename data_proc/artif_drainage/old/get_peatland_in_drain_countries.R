# get peatland area from Stocker 2014 

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get  polygon of countries w drainage
# save the selected shapefile as rds
artdrain_peatland_natpoly <- readRDS("./output/results/artif_drainage/artdrain_peatland_natpoly.rds")


# get raster of gridcell area, accounting for projection
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")
# aggregate grid to resolution of wetland data
area <- aggregate(area, fact= c(2), fun=sum) 



#==============================================================================#
###        GET THE PEATLAND RASTER     -----------------------------------------
#==============================================================================#


# time dimension is monthly over 112 years (1901-2012)
# variable names are:  "inund", "lu_area", "wtpos"
l <- './data/lpx_dytop_2014/s1_dyptop_output.nc'
lpx <- nc_open(l)

last_idx <- dim(lpx_peat)[3]

# peat doesn't change between month of the same year.
lpx_peat <-brick(l, varname="lu_area", level=2)
lpx_peat <- lpx_peat[[last_idx]]  *  area


#==============================================================================#
### Extract area --------------------------------------------
#==============================================================================#


# this could be modified to also extract forest/peatlands... later
source("./scripts/r/data_proc/fcn/fcn_raster_in_poly_sum_area.r")

# run function  that loops through polygons
#year_vector <- c(1500,1600,1700,seq(1800,2000,10))
area_extract <- get_raster_closest_yr(artdrain_peatland_natpoly, lpx_peat, 2000)

# select a few columns (to avoid the .x & .y  duplicated columns)
area_extract <- area_extract[,c("rec_id","area","closest_year")]

# merge new columns to polygons
artdrain_peatland_natpoly <- merge(artdrain_peatland_natpoly, area_extract, by="rec_id")

# remove the 0 area extracted
artdrain_peatland_natpoly <- subset(artdrain_peatland_natpoly, area>0)

# calculate percentage of cropla drained
artdrain_peatland_natpoly$fraction_drained <-  (artdrain_peatland_natpoly$drained_area_tot)  /  artdrain_peatland_natpoly$area


#==============================================================================#
# Save output ---------------------------------------------------------------
#==============================================================================#

# write out dataframe
saveRDS(area_extract, "./output/results/artif_drainage/artdrain_nat_peatland.rds")

# write out polygon
saveRDS(artdrain_peatland_natpoly, "./output/results/artif_drainage/artdrain_nat_poly_peatland.rds")

# delete variables
#rm(hyde_yrs, drain_poly, h, hyde, hyde_indx, area_extract)
