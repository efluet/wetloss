# Description: Extracts the agriculture area from HYDE in countries with drainage data 



# get hyde years
hyde_yrs <- readRDS("./output/results/hyde_yrs/hyde_yrs_since1700.rds")

# get  polygon of countries w drainage
drain_poly <- readRDS("./output/results/artif_drainage/artdrain_ag_nat.rds")

### read hyde ncdf at teh 0.5 resolution
h <- './output/results/hyde_resampled/hyde32_0.5.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals)
hyde_indx <- match(hyde_yrs, hyde$dim$time$vals)


#==============================================================================#
### Extract cropland area         ----------------------------------------------
#==============================================================================#

# this could be modified to also extract forest/peatlands... later
source("./scripts/r/data_proc/fcn/get_hydecroparea_in_polygons.r")

# run function  that loops through polygons
area_extract <- get_raster_closest_yr(artdrain_ag_nat)

# select a few columns (to avoid the .x & .y  duplicated columns)
area_extract <- area_extract[,c("rec_id","cropland","closest_hyde_year")]

# merge new columns to polygons
cols_to_keep <- names(drain_poly)[c(1,66:76)]
drain_poly@data <- drain_poly@data[,cols_to_keep]
drain_poly <- merge(drain_poly, area_extract, by="rec_id")


#==============================================================================#
### CALCULATE PERCENTAGE         -----------------------------------------------
#==============================================================================#

# calculate percentage of cropla drained
drain_poly$fraction_drained <-  (drain_poly$drained_area_tot)  /  drain_poly$cropland

drain_poly[drain_poly$fraction_drained>1,"fraction_drained"] <- 1


#==============================================================================#
# Save output ---------------------------------------------------------------
#==============================================================================#

# write out dataframe
saveRDS(area_extract, "./output/results/artif_drainage/artdrain_nat_wcroparea.rds")

# write out dataframe
saveRDS(drain_poly, "./output/results/artif_drainage/artdrain_nat_poly_wcroparea.rds")

# delete variables
rm(hyde_yrs, drain_poly, h, hyde, hyde_indx, area_extract)
