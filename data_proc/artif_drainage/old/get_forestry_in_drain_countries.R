# Description: Extracts forestry area from countries with drainage data 
# Read in the wood harvest  reconstruction of Hurtt et al 2006, 2011
# data used is the gridcell fraction of secondary forest harvested 1) young and 2) mature.
# Only secondary forest because primary forest wouldn't be drained.




  
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get  polygon of countries w drainage
artdrain_forest_natpoly <- readRDS("./output/results/artif_drainage/artdrain_forest_natpoly.rds")


wood_harvest <- readRDS("./data/hurtt_wood_harvest/wood_harvest.rds")


### Extract secondary forestry area --------------------------------------------

# this could be modified to also extract forest/peatlands... later
source("./scripts/r/data_proc/fcn/fcn_raster_in_poly_sum_area.r")

# run function  that loops through polygons
year_vector <- c(1500,1600,1700,seq(1800,2000,10))
area_extract <- get_raster_closest_yr(artdrain_forest_natpoly, wood_harvest, year_vector)


# select a few columns (to avoid the .x & .y  duplicated columns)
area_extract <- area_extract[,c("rec_id","area","closest_year")]

# merge new columns to polygons
artdrain_forest_natpoly <- merge(artdrain_forest_natpoly, area_extract, by="rec_id")

# remove the 0 area extracted
artdrain_forest_natpoly <- subset(artdrain_forest_natpoly, area>0)

# calculate percentage of cropla drained
artdrain_forest_natpoly$fraction_drained <-  (artdrain_forest_natpoly$drained_area_tot)  /  artdrain_forest_natpoly$area


#==============================================================================#
# Save output ---------------------------------------------------------------
#==============================================================================#

# write out dataframe
saveRDS(area_extract, "./output/results/artif_drainage/artdrain_nat_forestry.rds")

# write out polygon
saveRDS(artdrain_forest_natpoly, "./output/results/artif_drainage/artdrain_nat_poly_forestry.rds")

# delete variables
rm(hyde_yrs, drain_poly, h, hyde, hyde_indx, area_extract)
