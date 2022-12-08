# Description: 


# create record IDs
drained_peatland <-   drained %>% dplyr::filter(type == "peatland")


#==============================================================================#
### Make shapefile of nat drainage stats ---------------------------------------
#==============================================================================#

# extract cropland area for year and country from HYDE to calculate % drained

# get function that gets polygons for each row
source("./scripts/r/data_proc/fcn/fcn_get_polygons_for_drainage.r")

# create a shapefile with a polygon for each record row
# (some repeat because data from multiple years)
artdrain_peatland_natpoly <- sel_countries_in_shp(drained_peatland, countries, "iso_a3", "adm0_a3", "rec_id")


# join the data table to shapefile, using rec_id as key
artdrain_peatland_natpoly@data <- merge(artdrain_peatland_natpoly@data, as.data.frame(drained), 
                                        by="rec_id", all.x=T, all.y=F)


#==============================================================================#
### SAVE OUTPUT POLYGONS      --------------------------------------------------
#==============================================================================#

# save the filtered table...  is this necessary?
#saveRDS(drained, "./output/results/artif_drainage/drained_ag_stats.rds")

# save the selected shapefile as rds
saveRDS(artdrain_peatland_natpoly, "./output/results/artif_drainage/artdrain_peatland_natpoly.rds")


# delete objects --------------------------------------------
rm(histcases, histcases_nat, histcases_subnat, countries_shp, 
   subnat_shp, h_c_shp, h_s_shp, histcases_shp)

