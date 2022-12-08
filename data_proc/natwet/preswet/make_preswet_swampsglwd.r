# make preswet from SWAMPS-GLWD

# /----------------------------------------------------------------------------#
#/ Get 0.5deg grid from SWAMPS-GLWD netcdf
f <- "../../upch4/data/swampsglwd/v2/gcp-ch4_wetlands_2000-2017_05deg.nc"
Fw <- brick(f, varname="Fw")

# filter to 2000-2010 average
# Fw <- Fw[[73 : 132]]
Fw <- Fw[[1 : 132]]

# Get mean of raster stack
Fw_max <- calc(Fw, fun = max, na.rm = T)
Aw_max = Fw_max * area(Fw_max)


# Save raster
writeRaster(Aw_max, 
            filename='../output/results/natwet/grid/swampsglwd_preswet.tif', 
            format="GTiff", overwrite=TRUE)






# # Convert to dataframe
# Fwdf <- as(Fw,'SpatialPointsDataFrame')
# # Convert to sf points
# pts_05deg <- st_as_sf(Fwdf)
# # Add ID column; one point per row 
# pts_05deg$pt_id <- as.numeric(rownames(pts_05deg))
# 
# # Clean up environment
# rm(Fw, f)
