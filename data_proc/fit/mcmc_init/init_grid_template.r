# /----------------------------------------------------------------------------#
#/   Get pixel area grid (from HYDE- excl. open water) 
#   AND GRID TEMPLATE

# map with maximum landarea available per gridcell in km2
maxlncr <- raster('../data/lucc/hyde32_beta/general_files/maxln_cr.asc', 
                  crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

origin(maxlncr) <- c(0, 0)
maxlncr <- extend(maxlncr, extent(-180, 180, -90, 90))
maxlncr <- aggregate(maxlncr, fact=6, fun='sum')
maxlncr_df <- data.frame(as(maxlncr, 'SpatialPointsDataFrame'))[1:3]
maxlncr_df$x = round(maxlncr_df$x, 2)
maxlncr_df$y = round(maxlncr_df$y, 2)

landarea = maxlncr_df$maxln_cr

maxlncr_df_xy <- maxlncr_df[2:3]  # Get coordinates to use as common mask


# Cropt to extent of outputs
maxlncr_crop <- crop(maxlncr, extent(-180, 180, -56, 84))
