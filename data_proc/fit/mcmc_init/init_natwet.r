
# /----------------------------------------------------------------------------#
#/.. Read present day wetland GeoTiff                                      -----

wad2m_preswet <- raster('../output/results/natwet/grid/swampsglwd_preswet.tif')  # / 10^6
wad2m_preswet_df = raster2df(wad2m_preswet)
wad2m_preswet_df <- left_join(maxlncr_df_xy, wad2m_preswet_df, by=c('x','y'))
wad2m_preswet_df <- wad2m_preswet_df$swampsglwd_preswet


# GLWD3 (classes 4-12)
glwd3_wet_frac <- raster( '../output/results/natwet/preswet/glwd3_wet_fw.tif')
crs(glwd3_wet_frac) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
glwd3_wet_a <- glwd3_wet_frac * raster::area(glwd3_wet_frac) 
glwd3_wet_a <- aggregate(glwd3_wet_a, fact=2, fun=sum, na.rm=T)
glwd3_wet_a_df = raster2df(glwd3_wet_a)
glwd3_wet_a_df$x = round(glwd3_wet_a_df$x, 2)
glwd3_wet_a_df$y = round(glwd3_wet_a_df$y, 2)# round coordinates for join
glwd3_wet_a_df <- left_join(maxlncr_df_xy, glwd3_wet_a_df, by=c('x','y'))
glwd3_wet_a_df <- glwd3_wet_a_df$layer


# GIEMS v2  (minus GLWD 1-3)
giems2_wet <- raster('../output/results/natwet/preswet/old/giems2_max_wet.tif')
giems2_wet <- aggregate(giems2_wet, fact=2, fun=sum)  # aggregate to 0.5deg
giems2_wet_df = raster2df(giems2_wet)  # Convert to df
giems2_wet_df <- left_join(maxlncr_df_xy, giems2_wet_df, by=c('x','y'))
giems2_wet_df <- giems2_wet_df$giems2_max_wet


# Get global sums
# cellStats(wad2m_preswet, stat='sum', na.rm=T)/10^6
# cellStats(glwd3_wet_a, stat='sum', na.rm=T)/10^6
# cellStats(giems2_wet, stat='sum', na.rm=T)/10^6

# pres_wet <- raster('../output/results/potwet/pres_wet.tif')  / 10^6