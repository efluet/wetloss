# fig 2.   overall map

# get mapping theme
source('./scripts/r/plots/themes/map_theme.r')
# get libraries
source('./scripts/r/plots/map_libraries.r')




remwet_Mkm2_stack <- readRDS('./output/results/remwet_Mkm2_stack_wetchimp.rds')




names(remwet_Mkm2_stack)
names(temp_rast)


# subset stack of rasters
remwet_rast_1700 <- raster::subset(remwet_Mkm2_stack, grep('1700', names(remwet_Mkm2_stack), value = T))
remwet_rast_1800 <- raster::subset(remwet_Mkm2_stack, grep('1800', names(remwet_Mkm2_stack), value = T))
remwet_rast_1900 <- raster::subset(remwet_Mkm2_stack, grep('1900', names(remwet_Mkm2_stack), value = T))
remwet_rast_2000 <- raster::subset(remwet_Mkm2_stack, grep('2000', names(remwet_Mkm2_stack), value = T))

remwet_rast_1700_mean = mean(remwet_rast_1700)
remwet_rast_1800_mean = mean(remwet_rast_1800)
remwet_rast_1900_mean = mean(remwet_rast_1900)
remwet_rast_2000_mean = mean(remwet_rast_2000)

# delete
rm(remwet_rast_1700, remwet_rast_1800, remwet_rast_1900, remwet_rast_2000)

wetlossrate_1700_to_1800 <- (remwet_rast_1700_mean - remwet_rast_1800_mean) / 100
wetlossrate_1800_to_1900 <- (remwet_rast_1800_mean - remwet_rast_1900_mean) / 100
wetlossrate_1900_to_2000 <- (remwet_rast_1900_mean - remwet_rast_2000_mean) / 100

# delete
rm(remwet_rast_1700_mean, remwet_rast_1800_mean, remwet_rast_1900_mean, remwet_rast_2000_mean)


wetlossrate_1700_to_1800_robin <- projectRaster(wetlossrate_1700_to_1800, crs=CRS("+proj=robin"))
wetlossrate_1800_to_1900_robin <- projectRaster(wetlossrate_1800_to_1900, crs=CRS("+proj=robin"))
wetlossrate_1900_to_2000_robin <- projectRaster(wetlossrate_1900_to_2000, crs=CRS("+proj=robin"))


# delete
rm(wetlossrate_1700_to_1800, wetlossrate_1800_to_1900, wetlossrate_1900_to_2000)



wetlossrate_1700_to_1800_robin <- as(wetlossrate_1700_to_1800_robin, "SpatialPixelsDataFrame")
wetlossrate_1700_to_1800_robin <- as.data.frame(wetlossrate_1700_to_1800_robin)


wetlossrate_1700_to_1800_robin <- wetlossrate_1700_to_1800_robin %>%
                                  filter(layer > 0)


### get country shpfiles =======================================================
# read and reproject countries  -  and ticks to  Robinson 
natearth_dir <- "../chap5_global_inland_fish_catch/data/gis/nat_earth"
countries <- readOGR(natearth_dir, "ne_110m_admin_0_countries")
countries_df <- fortify(countries)
countries_robin <- spTransform(countries, CRS("+proj=robin"))
countries_robin_df <- fortify(countries_robin)


# read and reproject outside box
bbox <- readOGR(natearth_dir, "ne_110m_wgs84_bounding_box") 
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)



# MAP HISTORICAL CASES COUNTRIES
ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +

  # add raster  
  geom_tile(data=wetlossrate_1700_to_1800_robin, aes(x=x, y=y, fill=layer)) +
  
  # add outline of background countries
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_gradient(low = "#99e699", high = "#cc0000") +
  theme(legend.position="top") +
  theme(plot.margin = unit(c(-2,-1,-2,-1), "mm"))





### Save figure to file --------------------------------------------------------
ggsave('./output/figures/map_wetlossrate_1700_to_1800_robin.png',
       width=178, height=120, dpi=600, units="mm", type = "cairo-png")
dev.off()
