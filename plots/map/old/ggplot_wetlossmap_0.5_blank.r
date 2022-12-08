library(gridExtra)
library(raster) 
library(rasterVis) 
library(latticeExtra) 
library(grid) 

# rename years
if(yr<0){
  yr_label <- paste0(yr*-1, 'BC')
} else {
  yr_label <- paste0(yr, 'AD')
}




# wetloss in 1700 as natwet
natwet_0.5 <- wetloss_mean_stack0.5[[1]]
wet <- as(natwet_0.5, "SpatialPixelsDataFrame")
wet <- as.data.frame(wet)
names(wet) <- c("layer","x","y")


blank_tile = wet



# make adjoining map ===========================================================
# map wetland  
wet_plt <- ggplot() +
  
  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +
  # add background country polygons
  #geom_polygon(data=countries_df, aes(long, lat, group=group), fill='grey90') +
  
  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='Blues', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))


# plot cropland ================================================================
cropland_plt <- ggplot() +
  
  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +
  # add background country polygons
  # geom_polygon(data=countries_df, aes(long, lat, group=group), fill='grey90') +

  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='YlGn', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))


# plot wetloss  ===============================================================

wetloss_plt <- ggplot() +
  
  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +

  # add background country polygons
  # geom_polygon(data=countries_df, aes(long, lat, group=group), fill='grey90') +
  
  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='OrRd', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))



# combine as a grid
post_1700_maps <- arrangeGrob(wet_plt, cropland_plt, wetloss_plt, nrow=1)

rm(wet_plt, cropland_plt, wetloss_plt)