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



# make adjoining map ===========================================================

# #  percentage wetland
wet <- as(natwet_f, "SpatialPixelsDataFrame")
wet <- as.data.frame(wet)
names(wet) <- c("layer","x","y")
wet <- wet[wet$layer > 0,]


# map wetland  
wet_plt <- ggplot() +
  
  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +
  geom_tile(data=wet, aes(x=x, y=y, fill=layer)) +
  
  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='Blues', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))


# plot cropland ================================================================

# cropland fraction
c_f <- as(c_f, "SpatialPixelsDataFrame")
c_f <- as.data.frame(c_f)
c_f <- c_f[c_f$layer > 0,]

cropland_plt <- ggplot() +

  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +
  # add background mask
  geom_tile(data=c_f, aes(x=x, y=y, fill=layer)) +
  
  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='YlGn', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))



# plot wetloss  ===============================================================


# cropland fraction
wetloss <- as(wetloss_f, "SpatialPixelsDataFrame")
wetloss <- as.data.frame(wetloss)
names(wetloss) <- c("layer","x","y")
wetloss <- wetloss[wetloss$layer > 0,]

wetloss_plt <- ggplot() +
  
  # add background mask
  geom_tile(data=blank_tile, aes(x=x, y=y), fill= 'grey90') +
  geom_tile(data=wetloss, aes(x=x, y=y, fill=layer)) +
  
  coord_equal() +
  gif_map_theme() +
  scale_fill_distiller(palette='OrRd', direction=1, limits=c(0,1)) +
  theme(legend.position=c(0.1, 0.4))




#grid.arrange(wet_plt, cropland_plt, wetloss_plt, timeline, ncol=1, heights = c(1,1,1,0.1))

post_1700_maps <- arrangeGrob(wet_plt, cropland_plt, wetloss_plt, nrow=1)

rm(wet_plt, cropland_plt, wetloss_plt)
