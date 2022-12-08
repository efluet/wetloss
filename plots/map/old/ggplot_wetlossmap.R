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



# plot nat wet =================================================================

# #  percentage wetland
wet <- as(wet, "SpatialPixelsDataFrame")
wet <- as.data.frame(wet)

# glacier
glacier <- as(g, "SpatialPixelsDataFrame")
glacier <- as.data.frame(glacier)

# map wetland  
wet_plt <- ggplot() +
  geom_tile(data=glacier, aes(x=x, y=y), fill='grey80') +
  # add background mask
  geom_tile(data=wet, aes(x=x, y=y, fill=layer)) +
  coord_equal() +
  scale_fill_distiller(palette='Blues', direction=1, limits=c(0,1)) +
  gif_map_theme()


# plot cropland ================================================================

# cropland fraction
c_f <- as(c/a, "SpatialPixelsDataFrame")
c_f <- as.data.frame(c_f)
c_f2 <- c_f[c_f$layer > 0,]

cropland_plt <- ggplot() +
                geom_tile(data=glacier, aes(x=x, y=y), fill='grey80') +
                # add background mask
                geom_tile(data=c_f2, aes(x=x, y=y, fill=layer)) +
                
                coord_equal() +
                scale_fill_distiller(palette='YlGn', direction=1, limits=c(0,1)) +
                gif_map_theme()



# plot wetloss  ================================================================

# cropland fraction
wetloss <- as(wetloss_f, "SpatialPixelsDataFrame")
wetloss <- as.data.frame(wetloss)
wetloss <- wetloss[wetloss$layer > 0,]


wetloss_plt <- ggplot() +
                geom_tile(data=glacier, aes(x=x, y=y), fill='grey80') +
                # add background mask
                geom_tile(data=wetloss, aes(x=x, y=y, fill=layer)) +
                
                coord_equal() +
                scale_fill_distiller(palette='OrRd', direction=1, limits=c(0,1)) +
                gif_map_theme()




# arrange into a grid ==========================================================
#grid.arrange(wet_plt, cropland_plt, wetloss_plt, timeline, ncol=1, heights = c(1,1,1,0.1))
# y <- arrangeGrob(p1, p2, ncol = 1)
# grid.draw(y)

library(gridExtra)
pre_1700_maps <- arrangeGrob(wet_plt, cropland_plt, wetloss_plt, nrow=1)

