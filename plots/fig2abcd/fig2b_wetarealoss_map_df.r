library(rnaturalearth)
library(rworldmap)
library(rgeos)
library(ggrepel)
# set global extent to map ( excludes Antarctica)
com_ext <- extent(-180, 180,  -60, 88)



# /----------------------------------------------------------------------------#
#/   Make mask of high-wetland region (for map)

# THIS IS THE OBJECT NEEDED FROM  :  cumul_drained_for_fig2c_map


### AS RASTER
r <- rasterFromXYZ(as.data.frame(cumul_drained_for_fig2c_map)[, c('x', 'y', 'X2000')])

crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
cumul_drained_for_fig2c_map_robin <- projectRaster(r, crs=CRS('+proj=robin'), method='ngb', over=TRUE)

cumul_drained_for_fig2c_map_robin <- as(cumul_drained_for_fig2c_map_robin, 'SpatialPixelsDataFrame')
cumul_drained_for_fig2c_map_robin_df <- as.data.frame(cumul_drained_for_fig2c_map_robin)

cumul_drained_for_fig2c_map_robin_df <- cumul_drained_for_fig2c_map_robin_df %>% 
                                        filter(X2000 > 10) %>% 
                                        mutate(X2000 = ifelse(X2000>1000, 1000, X2000))


# /----------------------------------------------------------------------------#
#/    PRESWET FOR BACKGROUND MAP
wad2m_preswet <- raster('../output/results/natwet/grid/swampsglwd_preswet.tif')  # / 10^6
wad2m_preswet_df = raster2df(wad2m_preswet)
wad2m_preswet_df <- left_join(maxlncr_df_xy, wad2m_preswet_df, by=c('x','y'))
# wad2m_preswet_df <- wad2m_preswet_df$swampsglwd_preswet

wad2m_preswet_ras <- rasterFromXYZ(as.data.frame(wad2m_preswet_df)[, c('x', 'y', 'swampsglwd_preswet')])
crs(wad2m_preswet_ras) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
wad2m_preswet_robin <- projectRaster(wad2m_preswet_ras, crs=CRS('+proj=robin'), method='ngb', over=TRUE)

wad2m_preswet_robin_df = raster2df(wad2m_preswet_robin)
wad2m_preswet_robin_df = wad2m_preswet_robin_df %>% 
                          filter(swampsglwd_preswet > 250) %>% 
                          mutate(swampsglwd_preswet = ifelse(swampsglwd_preswet>1000, 1000, swampsglwd_preswet))



# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted

library(ggnewscale)

arealossmap <- 
  ggplot()+
    
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +

  # Add high wetland regions
  # geom_tile(data=pres_wet_f_robin, aes(x=x, y=y), fill='#80bcff') +
  geom_tile(data=wad2m_preswet_robin_df, aes(x=x, y=y, fill=swampsglwd_preswet)) +

  scale_fill_gradient(low='#99eaff', high='#0091b8',
                      # trans='log',
                      breaks=c(10^0, 10^1, 10^2, 10^3),
                      labels=c(10^0, 10^1, 10^2, 10^3),
                      limits=c(250, 10^3)) +
  new_scale_fill() +
  
  # Add wetloss raster
  geom_tile(data=cumul_drained_for_fig2c_map_robin_df, aes(x=x, y=y, fill=X2000))  +
  

  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
    
  
  coord_equal() +  theme_raster_map() +
  
  # scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  scale_fill_gradient(low='#fff385', high='#e60000', 
                      trans='log', 
                      breaks=c(10^0, 10^1, 10^2, 10^3),
                      labels=c(10^0, 10^1, 10^2, 10^3),
                      limits=c(10^1, 10^3)) +

  guides(fill = guide_colorbar(nbin=10, raster=F, 
                               barheight = 0.7, barwidth=10, 
                               frame.colour=c('black'), frame.linewidth=0.7, 
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland area\ndrained or converted (km'^2*')')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

arealossmap














#-----------------------------------------------------------------------------

# pres_wet_f <- pres_wet / pixarea
# cutoff_f = 0.2
# pres_wet_f[pres_wet_f < cutoff_f] <- NA
# pres_wet_f[pres_wet_f >= cutoff_f] <- 1
# plot(pres_wet_f)
# 
# # declare incoming CSR (should be done wayyyyyyy earlier than this)
# crs(pres_wet_f) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
# pres_wet_f_robin <- projectRaster(pres_wet_f, crs=CRS('+proj=robin'), method='ngb', over=TRUE)
# 
# pres_wet_f_robin <- as(pres_wet_f_robin, 'SpatialPixelsDataFrame')
# pres_wet_f_robin <- as.data.frame(pres_wet_f_robin)
# 
# 
# # /----------------------------------------------------------------------------#
# #/  GET 2000 Wetloss
# 
# # drain_Mkm2_stack <- readRDS('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul.rds')
# remwet_Mkm2_stack <- readRDS('../output/results/wetloss/grid/remwet_tot_stack_0.5deg_serialbestvar.rds')
# 
# remwet2000 <- remwet_Mkm2_stack[[grep(pattern='2000', names(remwet_Mkm2_stack))]]
# remwet1700 <- remwet_Mkm2_stack[[grep(pattern='1700', names(remwet_Mkm2_stack))]]
# sum_loss2000 <- remwet1700 - remwet2000




# /----------------------------------------------------------------------------#
#/   reproject the grid to robinson projection


# # declare incoming CSR (should be done wayyyyyyy earlier than this)
# crs(sum_loss2000) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
# sum_loss2000_robin <- projectRaster(sum_loss2000, crs=CRS('+proj=robin'), method='ngb', over=TRUE)


# # /----------------------------------------------------------------------------#
# #/    reformat rasters  for graph in ggplot
# sum_loss2000_robin <- as(sum_loss2000_robin, 'SpatialPixelsDataFrame')
# sum_loss2000_robin <- as.data.frame(sum_loss2000_robin)

# sum_loss2000_robin <- sum_loss2000_robin %>% filter(layer>0)
# 
# ### compress color ramap 
# sum_loss2000_robin <- sum_loss2000_robin %>% mutate(layer = ifelse(layer>1000, 1000, layer))
# sum_loss2000_robin <- sum_loss2000_robin %>% filter(layer>=10)
# 
