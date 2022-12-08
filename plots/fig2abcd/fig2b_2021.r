library(rnaturalearth)
library(rworldmap)
library(rgeos)
library(ggrepel)
# set global extent to map ( excludes Antarctica)
# com_ext <- extent(-180, 180,  -60, 88)



    
# /----------------------------------------------------------------------------#
#/  Get drainage data


# f <- paste0('../output/results/wetloss/grid/grid_drain_s', s_i, '_p', p_i, '.csv')
f <- paste0('../output/results/wetloss/grid/grid_remwet_s', s_i, '_p', p_i, '_', test_theta, '.csv')
grid_drain_peryear <- read.csv(f) %>% dplyr::select(X2020)
grid_drain_peryear <- cbind(grid_drain_peryear, maxlncr_df_xy)

# Make raster from df
r <- rasterFromXYZ(as.data.frame(grid_drain_peryear)[, c('x', 'y', 'X2020')])

total_drain_peryear_robin_df <- WGSraster2dfROBIN(r) #  %>%
#   filter(X2020 > 10) %>% 
#   mutate(X2020 = ifelse(X2020>1000, 1000, X2020))




# /------------------------------------------------------------------
#/  Get preswet
preswet <- preswet_stack[[p_i]]  # 1-3
preswet_df <- raster2df(preswet)
names(preswet_df) <- c('preswet_df', 'x', 'y')
r <- rasterFromXYZ(as.data.frame(preswet_df)[, c('x', 'y', 'preswet_df')])

preswet_df <- WGSraster2dfROBIN(r) 
# Convert preswet to grid df
# preswet_df <- left_join(maxlncr_df_xy, raster2df(preswet), by=c('x','y'))
# Select column w data

preswet_df = preswet_df %>%
  filter(preswet_df > 250) %>%
  mutate(preswet_df = ifelse(preswet_df>1000, 1000, preswet_df))



grid_perc_wetloss <- left_join(total_drain_peryear_robin_df, preswet_df, by=c('x','y'))
glimpse(grid_perc_wetloss)

# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP PERC LOST / Converted

library(ggnewscale)

fig2b <- 
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  # geom_tile(data=pres_wet_f_robin, aes(x=x, y=y), fill='#80bcff') +
  geom_tile(data=preswet_df, aes(x=x, y=y, fill=preswet_df)) +
  
  scale_fill_gradient(low='#99eaff', high='#0091b8',
                      # trans='log',
                      # breaks=c(10^0, 10^1, 10^2, 10^3),
                      # labels=c(10^0, 10^1, 10^2, 10^3),
                      limits=c(0, 10^2)) +
  new_scale_fill() +
  
  # Add wetloss raster
  geom_tile(data=total_drain_peryear_robin_df, aes(x=x, y=y, fill=X2020))  +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  # scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  scale_fill_gradient(low='#fff385', high='#e60000', 
                      trans='log', 
                      # breaks=c(10^0, 10^1, 10^2, 10^3),
                      # labels=c(10^0, 10^1, 10^2, 10^3),
                      limits=c(0, 10^2)) +
  
  guides(fill = guide_colorbar(nbin=10, raster=F, 
                               barheight = 0.7, barwidth=10, 
                               frame.colour=c('black'), frame.linewidth=0.7, 
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland area\ndrained or converted (km'^2*')')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig2b


# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted
# 
# library(ggnewscale)
# 
# fig2b <- 
#   
#   ggplot()+
#   
#   # countries background & outline
#   geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
#   
#   # Coastline
#   geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
#   
#   # Add high wetland regions
#   # geom_tile(data=pres_wet_f_robin, aes(x=x, y=y), fill='#80bcff') +
#   geom_tile(data=preswet_df, aes(x=x, y=y, fill=preswet_df)) +
#   
#   scale_fill_gradient(low='#99eaff', high='#0091b8',
#                       # trans='log',
#                       breaks=c(10^0, 10^1, 10^2, 10^3),
#                       labels=c(10^0, 10^1, 10^2, 10^3),
#                       limits=c(250, 10^3)) +
#   new_scale_fill() +
#   
#   # Add wetloss raster
#   geom_tile(data=total_drain_peryear_robin_df, aes(x=x, y=y, fill=X2020))  +
#   
#   # Add outline bounding box
#   geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
#   
#   coord_equal() +  theme_raster_map() +
#   
#   # scale_y_continuous(limits=c(-6600000, 8953595)) +
#   
#   scale_fill_gradient(low='#fff385', high='#e60000', 
#                       trans='log', 
#                       breaks=c(10^0, 10^1, 10^2, 10^3),
#                       labels=c(10^0, 10^1, 10^2, 10^3),
#                       limits=c(10^1, 10^3)) +
#   
#   guides(fill = guide_colorbar(nbin=10, raster=F, 
#                                barheight = 0.7, barwidth=10, 
#                                frame.colour=c('black'), frame.linewidth=0.7, 
#                                ticks.colour='black',  direction='horizontal',
#                                title = expression(paste('Wetland area\ndrained or converted (km'^2*')')))) +
#   
#   theme(legend.position = 'bottom',
#         legend.direction = 'horizontal')
# 
# fig2b



