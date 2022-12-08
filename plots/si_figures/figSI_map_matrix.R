# /----------------------------------------------------------------------------#
#/  Get drainage grid
f <- paste0('../output/results/wetloss/grid/grid_drain/grid_drain_s', s_i, '_p', p_i, '_t', t, '.csv')
grid_drain_peryear <- read.csv(f)


# Make raster from df
r <- rasterFromXYZ(as.data.frame(grid_drain_peryear)[, c('x', 'y', 'X2000')])

total_drain_peryear_robin_df <- 
              WGSraster2dfROBIN(r) %>% 
              filter(X2000 > 10) %>% 
              mutate(X2000 = ifelse(X2000>1000, 1000, X2000))


# /------------------------------------------------------------------
#/  Get preswet
preswet <- preswet_stack[[p_i]]  # 1-3
preswet_df <- raster2df(preswet)
names(preswet_df) <- c('preswet_df', 'x', 'y')
r <- rasterFromXYZ(as.data.frame(preswet_df)[, c('x', 'y', 'preswet_df')])

preswet_df <- WGSraster2dfROBIN(r) 
              filter(preswet_df > 250) %>%
              mutate(preswet_df = ifelse(preswet_df>1000, 1000, preswet_df))


# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted

library(ggnewscale)

fig <- 
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  geom_tile(data=preswet_df, aes(x=x, y=y, fill=preswet_df)) +
  
  scale_fill_gradient(low='#99eaff', high='#0091b8',
                      breaks=c(10^0, 10^1, 10^2, 10^3),
                      labels=c(10^0, 10^1, 10^2, 10^3),
                      limits=c(250, 10^3)) +
  new_scale_fill() +
  
  # Add wetloss raster
  geom_tile(data=total_drain_peryear_robin_df, aes(x=x, y=y, fill=X2000))  +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
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

fig
