

# /----------------------------------------------------------------------------#
#/    Read input data

s_i=4; p_i=1; pars='avg'
f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '.csv')


# /----------------------------------------------------------------------------#
#/      Get wetland area
grid_1700_wetarea <- 
  read.csv(f) %>% 
  dplyr::select(X1700, X2020) %>% 
  cbind(., maxlncr_df) %>% 
  mutate(Fwet1700 = X1700 / maxln_cr * 100,
         loss_perc = (X1700-X2020)/X1700 *100)


# Prep grid of max wetloss rate
grid_remwet_peryear <- 
  read.csv(f) %>% 
  dplyr::select(-X) %>% 
  cbind(., maxlncr_df_xy) %>% 
  pivot_longer(cols=X1700:X2020, names_to='year', values_to='remwet_km2') %>% 
  group_by(x, y) %>% 
  # Get biggest decline in remwet per decadal step (min because it is negative value)
  mutate(diff = remwet_km2 - lag(remwet_km2)) %>% 
  filter(diff==min(diff, na.rm=T)) %>% 
  slice_tail(n=1) %>% 
  filter(diff != 0) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(year, 2, 5))) %>% 
  left_join(., grid_1700_wetarea, by=c('x','y')) %>% 
  # Apply mask
  filter(Fwet1700 >= 5, loss_perc >= 5)


glimpse(grid_remwet_peryear)


# Make raster from df
r <- rasterFromXYZ(as.data.frame(grid_remwet_peryear)[, c('x', 'y', c('year'))])
grid_remwet_peryear_robin_df <-  WGSraster2dfROBIN(r)

  
# /----------------------------------------------------------------------------#
#/    Make map of max drainage rate

max_rate_map <- 
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add wetloss raster
  geom_raster(data=grid_remwet_peryear_robin_df, aes(x=x, y=y, fill=year)) + 
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  scale_fill_stepsn(colours = c('maroon','red','#e86413','orange','#fff83d','#14cc00','#0dc8de','#1259ff', '#100387'),
                       breaks=c(1850, 1900, 1925, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                       limits=c(1850, 2020)) +
  #
  guides(fill = guide_colorbar(#nbin=10, raster=F,
                               barheight = 0.4, barwidth=22, 
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Decade of maximum\nwetland loss rate\n(km^2 per year)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


max_rate_map

# /----------------------------------------------------------------------------#
#/ SAVE FILE
ggsave('../output/figures/forsi/max_loss_rate_map_sep2022.png', max_rate_map,
       width=190, height=110, dpi=600, units='mm' )

ggsave('../output/figures/forsi/max_loss_rate_map_sep2022.pdf', max_rate_map,
       width=190, height=110, dpi=600, units='mm' )
dev.off()




# scale_fill_gradientn(colours = c('orange','#fff83d','#14cc00','#0dc8de','#1259ff'),
#                      breaks=c(1800, 1950, 1980, 2000, 2020),
#                      limits=c(1800,2020)) +