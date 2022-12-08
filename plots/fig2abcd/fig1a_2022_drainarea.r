
# /----------------------------------------------------------------------------#
#/    FIG 1a

fig2b_onlyloss <-
  
  ggplot()+
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add wetloss raster
  geom_raster(data=grid_remwet_perc_robin_df, aes(x=x, y=y, fill=cumloss_perc)) + 
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  # scale_y_continuous(limits=c(-6600000, 8953595)) +
  # '#fff385'
  scale_fill_gradient(low='#ffd11a', high='#e60000',
                      breaks=c(1, 25, 50, 75, 100),
                      limits=c(1, 100)) +
  #
  guides(fill = guide_colorbar(nbin=10, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Percentage of wetland loss\n (% of wetland area in 1700)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig2b_onlyloss


