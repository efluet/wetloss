
# /----------------------------------------------------------------------------#
#/    FIG 1-A: BUT ONLY 1700 WETLAND AREA


# Filter wetloss grid to
grid_remwet_perc_robin_df <-  WGSraster2dfROBIN(Fwet1700_r) %>% 
  # Percentage loss above a certain %
  # filter(cumloss_perc > 1) %>% # map_cumullossperc_floor) %>%
  # Where pixels had originally >5% wetland
  filter(Fwet1700 * 100 > 5) # map_Fwet1700_floor)



fig2b_1700wet <-
  
  ggplot()+
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add wetloss raster
  geom_raster(data=grid_remwet_perc_robin_df, aes(x=x, y=y, fill=Fwet1700*100)) + 
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  # scale_y_continuous(limits=c(-6600000, 8953595)) +
  # '#fff385'
  scale_fill_gradient(low='#97cf99', high='#183b19',  #'#1e4a20',
                      breaks=c(5, 25, 50, 75, 100),
                      limits=c(5, 100)) +
  #
  guides(fill = guide_colorbar(nbin=10, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland extent in 1700 \n(% of cell)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig2b_1700wet


