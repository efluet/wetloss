

# # Filter wetloss grid to
# grid_remwet_perc_robin_df <-  WGSraster2dfROBIN(r) %>% 
#   # Percentage loss above a certain %
#   # filter(cumloss_perc > 1) %>% # map_cumullossperc_floor) %>%
#   # Where pixels had originally >5% wetland
#   filter(Fwet1700 * 100 > 5) # map_Fwet1700_floor)
# 


# Get wetland in 1700
# Fwet1700_r <- r[[2]]

# plot(r_wet1700)

# Get preswet
preswet <- preswet_max_stack[[p_i]]
preswet <- preswet / area(preswet) * 100


# Apply extent from raster to preswet raster
crs(Fwet1700_r) <- crs(preswet)
# preswet <-setExtent(preswet, extent(r_wet1700), keepres=TRUE)
preswet <- crop(preswet, extent(Fwet1700_r))


# Stack preswet on raster of ('cumloss_perc', 'Fwet1700'))])
fig1b_r <- stack(Fwet1700_r, preswet)
names(fig1b_r) <- c('cumulloss_perc','Fwet1700', 'preswet')

fig1b_df <- WGSraster2dfROBIN(fig1b_r) %>% 
            mutate(perc_change = Fwet1700*100 - preswet) %>% 
            filter(perc_change >= 1)

# glimpse(fig1b_df)




# /----------------------------------------------------------------------------#
#/    FIG 1-A: BUT ONLY present WETLAND AREA


fig2b_preswetonly_percchange <-
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  # geom_raster(data=preswet_df, aes(x=x, y=y, fill=Fpreswet)) +
  geom_raster(data=fig1b_df, aes(x=x, y=y, fill=perc_change)) +
  
  
  scale_fill_gradient(low='#99ccff', high='#00307A', #'#003d99',
                      breaks=c(1, 25, 50, 75, 100),
                      limits=c(1, 100)) +
  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  #
  guides(fill = guide_colorbar(nbin=10, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland extent decline since 1700\n (% of cell)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig2b_preswetonly_percchange
