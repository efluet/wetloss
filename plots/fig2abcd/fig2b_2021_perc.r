
# /----------------------------------------------------------------------------#
#/ Get land area df 
landarea <- area(preswet) # 1-3
landarea_df <- raster2df(landarea)
names(landarea_df) <- c('landarea', 'x', 'y')

landarea_df <- left_join(maxlncr_df, landarea_df, by=c('x','y')) %>% dplyr::select(landarea)
# landarea_df <- landarea_df[,'landarea']


# /----------------------------------------------------------------------------#
#/  Get remwet & calculate perc loss

f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '_v1.csv')
# Cacluate % loss per pixel
grid_remwet_peryear <- 
        read.csv(f) %>% 
        mutate(cumloss_perc = (X1700 - X2020)/X1700*100) %>%  
        mutate(cumloss_perc = ifelse(cumloss_perc>100, 100, cumloss_perc),
               cumloss_perc = ifelse(cumloss_perc<0, 0, cumloss_perc))

# Append landmask and area to the wetloss% grid
grid_remwet_peryear <- bind_cols(grid_remwet_peryear, ciso_df)
grid_remwet_peryear <- bind_cols(grid_remwet_peryear, landarea_df)  

# Calculate wetland as % of gridcell 
grid_remwet_peryear <- 
            grid_remwet_peryear %>% 
            mutate(Fwet1700 = X1700 / landarea)  

# Make raster from df
r <- rasterFromXYZ(as.data.frame(grid_remwet_peryear)[, c('x', 'y', c('cumloss_perc', 'Fwet1700'))])

# Filter wetloss grid to mask uplands
grid_remwet_perc_robin_df <-  WGSraster2dfROBIN(r) %>% 
                              # Percentage loss above a certain %
                              filter(cumloss_perc > map_cumullossperc_floor) %>% 
                              # Where pixels had originally >5% wetland
                              filter(Fwet1700 * 100 > map_Fwet1700_floor)


# /------------------------------------------------------------------
#/  Get preswet - PERCENTAGE!! - for filtering
preswet <- preswet_max_stack[[p_i]]
preswet <- preswet / area(preswet) * 100
preswet_df <- WGSraster2dfROBIN(preswet)
### THIS IS THE NEW FIX TO PREVENT CORNERS IN ROBINSON PROJ - MAY 2021
names(preswet_df) <- c('x','y','Fpreswet')
preswet_df <- preswet_df %>% filter(Fpreswet >= map_Fwet1700_floor_forwetmap)



# /----------------------------------------------------------------------------#
#/    FIG 1-B :  MAP AREA LOST / Converted
library(ggnewscale)

fig2b <-
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  geom_raster(data=preswet_df, aes(x=x, y=y, fill=Fpreswet)) +

  scale_fill_gradient(low='#99ccff', high='#003d99',
                      breaks=c(1, 25, 50, 75, 100),
                      limits=c(0, 100)) +

  new_scale_fill() +

  # Add wetloss raster
  geom_raster(data=grid_remwet_perc_robin_df, aes(x=x, y=y, fill=cumloss_perc)) + 

  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +

  coord_equal() +  theme_raster_map() +

  # scale_y_continuous(limits=c(-6600000, 8953595)) +
  # '#fff385'
  scale_fill_gradient(low='#ffd11a', high='#e60000',
                      breaks=c(0, 25, 50, 75, 100),
                    limits=c(0, 100)) +
  #
  guides(fill = guide_colorbar(nbin=4, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland percentage lost\n(% of area in 1700)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig2b


# /----------------------------------------------------------------------------#
#/ Make separate maps of loss, preswet, wet1700 for SI
if(0){  source('plots/fig2abcd/fig2b_separate_forsi.R') }



# /----------------------------------------------------------------------------#
#/  For legend
if(1){
  
  legendplot <- 
    ggplot() +
    
    # Add high wetland regions
    geom_tile(data=preswet_df, aes(x=x, y=y, fill=Fpreswet)) +
    scale_fill_gradient(low='#99ccff', high='#003d99',
                        breaks=c(1, 25, 50, 75, 100),
                        limits=c(0, 100)) +
    
    guides(fill = guide_colorbar(nbin=4, raster=F,
                                 barheight = 0.4, barwidth=7,
                                 # reverse=T,
                                 frame.colour=c('black'), frame.linewidth=0.7,
                                 ticks.colour='black',  direction='horizontal', 
                                 title = expression(paste('Present-day wetland\nfraction (% of cell)')))) +
    theme_raster_map() +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal')
  
  
  
  ggsave('../output/figures/fig2/v7/fig2b_wetarea_scale_v7.pdf',
         legendplot,
         width=190, height=130, dpi=600, units='mm' )
  # dev.off()

}

