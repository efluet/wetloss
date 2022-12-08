# Apply floor values for maps; tuned for s4p1
map_cumullossperc_floor = 1 #0.05 #1
map_Fwet1700_floor = 1 #0.05  # b is 1
map_Fwet1700_floor_forwetmap = 10 #10


# set parameters
pars <- 'avg'




#  Names of ensemble members
preswet_names <- c('WAD2M', 'GLWD', 'GIEMS2')
simwet_names <- c('ORCHIDEE', 'SDGVM', 'DLEM', 'LPJ-wsl')


# /----------------------------------------------------------------------------#
#/  Get land area df 

landarea <- area(preswet) # 1-3
landarea_df <- raster2df(landarea)
names(landarea_df) <- c('landarea', 'x', 'y')

landarea_df <- left_join(maxlncr_df, landarea_df, by=c('x','y')) %>% dplyr::select(landarea)
# landarea_df <- landarea_df[,'landarea']


# /----------------------------------------------------
#/   Compile losses from 12 ensembles


total_drain_peryear_robin_df_all <- data.frame()
grid_remwet_perc_robin_df_comb <- data.frame()

# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste(s_i, p_i))
    
    f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '_v1.csv')
    
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
      filter(Fwet1700 * 100 > map_Fwet1700_floor)  %>%
      # dplyr::select(X2020) %>% 
      mutate(preswet = preswet_names[p_i]) %>% 
      mutate(simwet = simwet_names[s_i]) 
    
    
    grid_remwet_perc_robin_df_comb <-  bind_rows(grid_remwet_perc_robin_df_comb, grid_remwet_perc_robin_df) 
    
  }
}
    


# /----------------------------------------------------------------------------#
#/    FIG 1-B :  MAP AREA LOST / Converted

fig2b_loss_facet <-
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add wetloss raster
  geom_raster(data=grid_remwet_perc_robin_df_comb, aes(x=x, y=y, fill=cumloss_perc)) +

  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +

  coord_equal() +  theme_raster_map() +

  scale_fill_gradient(low='#ffd11a', high='#e60000',
                      breaks=c(map_cumullossperc_floor, 25, 50, 75, 100),
                      limits=c(map_cumullossperc_floor, 100)) +
  #
  guides(fill = guide_colorbar(nbin=4, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Wetland percentage lost\n(% of area in 1700)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        strip.text = element_text(face = "bold"),
        strip.background = element_blank()) +
  facet_grid(simwet ~ preswet)

# fig2b_loss_facet


# /----------------------------------------------------------------------------#
#/
ggsave('../output/figures/wetloss_ensemble_panel_si.pdf',
       fig2b_loss_facet,
       width=190, height=120, dpi=600, units='mm' )


ggsave('../output/figures/wetloss_ensemble_panel_si.png',
       fig2b_loss_facet,
       width=190, height=120, dpi=600, units='mm' )

dev.off()



# new_scale_fill() +
# 

# # scale_y_continuous(limits=c(-6600000, 8953595)) +
# # '#fff385'
# scale_fill_gradient(low='#ffd11a', high='#e60000',
#                     breaks=c(0, 25, 50, 75, 100),
#                     limits=c(0, 100)) +
#

# Append landmask and area to the wetloss% grid
# grid_remwet_peryear <- bind_cols(grid_remwet_peryear, ciso_df)
# grid_remwet_peryear <- bind_cols(grid_remwet_peryear, landarea_df)  

