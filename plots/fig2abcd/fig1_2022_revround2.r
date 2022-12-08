# Panel A - Line plot of Rem.Wet area %
# Panel B - Map of drain area
# Panel C - Map of present-day wetland 
# Panel D - Map of wetland in 1700
# Panel E - Stack area of 
# Panel F - Map of dominant driver


# /----------------------------------------------------------------------------#
#/  Apply floor values for maps; tuned for s4p1                         --------
map_cumullossperc_floor = 1 #0.05 #1
map_Fwet1700_floor = 5 #0.05  # b is 1
map_Fwet1700_floor_forwetmap = 5 #10



# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste(s_i, p_i))
    
    
    
    # /----------------------------------------------------------------------------#
    #/ Get land area df                                                     --------
    landarea <- area(preswet) # 1-3
    landarea_df <- raster2df(landarea)
    names(landarea_df) <- c('landarea', 'x', 'y')
    
    landarea_df <- left_join(maxlncr_df, landarea_df, by=c('x','y')) %>% dplyr::select(x, y, landarea)
    # landarea_df <- landarea_df[,'landarea']
    
    
    # /----------------------------------------------------------------------------#
    #/  Get remwet & calculate perc loss                                    --------
    
    f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '_v1.csv')
    # Calculate % loss per pixel
    grid_remwet_peryear <- 
      read.csv(f) %>% 
      mutate(cumloss_perc = (X1700 - X2020)/X1700*100) %>%  
      mutate(cumloss_perc = ifelse(cumloss_perc>100, 100, cumloss_perc),
             cumloss_perc = ifelse(cumloss_perc<0, 0, cumloss_perc))
    
    # Append landmask and area to the wetloss% grid
    # grid_remwet_peryear <- bind_cols(grid_remwet_peryear, ciso_df)
    grid_remwet_peryear <- bind_cols(grid_remwet_peryear, landarea_df)  
    
    # Calculate wetland as % of gridcell 
    grid_remwet_peryear <- 
      grid_remwet_peryear %>% 
      mutate(Fwet1700 = X1700 / landarea) %>% 
      mutate(Fwet1700 = ifelse(Fwet1700>1, 1, Fwet1700))
    
    # Make raster from df
    r <- rasterFromXYZ(as.data.frame(grid_remwet_peryear)[, c('x', 'y', c('cumloss_perc', 'Fwet1700'))])
    
    Fwet1700_r <- r  # Reused in preswet panel
    
    # Filter wetloss grid to
    grid_remwet_perc_robin_df <-  WGSraster2dfROBIN(r) %>% 
      # Percentage loss above a certain %
      filter(cumloss_perc > 1) %>% # map_cumullossperc_floor) %>%
      # Where pixels had originally >5% wetland
      filter(Fwet1700 * 100 > map_Fwet1700_floor) # map_Fwet1700_floor)
    
    
    # /----------------------------------------------------------------------------#
    #/  Get preswet - PERCENTAGE!! - for filtering                -------------
    preswet <- preswet_max_stack[[p_i]]
    preswet <- preswet / area(preswet) * 100
    preswet_df <- WGSraster2dfROBIN(preswet)
    ### THIS IS THE NEW FIX TO PREVENT CORNERS IN ROBINSON PROJ - MAY 2021
    names(preswet_df) <- c('x','y','Fpreswet')
    preswet_df <- preswet_df %>% filter(Fpreswet > 5) # map_Fwet1700_floor_forwetmap)
    
    
    
    

    #$$$$$
    total_drain_peryear_robin_df_all <- data.frame()
    
    # set parameters
    pars <- 'avg'
    
    # /----------------------------------------------------------------------------#
    #/  Panel A - Rem.Wet area lineplot                                 --------
    source('plots/fig2abcd/fig2a_2021.r')
    
    # /----------------------------------------------------------------------------#
    #/  Panel B - Map Wet.Loss map + high Pres.Wet areas                    --------
    source('plots/fig2abcd/fig1a_2022_drainarea.r')
    
    # /----------------------------------------------------------------------------#
    #/  Panel C - Map wetland area change                       --------
    source('plots/fig2abcd/fig1b_2022_preswet.r')
    
    # /----------------------------------------------------------------------------#
    #/  Panel D - Wetland area in 1700                       --------
    source('plots/fig2abcd/fig1c_wetland1700.r')
    
    # /----------------------------------------------------------------------------#
    #/  Panel E - Stacked area                                 --------
    source('plots/fig2abcd/fig2c_2021.r')
    
    # /----------------------------------------------------------------------------#
    #/  Panel F - Map of dominant driver                                 --------
    source('plots/fig2abcd/fig2d_2021.r')
    
    
    
    blank <- ggplot() + theme_void()
    
    # /----------------------------------------------------------------------------#
    #/    Set margin sizes                                               ------
    
    # set tight margins so plots are close side-by-side
    # Left column
    fig2a <- fig2a + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
    fig2c <- fig2c  + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
    
    # Right column  
    # Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
    fig2b_onlyloss <- fig2b_onlyloss + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
    fig2b_preswetonly_percchange <- fig2b_preswetonly_percchange + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
    fig2b_1700wet <- fig2b_1700wet + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
    fig2d <- fig2d + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
    
    
    # /----------------------------------------------------------------------------#
    #/    Make multipanel plot: arrange in grid                          --------
    
    # arrange plots grob into layout 
    fig2 <- plot_grid(fig2a, fig2b_onlyloss, 
                      blank, fig2b_preswetonly_percchange,
                      blank, fig2b_1700wet,
                      fig2c, fig2d,
                      
                      ncol=2, nrow=4, 
                      rel_heights = c(1, 1, 1, 1),
                      rel_widths = c(.5, 1),
                      
                      labels = c('A','B','','C','','D','E','F'),
                      align='v')
    
    # /----------------------------------------------------------------------------#
    #/    Save figure to file                                               --------
    
    # ggsave(paste0('../output/figures/fig2/fig2abcd__s',s_i,'_p',p_i,'_', test_theta, '_', pars, '_v2.png'), 
    #        fig2,
    #        width=190, height=130, dpi=600, units='mm' )
    
    ggsave(paste0('../output/figures/fig2/v8/fig2abcd_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_v8_pres1_past5.pdf'), 
           fig2,
           width=190, height=230, dpi=600, units='mm')
    # dev.off()
    
  }
}
