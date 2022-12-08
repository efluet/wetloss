# Fig 2 A - Rem.Wet area lineplot
# Fig 2 B - Wet.Loss map + high Pres.Wet areas
# Fig 2 C - Drain LU type stacked-area plot
# Fig 2 D - LU loss driver map


# Apply floor values for maps; tuned for s4p1
map_cumullossperc_floor = 5 #0.05 #1
map_Fwet1700_floor = 5 #0.05  # b is 1
map_Fwet1700_floor_forwetmap = 10 #10

#$$$$$
total_drain_peryear_robin_df_all <- data.frame()

# set parameters
pars <- 'avg'

# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste(s_i, p_i))
  
  # /----------------------------------------------------------------------------#
  #/  Fig 2 A - Rem.Wet area lineplot
  source('plots/fig2abcd/fig2a_2021.r')
  
  # /----------------------------------------------------------------------------#
  #/  Fig 2 B - Wet.Loss map + high Pres.Wet areas
  source('plots/fig2abcd/fig2b_2021_perc.r')
  
  # /----------------------------------------------------------------------------#
  #/  Fig 2 C - Drain LU type stacked-area plot
  source('plots/fig2abcd/fig2c_2021.r')
  
  # /----------------------------------------------------------------------------#
  #/  Fig 2 D - Drain LU type stacked-area plot
  source('plots/fig2abcd/fig2d_2021.r')
  
  
  
  # /----------------------------------------------------------------------------#
  #/    Make multipanel plot                                               ------
  
  # fig2a_wetarea_lineplot <- remwet_winset
  # set tight margins so plots are close side-by-side
  fig2a <- fig2a + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
  fig2c   <- fig2c  + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
  
  
  # arrange in grid --------------------------------------------------------------
  # set tight margins so plots are close side-by-side
  # Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
  fig2b <- fig2b + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
  fig2d <- fig2d + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
  
  
  # arrange plots grob into layout 
  fig2 <- plot_grid(fig2a, fig2b, 
                    fig2c, fig2d,
                 
                 ncol=2, nrow=2, 
                 rel_heights = c(1, 1),
                 rel_widths = c(.6, 1),
                 
                 labels = c('A','B','C','D'),
                 align='h')
  
  # /----------------------------------------------------------------------------#
  #/    Save figure to file                                               --------
  
  # ggsave(paste0('../output/figures/fig2/fig2abcd__s',s_i,'_p',p_i,'_', test_theta, '_', pars, '_v2.png'), 
  #        fig2,
  #        width=190, height=130, dpi=600, units='mm' )
  
  ggsave(paste0('../output/figures/fig2/v7/fig2abcd_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_v7_resub1.pdf'), 
         fig2,
         width=190, height=130, dpi=600, units='mm')
  # dev.off()

  }
}



