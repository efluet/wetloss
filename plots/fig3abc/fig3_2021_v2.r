# Dataframe of 
corr_r2_df <- data.frame()

# Run without thetas
# test_theta=1
# pars='avg'

# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste0('s', s_i, ' p', p_i))

  # /----------------------------------------------------------------------------#
  #/  Fig 2 A - Case studies map
  source('plots/fig3abc/fig3a_2021_v2.r')
  
  # /----------------------------------------------------------------------------#
  #/  Fig 2 B - scatterplot vs CaseStudies (Davidson, WET, ...)
  source('plots/fig3abc/fig3b_2021.r')
  

  # /----------------------------------------------------------------------------#
  #/    Make multipanel plot                                               ------
  
  # arrange plots grob into layout 
  fig3 <- plot_grid(fig3a_histcase_map, fig3_scatter, # fig3c_cumulplot,
                    ncol=1, nrow=2,
                    rel_heights = c(0.45, 1), # 1),
                    labels = c('A','B')) #,'C'),
                    # align='v')
  
  # fig3
  
  # /----------------------------------------------------------------------------#
  #/    Save figure to file                                               --------
  if(1){
    ggsave(paste0('../output/figures/fig3/v12/fig3ab_2021_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_v12.png'), fig3,
           width=90, height=230, dpi=600, units='mm' )
    
    ggsave(paste0('../output/figures/fig3/v12/fig3ab_2021_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_v12.pdf'), fig3,
           width=90, height=230, dpi=600, units='mm')
    }

  # end of loop
  }
}

