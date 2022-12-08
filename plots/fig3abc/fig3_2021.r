
# /----------------------------------------------------------------------------#
#/  Fig 2 A - Case studies map
source('plots/fig3abc/fig3a_2021_v2.r')

# /----------------------------------------------------------------------------#
#/  Fig 2 B - scatterplot vs CaseStudies (Davidson, WET, ...)
source('plots/fig3abc/fig3b_2021.r')

# /----------------------------------------------------------------------------#
#/  Fig 3 C - cumul lineplot
source('plots/fig3abc/fig3c_2021.r')



# /----------------------------------------------------------------------------#
#/    Make multipanel plot                                               ------

# arrange plots grob into layout 
fig3 <- plot_grid(fig3a_histcase_map, fig3_scatter, fig3c_cumulplot,
                  ncol=1, nrow=3,
                  rel_heights = c(0.4, 1.3, 1),
                  labels = c('A','B','C'),
                  align='v')

# fig3

# /----------------------------------------------------------------------------#
#/    Save figure to file                                               --------

ggsave('../output/figures/fig3/fig3abc_2021_v2.png', fig3,
       width=90, height=220, dpi=600, units='mm' )

ggsave('../output/figures/fig3/fig3abc_2021_v2.pdf', fig3,
       width=90, height=220, dpi=600, units='mm')

dev.off()







# fig2a_wetarea_lineplot <- remwet_winset
# set tight margins so plots are close side-by-side
# fig2a <- fig2a + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
# fig2c   <- fig2c   + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
# 

# arrange in grid --------------------------------------------------------------
# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
# fig2b <- fig2b + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
# fig2d <- fig2d + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))

