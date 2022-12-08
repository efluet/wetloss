# FIG 4 A & B PANELS 
# arrange in grid --------------------------------------------------------------
# set tight margins so plots are close side-by-side


# Combine consump map & inset
histcase_map_grob = ggplotGrob(histcase_map)
wetloss_cumulplot_w_inset = wetloss_cumulplot + annotation_custom(grob = histcase_map_grob, 
                                                           xmin= 1,
                                                           xmax= 99,
                                                           ymin= 1.05, 
                                                           ymax= 1.5)


# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
wetloss_scatterplot_m       <- wetloss_scatterplot + theme(plot.margin=unit(c(40, 2, 2, 1), 'mm'))
wetloss_cumulplot_w_inset_m <- wetloss_cumulplot_w_inset   + theme(plot.margin=unit(c(40, 1, 2, 2), 'mm'))




# arrange plots grob into layout 
library(ggpubr)  #ggarrange
p <- ggarrange(wetloss_scatterplot_m, wetloss_cumulplot_w_inset_m,
               
               ncol=2, nrow=1, 
               # rel_heights = c(2, 0.1),
               # rel_widths = c(1, 1),
               
               labels = c('A', 'B'),
               align='h')

# p


# /----------------------------------------------------------------------------#
#/    Save figure to file          --------

ggsave('../output/figures/fig4ab_wetloss_histcases_v4.png', p,
       width=180, height=130, dpi=300, units='mm') #type = 'cairo-png')

ggsave('../output/figures/fig4ab_wetloss_histcases_v4.pdf', p,
       width=180, height=130, dpi=300, units='mm') #type = 'cairo-png')

dev.off()

