# Fig 2 A - Rem.Wet area lineplot
# Fig 2 B - Wet.Loss map + high Pres.Wet areas
# Fig 2 C - Drain LU type stacked-area plot
# Fig 2 D - LU loss driver map


# /----------------------------------------------------------------------------#
#/  Fig 2 A - Rem.Wet area lineplot
source('plots/fig2abcd/fig2a_wetarea_lineplot.r')
# NOW ITS p FROM make_final_drainmap_v2

# /----------------------------------------------------------------------------#
#/  Fig 2 B - Wet.Loss map + high Pres.Wet areas
# TODO UPDATE
source('plots/fig2abcd/fig2b_wetarealoss_map_df.r')


# /----------------------------------------------------------------------------#
#/  Fig 2 C - Drain LU type stacked-area plot
# TODO UPDATE
source('plots/fig2abcd/fig2c_stackedarea_lu_drain.r')


# /----------------------------------------------------------------------------#
#/  Fig 2 D - Drain LU type stacked-area plot
# TODO UPDATE
source('plots/fig2abcd/fig2d_ludriver_map_df.r')



# /----------------------------------------------------------------------------#
#/    Make multipanel plot                                               ------

fig2a_wetarea_lineplot <- remwet_winset
# set tight margins so plots are close side-by-side
fig2a_wetarea_lineplot <- fig2a_wetarea_lineplot + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))
fig2c_barplot_lutype   <- fig2c_barplot_lutype   + theme(plot.margin=unit(c(3, 3, 4, 3), 'mm'))


# arrange in grid --------------------------------------------------------------
# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
arealossmap <- arealossmap + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))
drivermap   <- drivermap   + theme(plot.margin=unit(c(-13, -5.5, -9, 1), 'mm'))


# arrange plots grob into layout 
p <- plot_grid(fig2a_wetarea_lineplot, arealossmap, 
               fig2c_barplot_lutype, drivermap,
               
               ncol=2, nrow=2, 
               rel_heights = c(1, 1),
               rel_widths = c(.6, 1),
               
               labels = c('', 'C', 'D','E'),
               align='h')

p

# /----------------------------------------------------------------------------#
#/    Save figure to file                                               --------

ggsave('../output/figures/fig2/fig2abcd_drain_wetarea_plots_v11.png', p,
       width=190, height=130, dpi=300, units='mm') #type = 'cairo-png')

ggsave('../output/figures/fig2/fig2abcd_drain_wetarea_plots_v11.pdf', p,
       width=190, height=130, dpi=300, units='mm') #type = 'cairo-png')
dev.off()






# /----------------------------------------------------------------------------#
# #/  RemWet AREA Mkm^2 plot                                               -------
# 
# ggplot(sum_overlap) +
#   
#   # plot nat wet cover line
#   geom_area(aes(x=year, y= sum_overlap/10^6, fill=lu,  group=lu), color='white', size=0.4, position = 'stack') +
#   # axis lables
#   xlab('Year') +  ylab(expression(paste('Wetland area (10'^{6},' km'^{2},')'))) +
#   
#   facet_rep_wrap(.~ overlap, ncol=1) +
#   
#   # axis scales
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   
#   line_plot_theme +
#   theme(#legend.position = c(0.2, 0.3), #'none',
#     plot.margin = margin(1,1,1,1,'mm')) #+ ggtitle('Cropland')
