#  /----------------------------------------------------------------------------#
#/   Read CSV of wetland loss 
cs_joined <- read.csv(paste0('../output/results/histcases/cs_joined_s', s_i, '_p', p_i, '.csv')) 


#  /----------------------------------------------------------------------------#
#/      Read WET index csv of point locations

# wetindex_df <-  read.csv('../output/results/WETindex_cases_wmappedwetloss1700to1970.csv') %>%
#                 # mutate(wetloss_perc_1700to1970 <- wetloss_perc_1700to1970 * -1) %>%
#                 filter(Ramsar.type != 'Human-made') %>%
#                 filter(! Land.cover..from.paper. %in%  c('Seagrass','Mangroves', 'Oyster reef'))

xy <- wetindex_df[ , c("Longitude","Latitude")]

wetindex_pts <- SpatialPointsDataFrame(coords = xy, data = wetindex_df, 
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))




#/  extract mapped loss of 1700-2000.

# From map in fig 2b
r <- rasterFromXYZ(as.data.frame(cumul_drained_for_fig2c_map)[, c('x', 'y', 'X2000')])
wetindex_df$mappedcumulloss <- raster::extract(r, wetindex_pts)

# source('./data_proc/post_overlay_analysis/get_grid_1700_2000_perc_wetloss.r')


# /----------------------------------------------------------------------------#
#/    Cumul plot  of mapped and case studies                            --------

wetloss_cumulplot <- 
  
  ggplot() +
  
  # Global background -----------------------------------
  stat_bin(data=subset(cumul_drained_for_fig2c_map, X2000>0),
           aes(x=X2000, y=cumsum(..count..)/sum(..count..)),
           color='black', bins=500, geom='line') +
  
  geom_text(aes(x=16,y=.92), label='Global reconstruction\n(n=49,867; 1700-2000)', color='black', size=2.7) +
  
  
  # WET-------------------------------
  stat_bin(data=subset(wetindex_df, mappedcumulloss>0), 
           aes(x=mappedcumulloss, y=cumsum(..count..)/sum(..count..)), 
           color='red', bins=500, geom='line') +
  
  geom_text(aes(x=80, y=.8), label='WET index\n(n=747; 1970-2015)', color='red', size=2.7) +
  
  
  # Davidson  -----------------------------
  stat_bin(data=subset(cs_joined, map_perc_lost>0),
           aes(x=map_perc_lost, y=cumsum(..count..)/sum(..count..)),
           color='blue', bins=500, geom='line') +
  
  geom_text(aes(x=50, y=.55), label='Long-term\nloss estimates\n(n=78; 1780-2010)', color='blue', size=2.7) +
  
  line_plot_theme +
  # scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(0,100))+ #, expand=c(0,0)) +
  xlab('Mapped wetland loss 1700-2000 (%)') + 
  ylab('Cumulative density') +
  
  theme(panel.background = element_rect(color="black", size=0.5, fill=NA),
        plot.margin=unit(c(40, 1, 3, 1), 'mm'))
        # legend.position = 'top') 

wetloss_cumulplot

# ### save plot ------------------------------------------------------------------
# ggsave('../output/figures/hist_cases/cumul_wetloss_histcases_vsbackground_v5.png',
#        width=90, height=90, dpi=600, units='mm', type = 'cairo-png')
# 
# ggsave('../output/figures/hist_cases/cumul_wetloss_histcases_vsbackground_v5.pdf',
#        width=90, height=90, units='mm')
# 
# dev.off()



# 
# #  /---------------------------------------------------------------------------#
# #/   Plot distribution of wetloss from the map                             -----
# 
# # Make function of plot
# histogram_dist_plot<- function(dfname, xname, fillcolor='white'){
#   
#   p <- ggplot() +
#     
#     ##  plot bars
#     geom_histogram(data=dfname, aes(x=dfname[[xname]], y=..density..), 
#                    binwidth=10, size=0.5,
#                    color='black', fill=fillcolor) +
#     
#     line_plot_theme +
#     coord_flip() +
#     scale_y_continuous(expand=c(0,0)) +
#     scale_x_continuous(limits=c(-100, 100)) +
#     
#     xlab('Mapped wetland loss (%)') + ylab('density') +
#     
#     theme(panel.border = element_rect(color='black', size=0.5),
#           legend.position = 'top')
#   
#   # return the plot
#   return(p)
# }
# 
# 
# # /----------------------------------------------------------------------------#
# #/ make histogram/density plot                      --------
# 
# # for background
# histogram_dist_plot(wetlossperc_1700_to_2000_df, 'layer', 'grey90')
# 
# # for Davidson et al case studies
# histogram_dist_plot(cs_joined, 'map_wetloss_prc_mean', 'lightblue')
# 
# # for WET index
# histogram_dist_plot(wetindex_df, 'wetloss_perc_from1700to1970', 'yellow')
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/    make histogram of mapped and case studies                    -------------
# ggplot() +
#   
#   geom_histogram(data=wetlossperc_1700_to_2000_df, 
#                  aes(x=layer, y=..count../sum(..count..)), 
#                  binwidth=10, size=0.1, color='black', fill='grey85') +
#   
#   # points
#   geom_histogram(data=cs_joined, 
#                  aes(x=map_wetloss_prc_mean, y=..count../sum(..count..)), 
#                  binwidth=10, size=0.1,
#                  color='black', fill='lightblue', alpha=0.5) +
#   
#   line_plot_theme +
#   coord_flip() +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(limits=c(-100, 100)) +
#   
#   xlab('Mapped wetland loss (%)') + ylab('density') +
#   
#   theme(panel.border = element_rect(color='black', size=0.5, fill=NA),
#         legend.position = 'top') 

