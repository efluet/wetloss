#  /----------------------------------------------------------------------------#
#/   Read CSV of wetland loss 
# cs_joined <- read.csv('../output/results/histcase_wetloss_joined.csv')
# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_v4_serialmcmc.csv')
cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_serialmcmcdf_v5.csv')

#  /----------------------------------------------------------------------------#
#/      Read WET index csv of point locations

wetindex_df <-  read.csv('../output/results/WETindex_cases_wmappedwetloss1700to1970.csv') %>% 
                mutate(wetloss_perc_1700to1970 <- wetloss_perc_1700to1970 * -1) %>% 
                filter(Ramsar.type != 'Human-made') %>% 
                filter(! Land.cover..from.paper. %in%  c('Seagrass','Mangroves', 'Oyster reef'))


# /----------------------------------------------------------------------------#
#/ Function that extract the grid between 1700-2000.
source('./data_proc/post_overlay_analysis/get_grid_1700_2000_perc_wetloss.r')
# Returns: wetlossperc_1700_to_2000_df


remwet_Mkm2_stack_all_df <- bind_cols(remwet_Mkm2_stack_low_df, remwet_Mkm2_stack_best_df, remwet_Mkm2_stack_high_df)
names(remwet_Mkm2_stack_all_df) <- c('low','best','high')

remwet_Mkm2_stack_all_df <- remwet_Mkm2_stack_all_df %>% 
                            arrange(best) %>% 
                            mutate(cumbest=cumsum(best),
                                   cumlow=cumsum(low),
                                   cumhigh=cumsum(high)) 
  




remwet_Mkm2_stack_all_df <- remwet_Mkm2_stack_all_df %>%  filter(best>0)

out <- data.frame()
r=1
for (i in seq(0,100,0.01)){
  print(i)
  
  out[r,'v'] <- i  
  out[r,'cumbest'] <- sum(remwet_Mkm2_stack_all_df$best <= i, na.rm = T)/nrow(remwet_Mkm2_stack_all_df)
  out[r,'cumlow'] <- sum(remwet_Mkm2_stack_all_df$low <= i, na.rm = T)/nrow(remwet_Mkm2_stack_all_df)
  out[r,'cumhigh'] <- sum(remwet_Mkm2_stack_all_df$high <= i, na.rm = T)/nrow(remwet_Mkm2_stack_all_df)
  r=r+1
}



#  /----------------------------------------------------------------------------#
#/    Cumul plot  of mapped and case studies                            --------
ggplot() +
  
  geom_ribbon(data=out, aes(x=v, ymin=cumlow, ymax=cumhigh), fill='grey80') +
  geom_line(data=out, aes(x=v, y=cumbest), color='black', size=0.4)  +
  
  # stat_bin(data=subset(remwet_Mkm2_stack_best_df, layer>0),
  #          aes(x=layer, y=cumsum(..count..)/sum(..count..)), 
  #          color='grey20', bins=400, geom='line') +
  
  
  # geom_ribbon(aes(x=remwet_Mkm2_stack_all_df$best, 
  #                 ymin = remwet_Mkm2_stack_low_df$cumlow/sum(remwet_Mkm2_stack_low_df$cumlow), 
  #                 ymax = remwet_Mkm2_stack_high_df$cumhigh/sum(remwet_Mkm2_stack_high_df$cumhigh)), 
  #             stat='stepribbon',fill = "blue", alpha=0.1) +
  

  
  stat_bin(data=subset(cs_joined, map_wetloss_prc_mean>=0), 
           aes(x=map_wetloss_prc_mean, y=cumsum(..count..)/sum(..count..)), 
           color='blue', bins=400, geom='line') +
  
  stat_bin(data=subset(wetindex_df, wetloss_perc_1700to1970>=0),
           aes(x=wetloss_perc_1700to1970, y=cumsum(..count..)/sum(..count..)),
           color='red', bins=400, geom='line') +
  
  line_plot_theme +
  
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(0, 100), expand=c(0,0)) +
  
  xlab('Mapped wetland loss (%)') + 
  ylab('Cumulative density') +
  
  theme(panel.border = element_blank(),#element_rect(color='black', size=0.5, fill=NA),
        legend.position = 'top') 


### save plot ------------------------------------------------------------------
ggsave('../output/figures/hist_cases/cumul_wetloss_histcases_vsbackground_ci.png',
       width=47, height=80, dpi=600, units='mm', type = 'cairo-png')

ggsave('../output/figures/hist_cases/cumul_wetloss_histcases_vsbackground_ci.pdf',
       width=97, height=93, units='mm')

dev.off()



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

