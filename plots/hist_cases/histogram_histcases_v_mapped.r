#  /----------------------------------------------------------------------------#
#/   Read CSV of wetland loss 
cs_joined <- read.csv("./output/results/histcase_wetloss_joined.csv")


#  /----------------------------------------------------------------------------#
#/      Read wet index data
wetindex_df <- read.csv("./output/results/WETindex_cases_wmappedwetloss1700to1970.csv")
#wetindex_df$wetloss_perc_from1700to1970 <-  wetindex_df$wetloss_perc_from1700to1970 * -1
wetindex_df$wetloss_perc_1700to1970 <-  wetindex_df$wetloss_perc_1700to1970 * -1




# Function that extract the grid between 1700-2000.
source("./scripts/data_proc/post_overlay_analysis/get_grid_1700_2000_perc_wetloss.r")



#  /---------------------------------------------------------------------------#
#/   Plot distribution of wetloss from the map                             -----

# Make function of plot
histogram_dist_plot<- function(dfname, xname, fillcolor="white"){
  
  p <- ggplot() +
    
    ##  plot bars
    geom_histogram(data=dfname, aes(x=dfname[[xname]], y=..density..), 
                   binwidth=10, size=0.5,
                   color='black', fill=fillcolor) +
    
    line_plot_theme +
    coord_flip() +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(limits=c(-100, 100)) +
    
    xlab("Mapped wetland loss (%)") + ylab("density") +
    
    theme(panel.border = element_rect(color="black", size=0.5),
          legend.position = "top")
  
  # return the plot
  return(p)
  
}



# /----------------------------------------------------------------------------#
#/ make histogram/density plot       ============================================


# for background
histogram_dist_plot(wetlossperc_1700_to_2000_df, "layer", "grey90")

# for Davidson et al case studies
histogram_dist_plot(cs_joined, "map_wetloss_prc_mean", "lightblue")

# for WET index
histogram_dist_plot(wetindex_df, "wetloss_perc_from1700to1970", "yellow")


# ### save plot ------------------------------------------------------------------
# ggsave("./output/figures/hist_cases/density_wetloss_histcases_vsbackground.png",
#        width=47, height=80, dpi=600, units='mm', type = "cairo-png")
# 
# ggsave("./output/figures/hist_cases/density_wetloss_histcases_vsbackground.pdf",
#        width=87, height=80, dpi=600, units='mm')
# 
# dev.off()
# 



#  /----------------------------------------------------------------------------#
#/    make histogram of mapped and case studies                    -------------
ggplot() +
  
  geom_histogram(data=wetlossperc_1700_to_2000_df, 
                 aes(x=layer, y=..count../sum(..count..)), 
                 binwidth=10, size=0.1, color='black', fill="grey85") +
  
  # points
  geom_histogram(data=cs_joined, 
                 aes(x=map_wetloss_prc_mean, y=..count../sum(..count..)), 
                 binwidth=10, size=0.1,
                 color='black', fill="lightblue", alpha=0.5) +
  
  line_plot_theme +
  coord_flip() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(-100, 100)) +
  
  xlab("Mapped wetland loss (%)") + ylab("density") +
  
  theme(panel.border = element_rect(color="black", size=0.5),
        legend.position = "top") 

### save plot ------------------------------------------------------------------
ggsave("./output/figures/hist_cases/histogram_wetloss_histcases_vsbackground.png",
       width=47, height=80, dpi=600, units='mm', type = "cairo-png")

ggsave("./output/figures/hist_cases/histogram_wetloss_histcases_vsbackground.pdf",
       width=87, height=80, dpi=600, units='mm')

dev.off()







#  /----------------------------------------------------------------------------#
#/    Cumul plot  of mapped and case studies                    -------------
ggplot() +
  
  stat_bin(data=subset(wetlossperc_1700_to_2000_df,layer>=0),
           aes(x=layer, y=cumsum(..count..)/sum(..count..)), 
           color='grey20', bins=400, geom="line") +
  
  stat_bin(data=subset(cs_joined, map_wetloss_prc_mean>=0), 
           aes(x=map_wetloss_prc_mean, y=cumsum(..count..)/sum(..count..)), 
           color='blue', bins=400, geom="line") +
  
  stat_bin(data=subset(wetindex_df, wetloss_perc_from1700to1970>=0), 
           aes(x=wetloss_perc_from1700to1970, y=cumsum(..count..)/sum(..count..)), 
           color='red', bins=400, geom="line") +
  
  line_plot_theme +
  #coord_flip() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(0, 100), expand=c(0,0)) +
  
  xlab("Mapped wetland loss (%)") + 
  ylab("Cumulative density") +
  
  theme(panel.border = element_rect(color="black", size=0.5),
        legend.position = "top") 


### save plot ------------------------------------------------------------------
ggsave("./output/figures/hist_cases/cumul_wetloss_histcases_vsbackground.png",
       width=47, height=80, dpi=600, units='mm', type = "cairo-png")

dev.off()
