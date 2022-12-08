
# library for 
library("ggforce")


# read natural wetland area
wetarea <-  read.csv("./output/results/wetloss/sum/wetloss_all_area.csv") %>% 
                dplyr::select(name, year, overlap, tot_wet_Mkm2, tot_remwet_Mkm2) %>%
                # only after 600BC
                filter(year >= -6000) %>%
                # get model from name column
                # remove .nc
                mutate(name=gsub("\\..*","",name)) %>%              
                separate(name, into = c('datatype1','datatype2','exp','model'), sep="_", extra = 'drop', remove = FALSE) %>%
                mutate(model=ifelse(name=='fmax','fmax',model)) %>%
                select(-datatype1) %>% select(-datatype2) %>%
                filter(model != "SDGCM")



# wetarea_mean <- wetarea %>%
#   group_by(year) %>%
#   summarize(mean_tot_remwet_Mkm2 = mean(tot_remwet_Mkm2))



#  RemWet AREA Mkm^2 plot ======================================================
ggplot(wetarea) +
  
  # nat wet  line
  geom_line(aes(x=year, y= tot_wet_Mkm2, color=name), size=0.4) +
  
  # line of remwet, for each overlap
  # geom_line(aes(x=year, y= tot_remwet_Mkm2, color=name, 
  #               group=paste0(name, overlap)), size=0.3) +
  
  # points of remwet, symbol for overlay 
  # geom_point(data=subset(wetarea, year %% 50 ==0),
  #            aes(x=year, y= tot_remwet_Mkm2, color=name, shape=overlap), 
  #            fill='white', size=0.8) +
  
  # add line of ensemble mean
  # geom_line(data=wetarea_mean,
  #           aes(x=year, y= mean_tot_remwet_Mkm2), 
  #           color="black", size=0.3) +
  
  # axis lables
  xlab("Year") +  ylab(expression(Wetland~area~~(10^6~~km^2))) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(5,45), breaks=c(5, 10, 20, 30, 40)) +
  # empty point shape
  scale_shape_manual(values=c(21, 22, 24)) +
  
  # add a zoomed-in panel
  facet_zoom(x = year>=1700, zoom.size = 1,show.area=T) + 
  
  # theme stuff
  line_plot_theme +
  theme(legend.position = c(0.1, 0.8))




### save plot ------------------------------------------------------------------
ggsave("./output/figures/line_plot_sum_nat_remwet_2colwide.png",
       width=178, height=140, dpi=600, units='mm', type = "cairo-png")

dev.off()







#  RemWet AREA Mkm^2 plot ======================================================
ggplot(subset(wetarea) +
  
  # nat wet  line
  geom_line(aes(x=year, y= tot_wet_Mkm2, color=name), size=0.4) +
  
  # plot nat wet cover line
  # geom_line(data=wetloss_pre1700,
  #           aes(x=year, y= remwet_percfrom1700, color=name, group=overlap), size=0.4) +
  # 
  # # points of remwet, symbol for overlay 
  # geom_point(data=wetloss_pre1700,
  #            aes(x=year, y= remwet_percfrom1700, color=name, shape=overlap), 
  #            fill='white', size=0.8) +
  # 
  # # plot nat wet cover line
  # geom_line(data=wetloss_pre1700,
  #           aes(x=year, y= natwet_percfrom1700, color=name, group=overlap), linetype=2, size=0.4) +
  # 
  # axis lables
  xlab("Year") +  ylab("Global wetland cover relative to 1700 (%)") +
  
  #ylab(expression(Global~wetland~area~~(10^6~~km^2))) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0),breaks= c(-6000, -4000, -2000, 0, 1700)) +
  #scale_y_continuous(expand=c(0,0), limits=c(10, 120), breaks=c(20, 40, 60, 80, 100, 120)) +
  # empty point shape
  scale_shape_manual(values=c(21, 22, 24)) +
  line_plot_theme +
  theme(legend.position = c(0.2, 0.3), #"none",
        plot.margin = margin(1,-3.15,1,1,"mm"))



# POST 1700 plot =============================================
post_1700_plot <- ggplot() +
  
  
  # plot nat wet cover line
  geom_line(data=wetloss_post1700,
            aes(x=year, y= remwet_percfrom1700, color=name, group=paste0(name,overlap)), size=0.4) +
  
  # points of remwet, symbol for overlay 
  geom_point(data=wetloss_post1700,
             aes(x=year, y= remwet_percfrom1700, color=name, shape=overlap), 
             fill='white', size=0.8) +
  
  
  # Add nick davidson's estimates
  geom_line(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
            color='black', size=0.35) +
  
  geom_point(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
             color='black', size=0.8) +
  
  geom_text(label="Meta-analysis from\nDavidson 2014", aes(x=1950, y= 45), size=2.8) +
  
  # axis lables
  xlab("Year") +  ylab("") +
  
  # axis scales
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 120), breaks=c(20, 40, 60, 80, 100, 120)) +
  # empty point shape
  scale_shape_manual(values=c(21, 22, 24)) +
  # theme stuff
  line_plot_theme +
  theme(legend.position = c(0.05, 0.3),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(1,1,1,-3.15,"mm"))









# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # combine rows
# sum_nat_wet_comb <- bind_rows(sum_nat_wet_20th, sum_nat_wet_lpx)
# 
# # remove pieces
# rm(sum_nat_wet_20th, sum_nat_wet_lpx)
# 
# 
# sum_nat_wet_comb <- sum_nat_wet_comb %>%
# 
# # plot 6000BC-2000AD ===========================================================
# 
# # get package that does ggzoom
# library(ggforce)
# 
# ggplot(sum_nat_wet_comb) +
# 
#   # add geometries
#   geom_line(aes(x=year, wet_Mkm2, color=name), size=1) +
#   geom_point(aes(x=year, wet_Mkm2, color=name), size=1) +
#   
#   # axis lables
#   xlab("Year") +  ylab(expression(Wetland~area~~(10^6~~km^2))) +
#   
#   # axis scales
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0), limits=c(0,40)) +
#   
#   # add a zoomed-in panel
#   facet_zoom(x = year>=1700, zoom.size = 1) + 
#   
#   # theme stuff
#   line_plot_theme +
#   theme(legend.position = c(0.1, 0.8))
# 
# ### save plot ------------------------------------------------------------------
# ggsave("./output/figures/line_plot_sum_nat_remwet.png",
#        width=87, height=120, dpi=600, units='mm', type = "cairo-png")
# 
# dev.off()




# # read in global sums from WETCHIMP data
# i <- "./output/results/old/global_sum_nat_wetland_20th.csv"
# sum_nat_wet_20th <- read.csv(i, stringsAsFactors = F) %>%
#                     gather(year_type, year, year_end:year_start) %>%
#                     dplyr::select(-one_of(c('year_type',"X")))
# 
# 
# 
# i <- "./output/results/old/global_sum_wetloss_v2.csv"
# sum_nat_wet_lpx <-  read.csv(i, stringsAsFactors = F) %>%
#                     mutate(wet_Mkm2 = tot_wet_Mkm2,
#                            name = "LPX-DYTOP") %>%
#                     dplyr::select(year, name, wet_Mkm2)



