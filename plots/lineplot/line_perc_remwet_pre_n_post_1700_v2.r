


wetloss_all <-  read.csv("./output/results/wetloss/sum/wetloss_all_area.csv", stringsAsFactors = F) %>% 
  dplyr::select(name, year, overlap, tot_wet_Mkm2, tot_remwet_Mkm2) %>%
  filter(year >= -6000)



wetloss_at6000 <-  wetloss_all %>% filter(year == -6000) %>% dplyr::select(-one_of("year"))
names(wetloss_at6000) <- c("name", "overlap",  "tot_wet_Mkm2_in1700", "tot_remwet_Mkm2_in6000")


wetloss_pre1700 <- wetloss_all %>% 
                   filter(year <= 1700, name=="trace21_129.cdf") %>%
                   left_join(., wetloss_at6000, by=c("name", "overlap")) %>%
                   mutate(remwet_percfrom6000 = tot_remwet_Mkm2/tot_remwet_Mkm2_in6000 *100) %>%
                   mutate(natwet_percfrom6000 = tot_wet_Mkm2/tot_remwet_Mkm2_in6000 *100)


wetloss_post1700 <- wetloss_all %>% filter(year >= 1700) %>%
                    left_join(., wetloss_at6000, by=c("name","overlap")) %>%
                    mutate(remwet_percfrom1700 = tot_remwet_Mkm2/tot_remwet_Mkm2_in1700 *100)




# read davison data
f<-"./data/hist_records/source_specific/davidson_2014/davidson2014_global_percent_wetloss.csv"
davidson2014 <- read.csv(f, stringsAsFactors = F) %>% filter(!is.na(percentage_fromtext_nfig4))


#  RemWet AREA Mkm^2 plot ======================================================
pre_1700_plot <- ggplot() +

  geom_hline(yintercept = 100, color="grey60", size=0.5) +
    
  # plot nat wet cover line
  geom_line(data=wetloss_pre1700,
            aes(x=year, y= remwet_percfrom6000, color=name, group=overlap), size=0.4) +
  
  # points of remwet, symbol for overlay 
  geom_point(data=wetloss_pre1700,
             aes(x=year, y= remwet_percfrom6000, color=name, shape=overlap), 
             fill='white', size=0.8) +
  
  # plot nat wet cover line
  geom_line(data=wetloss_pre1700,
            aes(x=year, y= natwet_percfrom6000, color=name, group=overlap), linetype=2, size=0.4) +
  
  # axis lables
  xlab("Year") +  ylab("Global wetland cover relative to 1700 (%)") +
  
  #ylab(expression(Global~wetland~area~~(10^6~~km^2))) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0),breaks= c(-6000, -4000, -2000, 0, 1700)) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 120), breaks=c(20, 40, 60, 80, 100, 120)) +
  # empty point shape
  scale_shape_manual(values=c(21, 22, 24)) +
  line_plot_theme +
  theme(legend.position = c(0.2, 0.3), #"none",
        plot.margin = margin(1,-3.15,1,1,"mm"))

pre_1700_plot



# POST 1700 plot =============================================
post_1700_plot <- ggplot() +
  
  
  geom_hline(yintercept = 100, color="grey60", size=0.5) +
  
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


# arrange together
pre_n_post <- grid.arrange(pre_1700_plot, post_1700_plot, nrow=1)

### save plot ------------------------------------------------------------------
ggsave("./output/figures/line_plot_remwet_perc_since1700_v3.png", pre_n_post,
       width=178, height=100, dpi=600, units='mm', type = "cairo-png")

ggsave("./output/figures/line_plot_remwet_perc_since1700_v3.pdf", pre_n_post,
       width=178, height=100, dpi=600, units='mm')


dev.off()





# # line of remwet, for each overlap
# geom_line(aes(x=year, y= tot_remwet_Mkm2, color=name, 
#               group=paste0(name, overlap)), size=0.3) +
# 
# # points of remwet, symbol for overlay 
# geom_point(data=subset(wetloss_all, year %% 50 ==0),
#            aes(x=year, y= tot_remwet_Mkm2, color=name, shape=overlap), 
#            fill='white', size=0.8) +
# 
# # add line of ensemble mean
# # geom_line(data=wetloss_all_mean,
# #           aes(x=year, y= mean_tot_remwet_Mkm2), 
# #           color="black", size=0.3) +
# 
# 
# # add a zoomed-in panel
# facet_zoom(x = year>=1700, zoom.size = 1,show.area=T) + 
# 
# # theme stuff
# line_plot_theme +
# theme(legend.position = c(0.1, 0.8))

# wetloss_all <-  read.csv("./output/results/wetloss/sum/wetloss_all_area.csv") %>%
#   filter(year >= 1700)
# 
# wetloss_all_mean <- wetloss_all %>%
#   group_by(year) %>%
#   summarize(mean_tot_remwet_Mkm2 = mean(tot_remwet_Mkm2),
#             mean_remwet_prc_since1700 = mean(remwet_prc_since1700))
# 
# # get max area lost???
# max(wetloss_all_mean$mean_tot_remwet_Mkm2) - min(wetloss_all_mean$mean_tot_remwet_Mkm2)
# 
# 
# 
# #  REMWET PERCENTAGE PLOT ======================================================
# 
# # read davison data
# f<-"./data/hist_records/source_specific/davidson_2014/davidson2014_global_percent_wetloss.csv"
# davidson2014 <- read.csv(f, stringsAsFactors = F) %>% filter(!is.na(percentage_fromtext_nfig4))
# 
# 
# 
# ggplot(wetloss_all) +
#   
#   # add lines
#   geom_line(aes(x=year, y=remwet_prc_since1700, color=name, group=paste0(name, overlap))) +
#   
#   # add points for each scenario 
#   geom_point(aes(x=year, y=remwet_prc_since1700, color=name, shape=overlap), fill='white') +
#   
#   # add ensemble mean line
#   geom_line(data=wetloss_all_mean, aes(x=year, y=mean_remwet_prc_since1700), color='darkblue') +
#   
#   # Add nick davidson's estimates
#   geom_line(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
#             color='black', size=0.3) +
#   
#   geom_point(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
#              color='black', size=0.6) +
#   
#   # select shapes- empty point shape
#   scale_shape_manual(values=c(21, 22, 24)) +
#   line_plot_theme
# 
# 
# 
# # save figure to file
# ggsave('./output/figures/lineplot_remwet_global_perc_allcomb_since1700.png',  
#        width=178, height=90, dpi=600, units="mm", type = "cairo-png")
# dev.off()
# 
# 
# # delete objects
# rm(f, davidson2014, wetloss_all)
