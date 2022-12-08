
wetloss_all <- read.csv("./output/results/wetloss/wetloss_all")

wetloss_all_mean <- wetloss_all %>%
                    group_by(year) %>%
                    summarize(mean_tot_remwet_Mkm2 = mean(tot_remwet_Mkm2))


max(wetloss_all_mean$mean_tot_remwet_Mkm2) - min(wetloss_all_mean$mean_tot_remwet_Mkm2)


#  RemWet AREA Mkm^2 plot ======================================================
ggplot(wetloss_all) +
  geom_line(aes(x=year, y= tot_remwet_Mkm2, color=name, 
                group=paste0(name, overlap)), size=0.3) +
  geom_point(aes(x=year, y= tot_remwet_Mkm2, color=name, shape=overlap), 
             fill='white', size=0.6) +
  
  geom_line(data=wetloss_all_mean,
            aes(x=year, y= mean_tot_remwet_Mkm2), 
            color="black", size=0.3) +
  
  
  # axis labels
  labs(x="", y=expression(Remaining~wetland~area~(km^{2}))) +
  scale_shape_manual(values=c(21, 22, 24)) + 
  line_plot_theme



# save figure to file
ggsave('./output/figures/remwet_global_area_allcomb_since1700.png',  
       width=178, height=90, dpi=600, units="mm", type = "cairo-png")
dev.off()






#  REMWET pERCENTAGE PLOT ======================================================

# read davison data
f<-"./data/hist_records/source_specific/davidson_2014/davidson2014_global_percent_wetloss.csv"
davidson2014 <- read.csv(f, stringsAsFactors = F) %>% filter(!is.na(percentage_fromtext_nfig4))



ggplot(wetloss_all) +

  geom_line(aes(x=year, y= remwet_prc_since1700, color=name, 
                group=paste0(name, overlap))) +
  geom_point(aes(x=year, y= remwet_prc_since1700, color=name, shape=overlap), fill='white') +
  
  
  geom_line(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
            color='black', size=0.3) +
  
  geom_point(data=davidson2014, aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
             color='black', size=0.6) +

  # empty point shape
  scale_shape_manual(values=c(21, 22, 24)) +
  line_plot_theme



# save figure to file
ggsave('./output/figures/remwet_global_percent_allcomb_since1700.png',  
       width=178, height=90, dpi=600, units="mm", type = "cairo-png")
dev.off()


# delete objects
rm(f, davidson2014, wetloss_all)
