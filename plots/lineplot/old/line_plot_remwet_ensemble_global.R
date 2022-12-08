
wetloss_ensemble_prc <- read.csv("./output/results/wetloss/wetloss_ensemble_prc")


# read davison data
f<-"./data/hist_records/source_specific/davidson_2014/davidson2014_global_percent_wetloss.csv"
davidson2014 <- read.csv(f, stringsAsFactors = F) %>% filter(!is.na(percentage_fromtext_nfig4))



# percentage plot with ribbon ==================================================

ggplot(wetloss_ensemble_prc) +
  
  geom_line(aes(x=year, y= remwet_prc_since1700_mean), color='blue', size=0.3) +
  
  geom_ribbon(aes(x=year, ymin=remwet_prc_since1700_min, ymax=remwet_prc_since1700_max), 
              fill='blue', alpha=0.2) +
  
  
  geom_line(data=davison2014, 
            aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
            color='black', size=0.3) +
  
  geom_point(data=davison2014, 
             aes(x=ï..year_start, y= percentage_fromtext_nfig4), 
             color='black', size=0.6) +
  
  xlab("") + ylab("Remaining wetland (%)") +  
  line_plot_theme



# save figure to file
ggsave('./output/figures/remwet_global_ensemblemean_vs_davidson.png',  
       width=87, height=80, dpi=600, units="mm", type = "cairo-png")
dev.off()



#  delete objects
rm(wetloss_ensemble_prc, f, davidson2014)

