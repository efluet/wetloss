
# read in global sums from WETCHIMP data
i <- "./output/results/old/global_sum_nat_wetland_20th.csv"
sum_nat_wet_20th <- read.csv(i, stringsAsFactors = F) %>%
                    gather(year_type, year, year_end:year_start) %>%
                    dplyr::select(-one_of(c('year_type',"X")))



i <- "./output/results/old/global_sum_wetloss_v2.csv"
sum_nat_wet_lpx <-  read.csv(i, stringsAsFactors = F) %>%
                    mutate(wet_Mkm2 = tot_wet_Mkm2,
                           name = "LPX-DYTOP (Stocker et al. 2017)") %>%
                    dplyr::select(year, name, wet_Mkm2)
  


sumarea_orchidee <- readRDS("./output/results/natwet/natwet_orchidee.rds")




# combine rows
sum_nat_wet_comb <- bind_rows(sum_nat_wet_20th, sum_nat_wet_lpx)

# remove pieces
rm(sum_nat_wet_20th, sum_nat_wet_lpx)

# declare time break for x-axis
mybreaks <- c(1850, 1901, 1932, 1993, 2004)



# plot 6000BC-2000AD ===========================================================
ggplot(sum_nat_wet_comb) +
  geom_line(aes(x=year, wet_Mkm2, color=name), size=1) +
  geom_point(aes(x=year, wet_Mkm2, color=name), size=1) +
  
  scale_x_continuous(breaks=mybreaks, labels=mybreaks) +
  xlab("Year") +  ylab("Natural wetland area (10^6 km2)") +
  xlim(-6000, 2000) + ylim(5,15)


# plot 1850 onward ---------------------------------------------------
ggplot(sum_nat_wet_comb) +
  
  geom_line(aes(x=year, wet_Mkm2, color=name)) +
  #geom_point(aes(x=year, wet_Mkm2, color=name), size=1) +
  
  geom_line(data=sumarea_orchidee, aes(x=year, y=tot_natwet_Mkm2, color=model)) + 
  
  scale_x_continuous(breaks=mybreaks, labels=mybreaks, 
                     limits=c(1850,2010), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,65), expand=c(0,0)) +
  xlab("Year") +
  ylab("Natural wetland area (10^6 km2)")



### save plot ------------------------------------------------------------------
ggsave("./output/figures/natwet_lineplot/line_plot_sum_nat_wet_post1850.png",
       width=240, height=120, dpi=600, units='mm') #, type = "cairo-png")

dev.off()
