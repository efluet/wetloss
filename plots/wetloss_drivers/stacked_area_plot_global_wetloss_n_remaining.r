
# get global sum data


#read.csv('../../output/results/global_sum_wetland_loss.csv') %>%

globsums <- read.csv("./output/results/wetloss/sum/wetloss_ensemble_prc.csv") 
  


            # gather(var, dat, tot_remwet_Mkm2:tot_irrice_Mkm2) %>%
            # filter(!year %in% c(1700, 2000))


# set year breaks
mybreaks <- seq(-6000, 0, 2000)

# make area plot
#wetloss_plot <-
  ggplot(globsums) +
  
  geom_area(data=subset(globsums, var %in% c("tot_remwet_Mkm2","tot_wetloss_Mkm2","tot_convtorice_Mkm2")),
            aes(x=year, y=dat, fill=var)) +
  
  geom_line(data=subset(globsums, var=='tot_crop_Mkm2'), 
            aes(x=year, y=dat, color='black'), size=1) +
  
  geom_line(data=subset(globsums, var=='tot_ir_ice_Mkm2'), 
            aes(x=year, y=dat, color='pink'), size=1) +  

  xlab("Year") +  ylab("Area (Mkm2)") + 
  scale_x_continuous(expand=c(0,0), breaks=mybreaks, labels=mybreaks, limits = c(-6000, max(mybreaks))) +   
  scale_y_continuous(expand=c(0,0)) +
  scale_colour_manual(name = 'Human Land Cover', 
                      values =c('black'='black','pink'='pink'), labels = c('cropland','irrigated rice'))
  
wetloss_plot


### save plot ------------------------------------------------------------------
ggsave("../../output/figures/area_plot_sum_wetloss.png", wetloss_plot, 
       width=178, height=180, dpi=600, units='mm', type = "cairo-png")

dev.off()
