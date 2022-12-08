
source("./plots/themes/line_plot_theme.R")

# write summed area of wetloss, remaining, etc... to output
wetchimp_wetloss <- read.csv('../../output/results/global_sum_wetloss_wetchimp.csv') %>%
                    mutate(remwet_perc_from6000BC = tot_remwet_Mkm2/tot_wet_Mkm2) %>%
                    gather(var, dat, tot_wet_Mkm2:remwet_perc_from6000BC) 


globsums<- wetchimp_wetloss

# set year breaks
mybreaks <- seq(1700, 2000, 100)


# make area plot
area_wetloss_plot <-
  ggplot() +
  geom_area(data=subset(globsums, var %in% c("tot_remwet_Mkm2","tot_wetloss_Mkm2","tot_convtorice_Mkm2")),
            aes(x=year, y=dat, fill=var)) +
  
  geom_line(data=subset(globsums, var=='tot_crop_Mkm2'), 
            aes(x=year, y=dat), color='black', size=1) +
  
  geom_line(data=subset(globsums, var=='tot_ir_rice_Mkm2'), 
            aes(x=year, y=dat), size=1 , color='pink') +  
  
  xlab("Year") +  ylab("Area (Mkm2)") + 
  scale_x_continuous(expand=c(0,0), breaks=mybreaks, labels=mybreaks)+#, limits = c(-6000, max(mybreaks))) +   
  scale_y_continuous(expand=c(0,0)) +
  scale_colour_manual(name = 'Human Land Cover', 
                      values =c('black'='black','pink'='pink'), labels = c('cropland','irrigated rice')) +
  
  facet_wrap(~name) +
  line_plot_theme#, scales="free") 

area_wetloss_plot





# make area plot
wetloss_plot <-
  ggplot() +
  # geom_area(data=subset(globsums, var %in% c("tot_remwet_Mkm2","tot_wetloss_Mkm2","tot_convtorice_Mkm2")),
  #           aes(x=year, y=dat, fill=var)) +
  
  geom_line(data=subset(globsums, var=='remwet_perc_from6000BC'), 
            aes(x=year, y=dat, color=name), size=1) +
  
  # geom_line(data=subset(globsums, var=='tot_ir_rice_Mkm2'), 
  #           aes(x=year, y=dat), size=1 , color='pink') +  
  
  xlab("Year") +  ylab("Area (Mkm2)") + 
  scale_x_continuous(expand=c(0,0), breaks=mybreaks, labels=mybreaks) +   
  scale_y_continuous(expand=c(0,0), limits = c(0, 1)) +
  # scale_colour_manual(name = 'Human Land Cover', 
  #                     values =c('black'='black','pink'='pink'), labels = c('cropland','irrigated rice')) +
  
  #facet_wrap(~name) +
  line_plot_theme#, scales="free") 
wetloss_plot
