
### Barplot of value types in the time-series   -------------------------------

#  Read in data
die <- read.csv("./output/results/artif_drainage/drained_wetcult_ha_sigmoidpred.csv",
                stringsAsFactors = T)

# prep for plot
die_forplot <- die %>%
               group_by(type, year, continent) %>%
               dplyr::summarize(pred_drained = sum(pred_drained)) %>%
               ungroup() %>%
               mutate(type= stri_trans_totitle(type))
  

# /-----------------------------------------------------------------------------
#/     Make the plot 
barplot_baltype <- 
  
  ggplot(die_forplot) +
  
  geom_bar(aes(x=year, y=pred_drained/1000, fill=continent), 
           color='white', size=0.1,
           position = "stack", stat="identity") +
  
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0), labels = seq(1700,2000,50), breaks= seq(1700,2000,50)) +
  
  
  ylab(expression(paste("Drained area (10"^{3},' km'^{2}," )" ))) +
  xlab("") +
  
  facet_wrap(~type, scales="free_y") +
  line_plot_theme +
  
  theme(legend.position = c(0.02, 0.8)) 

barplot_baltype



# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(plot=barplot_baltype, 
       "./output/figures/artif_drainage/barplot_draintype.png",
       dpi=300, width=180, height=100, units='mm' , type = "cairo-png")

dev.off()






# value types:  data, interpolated, extrapolated

# convert to factor
#die$valtype <- factor(die$valtype, levels = c("extrapol","interp","data"))
