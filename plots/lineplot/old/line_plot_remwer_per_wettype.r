


wetloss_pertype <- read.csv('./output/results/wettype/remwet_perc_wettype_since1700.csv')




# plot area ====================================================================
#wettype_perc_bylat <- 
ggplot(wetloss_pertype) +
  
  # add lines 
  geom_line(aes(x=year,y=perc_remwet, color=Name), size=0.35) +
  
  # make multiple facets per lat slices
  facet_wrap(~type, nrow=1, scales="free_y") +
  xlab("")  + ylab("Wetland area percentage change since 1700 (%))") +
  
  
  # axes limit
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), limits=c(15, 100))+
  #scale_color_brewer(name="Wetland area (% of 1700 area)", palette = "Set1")+
  
  
  line_plot_theme +
  theme(legend.position = c(0.1, 0.3))



# save to file ------------------------------------------------

# save figure to file
ggsave('./output/figures/wettype_remwet_perc_v2.pdf', 
       width=178, height=80, dpi=600, units="mm")

### Save figure to file
ggsave('./output/figures/wettype_remwet_perc_v2.png', 
       width=178, height=80, dpi=600, units="mm", type = "cairo-png")

dev.off()
