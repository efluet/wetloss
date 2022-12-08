
die_subset <- die[1:7440,]


## plot for in between years ---------------------------------------------------
drained_area_inter_n_extrapol_plot <- ggplot() +
  
  
  ## Cropland
  geom_point(data=subset(die, type=="cropland" & valtype == "data"), aes(x=year, y=ts_drained_area_tot, shape=valtype), color='blue', size=1.8) +
  #geom_line(data=subset(die_subset, type=="cropland"), aes(x=year, y=ts_drained_area_tot), color='blue',size=0.8, alpha=0.3, line="dash") +
  geom_line(data=subset(die, type=="cropland"), aes(x=year, y=  ts_drained_area_tot_templateapplied), color='blue', size=0.8, alpha=0.5) +
  
  
  ## 
  geom_point(data=subset(die, type=="forestry"), aes(x=year, y=ts_drained_area_tot), color='green', size=0.4) +
  geom_line(data=subset(die, type=="forestry"), aes(x=year, y=ts_drained_area_tot_templateapplied), color='green',size=0.4, alpha=0.3) +

  geom_point(data=subset(die, type=="peatland"), aes(x=year, y=ts_drained_area_tot), color='brown', size=0.4) +
  geom_line(data=subset(die, type=="peatland"), aes(x=year, y=ts_drained_area_tot_templateapplied), color='brown',size=0.4, alpha=0.3) +
  
  #geom_bar(data=a, (aes=))
  scale_x_continuous(limits=c(1700, 2020)) +
  scale_shape(solid = FALSE) +
  
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free") +
  #facet_grid(type~continent, scales="free") +
  line_plot_theme +
  theme(legend.position = c(0.8, 0.1)) +
  ylab("Area drained (1000 ha)") + xlab("")

drained_area_inter_n_extrapol_plot


### save plot
ggsave(plot=drained_area_inter_n_extrapol_plot, 
       "./output/figures/artif_drainage/artif_drainage_nat_interp_n_extra_area_v2.png",
       dpi=300, width=550, height=300, units='mm' , type = "cairo-png")

dev.off()
