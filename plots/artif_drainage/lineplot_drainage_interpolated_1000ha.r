

## plot for in between years ---------------------------------------------------
drained_area_interp_plot <- ggplot() +
  
  geom_point(data=subset(d, type=="cropland"), aes(x=year, y=drained_area_tot), color='blue', size=0.4) +
  geom_line(data=subset(di, type=="cropland"), aes(x=year, y=ts_drained_area_tot), color='blue',size=0.4, alpha=0.3) +
  
  geom_point(data=subset(d, type=="forestry"), aes(x=year, y=drained_area_tot), color='green', size=0.4) +
  geom_line(data=subset(di, type=="forestry"), aes(x=year, y=ts_drained_area_tot), color='green',size=0.4, alpha=0.3) +
  
  geom_point(data=subset(d, type=="peatland"), aes(x=year, y=drained_area_tot), color='brown', size=0.4) +
  geom_line(data=subset(di, type=="peatland"), aes(x=year, y=ts_drained_area_tot), color='brown',size=0.4, alpha=0.3) +
  
  #geom_bar(data=a, (aes=))
  scale_x_continuous(limits=c(1900, 2010)) +
  
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free") +
  #facet_grid(type~continent, scales="free") +
  line_plot_theme +
  theme(legend.position = c(0.8, 0.1)) +
  ylab("Area drained (1000 ha)") + xlab("")


### save plot
ggsave(plot=drained_area_interp_plot, 
       "./output/figures/artif_drainage/artif_drainage_nat_interp_area_v2.png",
       dpi=300, width=550, height=300, units='mm' , type = "cairo-png")

dev.off()