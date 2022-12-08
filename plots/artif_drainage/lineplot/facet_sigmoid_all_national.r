# /----------------------------------------------------------------------------#
#/     Plot facets

predall <- read.csv("../output/results/artif_drainage/drained_wetcult_km2_sigmoidpred_march2021.csv")


m <- 
  ggplot() +
  geom_line(data = predall, aes(x= year, y= pred_drained, color=type)) +
  geom_point(data= d_nat, aes(x= year, y= drained_area_tot, color=type), size=0.6) +
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free") +
  line_plot_theme +
  theme(legend.position = c(0.9, 0.03)) +
  ylab("Area drained (km^2)") + xlab("")



# save plot
ggsave(plot=m, 
       filename="../output/figures/artif_drainage/sigmoid/all/drain_sigmoid_predall_maxed_march2021_v2.png",
       width=400, height=280, dpi=300, units='mm') #, type = "cairo")

dev.off()

