
# /----------------------------------------------------------------------------#
#/     Plot facets
predall <- read.csv("./output/results/artif_drainage/drained_wetcult_ha_sigmoidpred.csv")


m <-  ggplot() +
  geom_line(data = predall, aes(x= year, y= pred_drained, color=type)) +
  geom_point(data= d,      aes(x= year, y= drained_area_tot, color=type)) +
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free") +
  line_plot_theme +
  theme(legend.position = c(0.9, 0.03)) +
  ylab("Area drained (km^2)") + xlab("")

### save plot
ggsave(plot=m, "./output/figures/artif_drainage/sigmoid/all/drain_sigmoid_predall_maxed.png",
       width=16.5, height=12.5, dpi=300, units='in' , type = "cairo-png")

dev.off()



# # /----------------------------------------------------------------------------#
# #/   Make plot that stacks all curves 
# predall <- predall %>%
#   group_by(country_name, type) %>%
#   mutate(max_pred_drained = max(pred_drained)) %>% 
#   mutate(perc_drained_ofmax = pred_drained/max_pred_drained) %>%
#   ungroup()
# 
# 
# # /----------------------------------------------------------------------------#
# #/     Plot facets
# m <- ggplot() +
#   geom_line(data = predall, aes(x= year, y= perc_drained_ofmax, group=country_name, color=continent)) +
#   expand_limits(y=0) +
#   facet_grid(continent~type, scales="free", space="free") +
#   line_plot_theme +
#   theme(legend.position = "none") + # c(0.9, 0.03)
#   ylab("Area drained (km^2)") + xlab("")
