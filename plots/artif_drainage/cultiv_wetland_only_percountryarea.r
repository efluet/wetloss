
# plot timline of --------------------------------------------------------------
ggplot(cultwet) +
  geom_point(aes(x=year, y=drained_area_irrig, color=type)) +
  geom_line(aes(x=year, y=drained_area_irrig, color= type)) + 
  facet_wrap(~country_name, scales= "free") + 
  line_plot_theme



### save plot ------------------------------------------------------------------
ggsave(
  "./output/figures/artif_drainage/cultivated_wetland_aquastat.png",
  dpi=300, width=180, height=80, units='mm' , type = "cairo-png")

dev.off()

