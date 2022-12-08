# percentage drain



# linplot percentage by type   -------------------------------------------------
drained_plot <- ggplot() +
  
  geom_point(data=subset(d, type=="cropland"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  geom_line(data=subset(d, type=="cropland"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  
  geom_point(data=subset(d, type=="forestry"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  geom_line(data=subset(d, type=="forestry"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  
  geom_point(data=subset(d, type=="peatland"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  geom_line(data=subset(d, type=="peatland"), aes(x=year, y=f_drained, color=country_name), size=0.4) +
  
  #geom_hline(yintercept = 0, color="black") +
  
  
  geom_text_repel(data = subset(d, year == max_year),
                  aes(x=max_year, y=f_drained, label = country_name, color = country_name),
                  size = 2.5,
                  nudge_x = 10,
                  segment.color = NA,
                  box.padding = unit(0.25, 'mm')) + 
  
  expand_limits(y=0) +
  facet_wrap(~type, scales="free", nrow=1) +
  line_plot_theme +
  theme(legend.position = "none") + #c(0.8, 0.1)) +
  ylab("Percentage area drained (%)") + xlab("") +
  theme(panel.spacing = unit(2, "lines"))



### save plot ------------------------------------------------------------------
ggsave(plot=drained_perc_plot, "./output/figures/artif_drainage/artif_drainage_nat_percentage.png",
       dpi=300, width=180, height=140, units='mm' , type = "cairo-png")

dev.off()





# time plot ------------------------------------------------------
tot_drain_plot <- ggplot() +
  
  geom_bar(data=d, aes(x=decade, y=drained_area_tot/100/1000, fill=type), stat="identity", position="stack") +
  line_plot_theme +
  theme(legend.position = c(0.2, 0.8)) +
  ylab("Area drained (Mkm^2)") +
  xlab("")



tot_drain_plot







# # make plot --------------------------------------------------------------------
# drained_plot <- ggplot(drained_wfrac) +
#   
#   geom_point(aes(x=year, y=perc_ag_drained, color=country_name), size=0.5) +
#   geom_line(aes(x=year, y=perc_ag_drained, color=country_name), size=0.4) +
#   
#   geom_text_repel(data = subset(drained, year == max_year),
#                   aes(x=max_year, y=perc_ag_drained, label = country_name, color = country_name),
#                   size = 2.5,
#                   nudge_x = 3,
#                   segment.color = NA,
#                   box.padding = unit(0.2, 'mm')) + 
#   
#   
#   expand_limits(y=0) +
#   scale_x_continuous(limits=c(NA, 2025)) +
#   
#   facet_wrap(~type, scales="free", ncol=1) +
#   line_plot_theme +
#   theme(legend.position = "none",
#         plot.margin = unit(c(1, 12, -1, 1), "mm")) +
#   ylab("Percentage of cropland drained (%)") + xlab("")
# 
# 
# drained_plot
# 
# 
# 
# ### save plot
# ggsave(plot=drained_plot, "./output/figures/artif_drainage/artif_drainage_nat_perc.png",
#        dpi=300, width=179, height=500, units='mm' , type = "cairo-png")
# 
# dev.off()
# 
# 
# 
# #==============================================================================#
# # prep data for plot -----------------------------------------------------------
# #==============================================================================#
# 
# # for labelling
# drained <- drained %>%
#   group_by(type, country_name) %>%
#   mutate(max_year = max(year)) %>% 
#   ungroup()
# 
# 
# #==============================================================================#
# # line plot --------------------------------------------------------------------
# #==============================================================================#
# drained_plot <- ggplot() +
#   
#   geom_point(data=subset(drained, type=="agriculture"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   geom_line(data=subset(drained, type=="agriculture"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   
#   geom_point(data=subset(drained, type=="forestry"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   geom_line(data=subset(drained, type=="forestry"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   
#   geom_point(data=subset(drained, type=="peatland"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   geom_line(data=subset(drained, type=="peatland"), aes(x=year, y=drained_area_tot, color=country_name), size=0.4) +
#   
#   geom_hline(yintercept = 0, color="black") +
#   
#   
#   geom_text_repel(data = subset(drained, year == max_year),
#                   aes(x=max_year, y=drained_area_tot, label = country_name, color = country_name),
#                   size = 2.5,
#                   nudge_x = 10,
#                   segment.color = NA,
#                   box.padding = unit(0.5, 'mm')) + 
#   
#   
#   
#   
#   #expand_limits(y=0) +
#   facet_grid(type~continent, scales="free") +
#   #line_plot_theme +
#   theme(legend.position = c(0.8, 0.1)) +
#   ylab("Area drained (1000 ha)") + xlab("")
# 
# 
# ### save plot ------------------------------------------------------------------
# ggsave(plot=drained_plot, "./output/figures/artif_drainage/artif_drainage_nat_v3.png",
#        dpi=300, width=550, height=300, units='mm' , type = "cairo-png")
# 
# dev.off()
# 
# 
# 
# 
# 
# # time plot ------------------------------------------------------
# tot_drain_plot <- ggplot() +
#   
#   geom_bar(data=drained, aes(x=decade, y=drained_area_tot/100/1000, fill=type), stat="identity", position="stack") +
#   line_plot_theme +
#   theme(legend.position = c(0.2, 0.8)) +
#   ylab("Area drained (Mkm^2)") +
#   xlab("")
