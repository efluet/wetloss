

# Group the cases by time period
cs_peat_joined <- cs_peat_joined %>%
  mutate(period = paste0(yr_start_rnd, "-", yr_end_rnd)) %>%
  mutate(period = ifelse(yr_start_rnd >= 1900, "20th century", period)) %>%
  mutate(period = ifelse(period %in%  c("1700-1980", "1780-1980", "1790-1980", "1800-1980"), "~1780-1980", period)) %>%
  mutate(period = ifelse(period %in%  c("1880-1990", "1870-1960", "1810-1870"), "19th to 20th century", period))  %>%
  mutate(label = as.character(region)) %>% 
  mutate(label = ifelse(is.na(label), country, label)) %>% 
  mutate(perc_change_numeric = abs(perc_change_numeric)) %>%
  mutate(continent_period = paste0(continent, ' (', period, ')'))  
  


# Remove NAs
  cs_peat_joined <- cs_peat_joined %>%   filter(!is.na(map_perc_lost) & !is.na(perc_change_numeric))




#---------------------------
fig3_peat_scatter <- 
  
  ggplot() +
  
  # make 1:1 line
  geom_abline(slope=1, intercept=0, color='grey85', size=0.3) +
  
  # points labeled by continent/period
  geom_point(data=subset(cs_peat_joined, cs_type=='peat'),
             aes(x=perc_change_numeric, 
                 y=map_perc_lost, 
                 fill=continent, 
                 size=remwet_end/1000),  # areapoly_mkm2
             shape=21, color='black', stroke=0.35, alpha=0.65) +
  
  # geom_text(aes(x=0, y=95), label='Radj=0.65 \np<0.0001\nn=122', size=2.5, hjust=0) +
  
  # histcase label
  geom_text_repel(data = subset(cs_peat_joined, cs_type=='peat'),
                  aes(x=perc_change_numeric,
                      y=map_perc_lost,
                      label = region),
                  color='grey30',
                  segment.color='grey30',
                  size = 2.0,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +
  
  # axis labels
  xlab("Regional estimates of wetland loss (%)") + 
  ylab("Reconstructed wetland loss (%)\n over peat regions (>5% peatland cover)") +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100)) + 
  scale_y_continuous(limits=c(0, 100)) + 
  scale_shape_manual(values=c(21)) +
  scale_size(range=c(0.8, 16)) +
  scale_fill_brewer(palette = "Set1")+
  # guides(fill=guide_legend(ncol=2))+
  
  # fixed axis ratio
  coord_fixed() +
  line_plot_theme +
  theme(panel.background = element_rect(color="black", size=0.5, fill=NA),
        legend.position = 'top',#c(0 , 1.3), # c(0.05, 0.9),#"top",
        legend.direction = "horizontal",
        plot.margin=unit(c(1, 1, 3, 1), 'mm'))


  
fig3_peat_scatter



ggsave('../output/figures/hist_cases/scatterplot_vs_peatregion_loss_v1.png', fig3_peat_scatter,
       width=120, height=120, dpi=600, units='mm', type = 'cairo-png')
