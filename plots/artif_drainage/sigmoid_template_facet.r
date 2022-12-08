# /----------------------------------------------------------------------------#
#/     Plot national templates                                      ------------

#/  Read the fit metrics to add as text to plot
predtemplates <- read.csv('../output/results/artif_drainage/drained_sig_predtemplates_v4_regions.csv')

# Filter to keep only national reconstructions
predtemplates_nat <- predtemplates %>% filter(is.na(region) | region == '')

# Exclude peat extraction
predtemplates_nat <- 
    predtemplates_nat %>% 
    filter(type != 'Peat Extraction') %>% 
    mutate(label = paste0('pseudo R^2=', round(pseudo.R.squared, 2)))

# FILTER DATAPOINTS USING SEMI_JOIN
d_forplot <- semi_join(d, predtemplates_nat, by=c('country_name'='country_name', 'type'='type')) %>% #, 'region'='region'))
  filter(is.na(region) | region == '')




# /----------------------------------------------------------------------------#
#/   Make plot                                                          --------
m <- ggplot() +
  
  # Add data points
  geom_point(data=d_forplot,
             aes(x= year, y= drained_area_tot, color=type, group=region), size=0.7) +
  
  # Add interpolated line
  geom_path(data=predtemplates_nat, aes(x= year, y= pred_drained, color=type)) +
  
  # Add text 
  geom_label(data=subset(predtemplates_nat, year==2020), aes(x=1800, y=500, label=label), 
             fill=NA, label.size = NA, size=1.75) +
  
  expand_limits(y=0) +
  facet_wrap(~country_name, scales='free', ncol=5) +
  scale_x_continuous(expand=c(0,0))+
  
  line_plot_theme +
  # theme(legend.position = c(0.9, 0.03)) +
  theme(legend.position = 'bottom', 
        legend.direction='horizontal') +
  
  
  scale_color_manual(#labels = driver_names,
    values =
      c('Cropland' = '#CC3311',
        'Wetland Cultiv.' = '#33BBEE',
        'Forestry'   = '#228833',
        'Peat Extraction' = '#EE7733',
        'Rice'     = '#AA4499',
        'Pasture'  = '#DDCC77',
        'Urban'    = '#332288'),
    name="Driver of\nwetland loss") +
  ylab('Area drained (km^2)') + xlab('')


# /----------------------------------------------------------------------------#
#/ Save plot 

# Save PNG
ggsave(plot=m, '../output/figures/artif_drainage/sigmoid/template/drain_sigmoidtemplates_nat_v4.png',
       width=190, height=155, dpi=400, units='mm' , type = 'cairo-png')

# Save PDF
ggsave(plot=m, '../output/figures/artif_drainage/sigmoid/template/drain_sigmoidtemplates_nat_v4.pdf',
       width=190, height=155, dpi=400, units='mm')

dev.off()





# /----------------------------------------------------------------------------#
#/     Plot sub-national                                             -----------
#  NOT WORKING - NOV 2021

if(0){
  
  # Filter to keep only national reconstructions
  predtemplates_subnat <- predtemplates %>% filter(region != '')
  
  # FILTER DATAPOINTS USING SEMI_JOIN
  d_forplot <- semi_join(d, predtemplates_subnat, by=c('country_name'='country_name', 'type'='type', 'region'='region'))
  
  
  m <-  ggplot() +
    geom_point(data=d_forplot,
               aes(x= year, y= drained_area_tot, color=type, group=region), size=0.7) +
    
    geom_path(data=predtemplates_subnat, aes(x= year, y= pred_drained, color=type)) +
    
    expand_limits(y=0) +
    facet_wrap(~region, scales='free', ncol=4) +
    
    line_plot_theme +
    theme(legend.position = 'bottom', 
          legend.direction='horizontal') + # c(0.9, 0.03)) +
    ylab('Area drained (km^2)') + xlab('')
  
  
  ### save plot ------------------------------------------------------------------
  ggsave(plot=m, '../output/figures/artif_drainage/sigmoid/template/drain_sigmoidtemplates_regions3_us_v3.png',
         width=6.5, height=8.5, dpi=400, units='in' , type = 'cairo-png')
  
  dev.off()
}
