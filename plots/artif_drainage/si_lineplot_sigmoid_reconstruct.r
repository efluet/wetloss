# /-----------------------------------------------------------------------------
#/  Get wetcult data & reconstruction

drainage_wetcult <- 
  read.csv('../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_march2021.csv') %>% as_tibble() %>% 
  group_by(country_name, type) %>% 
  mutate(max_pred_drained = max(pred_drained, na.rm=T)) %>%
  ungroup() %>% 
  mutate(type = str_to_title(type)) %>% 
  filter(pred_drained != 0)


# Get data points for Wetland cultiv 
# wc <- read.csv('../output/results/artif_drainage/drained_wetcult_km2_onlydata.csv') %>% as_tibble() %>% 
wc <- read.csv('../output/results/artif_drainage/old/drained_wetcult_km2_onlydata.csv') %>% as_tibble() %>% 
  rename(drained_area_tot = drained_area_irrig) %>% 
  mutate(type = 'Wetland Cultiv.') %>% 
  mutate(type = str_to_title(type))


# Get datapoints for drainage (highly processed)
d <- read.csv('../output/results/artif_drainage/drained_data_fullproc_forfig1.csv', stringsAsFactors=F)
# Filter to only keep national data
d_nat <- d %>% filter(region=='')


# /----------------------------------------------------------------------------#
#/  Bind point and line data 
# Bind data points from drainage data and wetcultiv
d_wc_forplot <- bind_rows(d_nat, wc)

# Bind data points to time-series df
# d_wc_forplot <- semi_join(d_wc_forplot, drainage_wetcult, by=c('country_name'='country_name', 'type'='type'))
d_wc_forplot <- as_tibble(d_wc_forplot) %>% 
                filter(!is.na(drained_area_tot)) %>% 
                mutate(country_name=ifelse(country_name=='United Republic of Tanzania','Tanzania',country_name))


# /----------------------------------------------------------------------------#
#/  SPLIT COUNTRIES INTO MULTIPLE GROUPS FOR PLOTTING
q <-sort(unique(c(d_wc_forplot$country_name, drainage_wetcult$country_name)))
q <- split(q, ceiling(seq_along(q)/40))


for(i in 1:length(q)){
  
  print(i)
  
  # Subset 
  drainage_wetcult_s <- drainage_wetcult %>% filter(country_name %in% unlist(q[i]))
  d_wc_forplot_s <-   d_wc_forplot %>% filter(country_name %in% unlist(q[i]))
  
  
  # /----------------------------------------------------------------------------#
  #/  Make lineplot 
  m <- 
    ggplot() +
    
    # Interpolated line
    geom_line(data=drainage_wetcult_s, aes(x=year, y=pred_drained/1000, color=type, group=type), size=0.3) +
    
    # Data points
    geom_point(data=d_wc_forplot_s, aes(x=year, y=drained_area_tot/1000, color=type), size=.55) +  #  shape=21, fill='white') +
    
    facet_wrap(~country_name, scales='free', ncol=8) +
    
    expand_limits(y=0) +
    scale_x_continuous(limits = c(1700, 2080), expand=c(0,1)) +
    scale_y_continuous(expand=c(0.01,0), breaks=pretty_breaks()) +
    
    scale_color_manual(
      values =
        c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
          'Wetland Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
          'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
          'Peat Extraction' = '#ff7f00',#'brown',    # Peatland
          'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
          'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
          'Urban'    = '#a65628'),  #e0dd6e'), # Urban
      name="Driver of\nwetland loss",) +

    
    xlab('') + ylab(expression(paste('Cumulative area drained or converted (10'^{3},' km'^{2},')')))  +
    
    line_plot_theme +
    # guides(color = guide_legend(title.position = "bottom")) +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          axis.ticks = element_line(colour='black'),
          plot.margin=unit(c(1, 1, 1, 1), 'mm'),
          panel.spacing = unit(1, 'mm'),
          strip.text = element_text(hjust= 0, vjust = -1),
          legend.box.background = element_rect(color = "black", fill=NA, size=0.3))
          # legend.key = element_rect(fill='black'))
  
  
  # /-----------------------------------------------------------------------------
  #/ save plot 
  ggsave(plot = m, paste0('../output/figures/artif_drainage/sigmoid/drainstats_interpol_facet2021_p', i,'.png'),
         width=310, height=200, dpi=600, units='mm')
  
}
