# make facet plot of all LU drivers after mapping
p_i=1 ; s_i=4

df <- data.frame()
lul <- c('cropland', 'forestry', 'peatextr', 'pasture', 'urban', 'ir_rice', 'wetcultiv')

for (l in lul){
  
  df_lu <- read.csv(paste0('../output/results/wetloss/grid/grid_drain_perlu/grid_drain_', l, '_s', s_i, '_p', p_i, '_t', test_theta, '_', pars,'_v1.csv')) %>%
    bind_cols(., ciso_df) %>%
    dplyr::select(-X, -x...35, -y...36, -x...37, -y...38, -nat_id, -iso_a3) %>%
    group_by(country_name) %>%
    summarise_all(sum, na.rm=T) %>%
    mutate(lu_type = l)
  
  df <- bind_rows(df, df_lu)

}



df <- df %>% pivot_longer(cols=X1700:X2020, names_to='year', values_to='drain_area') %>%
      mutate(year = as.numeric(substring(year, 2,5))) %>%
      mutate(lu_type= stri_trans_totitle(lu_type)) %>% 
      mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
             lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
             lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
             lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
             lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type)) %>%
      filter(drain_area > 0)



# /----------------------------------------------------------------------------#
#/  SPLIT COUNTRIES INTO MULTIPLE GROUPS FOR PLOTTING
q <-sort(unique(c(df$country_name, df$country_name)))
q <- split(q, ceiling(seq_along(q)/36))


for(i in 1:length(q)){
  
  print(i)
  
  # Subset 
  # drainage_wetcult_s <- df %>% filter(country_name %in% unlist(q[i]))
  df_temp <-   df %>% filter(country_name %in% unlist(q[i]))
  
  
  # /----------------------------------------------------------------------------#
  #/  Make lineplot 
  m <- 
    ggplot(df_temp) +
    
    # Interpolated line
    geom_line(aes(x=year, y=drain_area, color=lu_type, group=lu_type), size=0.7, alpha=0.9) +
    
    # Data points
    # geom_point(data=d_wc_forplot_s, aes(x=year, y=drained_area_tot/1000, color=type), size=.55) +  #  shape=21, fill='white') +
    
    facet_wrap(~country_name, scales='free', ncol=9, nrow=4) +
    
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
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          axis.ticks = element_line(colour='black'),
          plot.margin=unit(c(1, 1, 1, 1), 'mm'),
          panel.spacing = unit(1, 'mm'),
          strip.text = element_text(size=7, hjust= 0, vjust = -1),
          legend.box.background = element_rect(color = "black", fill=NA, size=0.3))
  
  
  # /-----------------------------------------------------------------------------
  #/ save plot 
  ggsave(plot = m, paste0('../output/figures/timeline/SI_country_perlu_wetlos_p', i,'.png'),
         width=310, height=200, dpi=600, units='mm')
  
}

