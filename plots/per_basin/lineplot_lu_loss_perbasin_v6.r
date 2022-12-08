# /----------------------------------------------------------------------------#
#/   Get peatland regions                                          --------
source('./data_proc/wettype/regionalize_peatmap.r')

# /----------------------------------------------------------------------------#
#/  PREP RIVER BASINS                                             ---------
source('./data_proc/wettype/prep_riv_basin.r')


# /----------------------------------------------------------------------------#
#/   Append regions to the remwet data                            ---------

# read remwet
s_i=4 ; p_i=1 ; pars='avg'
f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta, '_', pars,'_v1.csv')
remwet <- read.csv(f)

# Append columns of regions to remwet data
remwet_wettype <- bind_cols(remwet, peatmap_df, rivbasin_df, ciso_df)


# /----------------------------------------------------------------------------#
#/   Summarize per basin                                          --------

remwet_perbasin <- 
        remwet_wettype %>% 
        filter(!is.na(basin_name)) %>% 
        group_by(basin_name) %>% 
        dplyr::select(basin_name, X1700:X2020) %>% 
        summarize_all(.funs=sum, na.rm=T) %>% 
        ungroup() %>%
        pivot_longer(X1700:X2020, names_to='year', values_to='remwet_km2') %>%
        mutate(year=as.numeric(substring(year,2,5))) %>% 
        group_by(basin_name) %>% 
        mutate(remwet_perc = 1 - remwet_km2/max(remwet_km2))


# Filter to only major basins 
# TODO: should use a specific criteria 
remwet_perbasin <- remwet_perbasin %>% 
                  filter(basin_name %in% c('Yangtze','Indus','Mississippi', 'Niger', 'Rio Grande (US)',
                                           'Murray','Nile','St. Lawrence','Mekong','Danube', 'Mekong', 'Yellow',
                                           'Tigris & Euphrates', 'Syr-Darya','Ganges','Amazon','Congo'))


unique(remwet_perbasin$basin_name)

# /----------------------------------------------------------------------------#
#/   Summarize per peatland region                                -------
remwet_perpeat <- 
        remwet_wettype %>% 
        filter(!is.na(peat_name)) %>% 
        group_by(peat_name) %>% 
        dplyr::select(peat_name, X1700:X2020) %>% 
        summarize_all(.funs=sum, na.rm=T) %>% 
        ungroup() %>%
        pivot_longer(X1700:X2020, names_to='year', values_to='remwet_km2') %>%
        mutate(year=as.numeric(substring(year,2,5))) %>% 
        group_by(peat_name) %>% 
        mutate(remwet_perc = 1 - remwet_km2/max(remwet_km2))
        # mutate(remwet_perc = 1 - remwet_km2/max(remwet_km2))


# /----------------------------------------------------------------------------#
#/   Summarize per country                                -------
remwet_percountry <- 
      remwet_wettype %>% 
      filter(!is.na(country_name)) %>% 
      group_by(country_name) %>% 
      dplyr::select(country_name, X1700:X2020) %>% 
      summarize_all(.funs=sum, na.rm=T) %>% 
      ungroup() %>%
      pivot_longer(X1700:X2020, names_to='year', values_to='remwet_km2') %>%
      mutate(year=as.numeric(substring(year,2,5))) %>% 
      group_by(country_name) %>% 
      mutate(remwet_perc = 1 - remwet_km2/max(remwet_km2))

remwet_percountry <- remwet_percountry %>% 
  filter(country_name %in% c('Russia','United States','Ireland', 'Germany', 'Canada',
                           'China','India','Brazil','Pakistan','Rwanda','Netherlands',
                           'Hungary','Lithuania','Italy','United Kingdom','Australia'))


# /----------------------------------------------------------------------------#
#/  Make peatland remwet perc lineplot                       --------
remwet_perc_perpeat_plot <- 
  ggplot(remwet_perpeat) +
  
  # add lines 
  geom_line(aes(x=year, y=remwet_perc*100, color=peat_name), size=0.35) +
  
  geom_text_repel(data = subset(remwet_perpeat, year==2000),
                  aes(x=year+40, y=remwet_perc*100, label = peat_name, color=peat_name),
                  segment.color='grey25',
                  size = 3.0,
                  nudge_x = 10,
                  segment.size = 0.25,
                  box.padding = unit(0.1, 'mm'),
                  point.padding = unit(0.1, 'mm')) +
  # axes limit
  xlab("")  + ylab("") + # ylab("Wetland area percentage change since 1700 (%)") +
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2120)) +
  scale_y_reverse() + # limits=c(0, 100), expand=c(0,0), breaks=seq(0, 100, 10)) +
    #limits=c(0, 100), expand=c(0,0), breaks=seq(0, 100, 10)) +  #  breaks=pretty_breaks(6),
  scale_colour_hue(c = 220, l = 55) +
  
  ggtitle('Peatland regions')+
  line_plot_theme +
  theme(legend.position = 'none')

remwet_perc_perpeat_plot


# /----------------------------------------------------------------------------#
#/  Make river basins                                                   --------

remwet_perc_perbasin_plot <- 

  ggplot(remwet_perbasin) +
  
  # add lines 
  geom_line(aes(x=year, y=remwet_perc*100, color=basin_name), size=0.35) +
  
  # make multiple facets per lat slices
  # facet_wrap(~type, nrow=1, scales="free_y") +
  xlab("")  + ylab("") +
  
  geom_text_repel(data = subset(remwet_perbasin, year==2020),
                  aes(x=year+40, y=remwet_perc*100, label = basin_name, color=basin_name),
                  segment.color='grey25',
                  size = 3.0,
                  nudge_x = 10,
                  segment.size = 0.25,
                  box.padding = unit(0.1, 'mm'),
                  point.padding = unit(0.1, 'mm')) +
  # axes limit
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2120)) +
  # scale_y_continuous(limits=c(0, 100), expand=c(0,0), breaks=seq(0, 100, 10))+  #
  scale_y_reverse(limits=c(100, 0), expand=c(0,0), breaks=seq(100, 0, -10))+  #
  scale_colour_hue(c = 220, l = 55) +
  
  ggtitle('River basins')+
  line_plot_theme +
  theme(legend.position = 'none')


remwet_perc_perbasin_plot



# /----------------------------------------------------------------------------#
#/  Make Country remwet perc lineplot                       --------
remwet_perc_percountry_plot <- 
  
  ggplot(remwet_percountry) +
  
  # add lines 
  geom_line(aes(x=year, y=remwet_perc*100, color=country_name), size=0.35) +
  
  geom_text_repel(data = subset(remwet_percountry, year==2020),
                  aes(x=year+40, y=remwet_perc*100, label = country_name, color=country_name),
                  segment.color='grey25',
                  size = 3.0,
                  nudge_x = 10,
                  segment.size = 0.25,
                  box.padding = unit(0.1, 'mm'),
                  point.padding = unit(0.1, 'mm')) +
  # axes limit
  xlab("")  + ylab("Wetland area percentage change since 1700 (%)") +
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2120)) +
  scale_y_reverse(limits=c(100, 0), expand=c(0,0), breaks=seq(100, 0, -10))+  #
  # scale_y_continuous(limits=c(0, 100), expand=c(0,0), breaks=seq(0, 100, 10))+ # breaks=pretty_breaks(5), 
  scale_colour_hue(c = 220, l = 55) + 
  
  ggtitle('Countries')+
  line_plot_theme +
  theme(legend.position = 'none')


remwet_perc_percountry_plot




# /----------------------------------------------------------------------------#
#/  Arrange plots grob into layout 

p <- plot_grid(remwet_perc_percountry_plot, 
               remwet_perc_perpeat_plot, 
               remwet_perc_perbasin_plot,
               ncol=3, nrow=1, 
               rel_heights = c(1, 1, 1),
               rel_widths = c(1, 1, 1),
               # labels = c('A','B','C'),
               align='hv')

# SAVE
ggsave('../output/figures/wettype/v6/remwet_perc_perpeatbasin_2021_v6.png', p,
       width=190, height=100, dpi=300, units='mm') #type = 'cairo-png')

ggsave('../output/figures/wettype/v6/remwet_perc_perpeatbasin_2021_v6.pdf', p,
       width=190, height=100, dpi=300, units='mm')



dev.off()




