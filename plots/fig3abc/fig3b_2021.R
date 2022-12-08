# Scatterplot of map and historical records
# x: mapped loss
# y: Davidson cases


# If using run without theta, read in diff file
cs_joined <- read.csv(paste0('../output/results/histcases/residuals/cs_joined_s', s_i, '_p', p_i, '_t', test_theta, '_', pars, '_v1.csv'))

# Group the cases by time period
cs_joined <- cs_joined %>% as_tibble() %>% 
  mutate(period = paste0(yr_start_rnd, '-', yr_end_rnd)) %>%
  mutate(period = ifelse(yr_start_rnd >= 1900, '20th century', period)) %>%
  mutate(period = ifelse(period %in%  c('1700-1980', '1780-1980', '1790-1980', '1800-1980'), '~1780-1980', period)) %>%
  mutate(period = ifelse(period %in%  c('1880-1990', '1870-1960', '1810-1870'), '19th to 20th century', period))  %>%
  mutate(label = as.character(region)) %>% 
  mutate(label = ifelse(is.na(label), country, label)) %>% 
  mutate(perc_change_numeric = abs(perc_change_numeric)) %>%
  mutate(continent_period = paste0(continent, ' (', period, ')')) %>% 
  # for plotting bubble size, bump up min polygon size of smallest regions
  mutate(areapoly_mkm2 = ifelse(areapoly_mkm2<0.1, 0.1, areapoly_mkm2))

# Remove NAs
cs_joined <- cs_joined %>% filter(!is.na(map_perc_lost) | !is.na(perc_change_numeric)) %>% 
                           filter(is.na(region) | region != 'Hawaii')


# /----------------------------------------------------------------------------#
#/ Calculate agreement metrics (R^2, RMSE, Bias)
source('./data_proc/overlay/calc_cs_joined_r2.r', local=T)


# /----------------------------------------------------------------------------#
#/     Prep for scatterplot                                            ------

# Make plot label of R^2, nsize, etc.
agg_label <- paste0('Radj=', r2adj_weighted, '\np<', pval, '\nn=', nrow(cs_joined))


# Set a subset of labels to show
labels_ls = c('Illinois', 'Ohio', 'Indiana', 'California', 'Idaho', 'Iowa', 'Arkansas', 'Missouri', 
              'Ohio', 'North Carolina', 'Louisiana', 'Colorado', 'West Virginia',
              'Yangtze & Han', 'Yangtze & Han\nRiver basins','West Songnen Plain', 'Anhui', 'Henan', 'Chongqing',
              'Lake Chad', 'Mesepotamian\nMarshes', 'Hula Swamp',
              'Rhine valley', 'Po delta', 'Rhone delta', 'Kuban Delta (Sea of\nAzov)', 'Cauca River Valley',
              'Great Britain', 'Denmark', 'France', 'Estonia', 
              'Thessaly', 'Peleponnese', 'Macedonia', 'Epirus',
              'Magdalena River delta', 'Castille-La\nMancha regio', 'Lake Urmia', 'Friuli-Venezia\nGiulia',  
              'Najafgarh Lake', 'Lake Amik', 'Danube', 'New Zealand',  'Finland', 'Bulgaria',
              'Xinjiang', 'Tees Estuary','Tunisia','Danube','Magdalena \nRiver delta')



# /----------------------------------------------------------------------------#
#/     Make scatterplot by period                                         ------

fig3_scatter <- ggplot() +
  
  # make 1:1 line
  geom_abline(slope=1, intercept=0, color='grey85', size=0.3) +
  
  # points labeled by continent/period
  geom_point(data=cs_joined,
             aes(x=perc_change_numeric, 
                 y=map_perc_lost, 
                 fill=continent, 
                 size=areapoly_mkm2), #remwet_end/1000),  # areapoly_mkm2
             shape=21, color='black', stroke=0.32, alpha=0.65) +
  
  geom_text(aes(x=0, y=95), size=2.5, hjust=0, label= agg_label) +
  
  # histcase label
  geom_text_repel(data = subset(cs_joined, label %in% labels_ls),
                  aes(x=perc_change_numeric,
                      y=map_perc_lost,
                      color=continent,
                      label = label),
                  # color='grey30',
                  segment.color='grey30',
                  size = 2.0, 
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(1, 'mm'),
                  point.padding = unit(1, 'mm')) +
  
  # axis labels
  xlab('Regional estimates of wetland loss (%)') + 
  ylab('Reconstructed wetland loss from this study (%)') +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(limits=c(0, 100)) + 
  
  scale_shape_manual(values=c(21)) +
  scale_fill_brewer(palette = 'Set1', name = 'Continent') +
  scale_color_brewer(palette = 'Set1', guide='none') +
  
  scale_size_continuous(name = expression(paste('Region area (', 10^{6}, ' ', km^{2}, ')')),
                        breaks = c(0.5, 5, 10, 15),
                        limits = c(0.1, 18),
                        labels = c('0.5', '5', '10', '15'),
                        range = c(0.6, 7)) +  # c(0.3, 12)) +
  
  guides(fill=guide_legend(ncol=2, override.aes = list(size=6), title.position='top'),
         size=guide_legend(ncol=1, title.position='top')) +
  
  # fixed axis ratio
  coord_fixed() +
  line_plot_theme +
  theme(panel.background = element_rect(color='black', size=0.5, fill=NA),
        legend.position = 'top',#c(0 , 1.3), # c(0.05, 0.9),#'top',
        legend.title=element_text(),
        legend.direction = 'horizontal',
        plot.margin=unit(c(10, 1, 3, 1), 'mm'))


fig3_scatter


# # ### save plot    -----------------------------------------------------
# fout <- paste0('../output/figures/hist_cases/', 'scatterplot_wetloss_2021_s', s_i, '_p', p_i, '_t', test_theta, '_', pars,'.png')
# ggsave(fout, fig3_scatter,
#        width=90, height=150, dpi=700, units='mm', type = 'cairo-png')
# 
# fout <- paste0('../output/figures/hist_cases/', 'scatterplot_wetloss_2021_s', s_i, '_p', p_i, '_t', test_theta, '_', pars,'.pdf')
# ggsave(fout, fig3_scatter,
#        width=90, height=150, dpi=700, units='mm')
# 
# dev.off()


# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_serialmcmcdf_v8.csv')
# cs_joined <- cs_joined %>% 
#   pivot_wider(id_cols=c(rec_id, name, yr_start_rnd, yr_end_rnd, perc_change_numeric, areapoly_mkm2, continent), 
#               names_from=param_type, 
#               values_from = map_wetloss_prc_mean)
