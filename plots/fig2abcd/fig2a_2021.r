

# Get the min/max uncertainty range from different configurations
source('data_proc/fit/calc_sum_remwet_ci_range.r')


# /----------------------------------------------------------------------------#
#/   Get current simul run, for black line in plot          -------
library(lemon)
# Get line of median model run
f <- paste0('../output/results/wetloss/sum/sum_remwet_s', s_i, '_p', p_i,'_t', test_theta,'_', pars, '_v1.csv')
remwet_sum <- read.csv(f)


# /----------------------------------------------------------------------------#
#/   Make remwet lineplot                                             -------

fig2a <- 
  
  ggplot(remwet_sum) +
  
  # Error band of full theta parameter min/max
  geom_ribbon(data=remwet_sum_minmax_range,
              aes(x=year, ymin= remwet_perc_min, ymax= remwet_perc_max, fill='Parameter fit uncertainty')) +
              # fill='grey92') +

  # # Error band 
  geom_ribbon(data= remwet_sum_mean_range,
              aes(x=year, ymin= remwet_perc_min, ymax= remwet_perc_max, fill='Wetland area uncertainty')) +
              # fill='grey65') +
  
  # plot nat wet cover line
  geom_line(data=remwet_sum,
            aes(x=year, y= remwet_perc, color='Best reconstruction'), 
            size=0.4) +
  
  # axis labels
  xlab('Year') +  
  ylab('Percentage wetland remaining from 1700 (%)') +
  
  # axis scales
  scale_x_continuous(expand=c(0,0), labels = seq(1700,2020,100), breaks=seq(1700,2020,100)) +
  scale_y_continuous(expand=c(0,0), limits=c(50, 100), labels=seq(0, 100, 10), breaks=seq(0,100,10)) +
  
  scale_color_manual(name = '',
                     breaks = c('Best reconstruction'),
                     values = c('Best reconstruction' = 'black')) +
  scale_fill_manual(name = '',
                     breaks = c('Parameter fit uncertainty', 'Wetland area uncertainty', 'Best reconstruction'),
                     values = c('Parameter fit uncertainty' = 'grey92', 
                                'Wetland area uncertainty' = 'grey70', 
                                'Best reconstruction' = 'black')) +
  line_plot_theme +
  theme(legend.position = c(0.2, 0.3), #'none',
    plot.margin = margin(1,3,1,1,'mm'))

fig2a


# /----------------------------------------------------------------------------#
#/  Fig 2 A - Rem.Wet area lineplot
# 
# remwet_df_allparams <- 
#   read.csv('../output/results/wetloss/global_sum_wetloss_mcmc_paramsrange.csv') %>% 
#   # read.csv('../output/results/wetloss/global_sum_wetloss_mcmc_paramsrange.csv') %>% 
#   dplyr::select(-X, -drain_area_mkm2) %>% 
#   pivot_wider(id_cols = year, names_from = type, values_from = rem_wet_area_mkm2)

