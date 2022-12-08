
# /----------------------------------------------------------------------------#
#/   COMPILE REMWET ACROSS MODEL RUNS                                      ---------

pars = 'avg'
remwet_sum_mean_range <- data.frame()

p_names=c('WAD2M','GLWD','GIEMSv2')
s_names=c('DLEM','ORCHIDEE','SDGVM','LPJ-wsl')





# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste0('s', s_i, ' p', p_i))

  
    f <- paste0('../output/results/wetloss/sum/sum_remwet_s', s_i, '_p', p_i, '_t', test_theta, '_', pars, '.csv')
    remwet_sum <- read.csv(f) %>% mutate(s_name=s_names[s_i], p_name=p_names[p_i])
    remwet_sum_mean_range <- bind_rows(remwet_sum_mean_range, remwet_sum) %>%  as_tibble()
  }
}



# /----------------------------------------------------------------------------#
#/   Make remwet lineplot                                             -------

wetarea_lineplot_forSI <- 
  
  ggplot() +
  
  # # Error band of full overlap
  # geom_ribbon(data=sum_overlap_mod, 
  #             aes(x=year, ymin= avoid_inv_overlap, ymax=pref_inv_overlap), 
  #             fill='grey92') +
  # 
  # # Error band of MCMC fit
  # geom_ribbon(data= remwet_df_allparams,
  #             aes(x=year, ymin= low, ymax= high), 
  #             fill='grey65') +
  
  # plot nat wet cover line
  geom_line(data=remwet_sum_mean_range,
            aes(x=year, y=remwet_km2/10^6, color=p_name, linetype=s_name)) +
  
  # axis labels
  xlab("Year") + ylab(expression(paste("Natural wetland area (10"^{6},' km'^{2},")"))) +
  

  # axis scales
  scale_x_continuous(expand=c(0,0), labels = seq(1700,2020,100), breaks= seq(1700,2020,100)) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 18), labels = seq(10, 20, 2), breaks= seq(10,20,2)) +
  scale_color_brewer(palette='Set1') +
  
  line_plot_theme +
  guides(colour=guide_legend(title="Present-day\nwetland maps"), linetype=guide_legend(title='Simulated\nwetland maps')) +
  theme(legend.position = 'bottom', #c(0.2, 0.3), #"none",
        legend.title = element_text(face='bold'),
    plot.margin = margin(1,3,1,1,"mm")) #+ ggtitle("Cropland")

wetarea_lineplot_forSI



# /-----------------------------------------------------------------------------
#/ save plot 
ggsave(paste0('../output/figures/timeline/remwet_area/remwet_area_reconstruct_2022.png'),
       wetarea_lineplot_forSI,
       width=120, height=120, dpi=500, units='mm')


# # Get min & max of mean par runs
# remwet_sum_mean_range <- 
#   remwet_sum_mean_range %>% 
#   dplyr::select(-X) %>% 
#   group_by(year) %>% 
#   summarise_all(list(min = min, max=max))#.funs=c(min,max))


# /----------------------------------------------------------------------------#
#/   GET RANGE OF MIN/MAX PARS                                      ---------
# 
# remwet_sum_minmax_range <- data.frame()
# 
# 
# for(r in runs){
#   
#   # print(r)
#   # Read in files
#   pars='min'
#   f_min <- paste0('../output/results/wetloss/sum/sum_remwet_', r,'_t', test_theta,'_', pars, '.csv')
#   pars='max'
#   f_max <- paste0('../output/results/wetloss/sum/sum_remwet_', r,'_t', test_theta,'_', pars, '.csv')
#   
#   remwet_sum_min <- read.csv(f_min)
#   remwet_sum_max <- read.csv(f_max)
#   
#   remwet_sum_minmax_range <- bind_rows(remwet_sum_minmax_range, remwet_sum_min, remwet_sum_max)
# }
# 
# # Calculate range
# remwet_sum_minmax_range <- 
#   remwet_sum_minmax_range %>% 
#   dplyr::select(-X) %>% 
#   group_by(year) %>% 
#   summarise_all(list(min = min, max=max))
# 
# 
# # reset the pars variable
# pars <- 'avg'
# 
# 
# # /----------------------------------------------------------------------------#
# #/  Fig 2 A - Rem.Wet area lineplot
# 
# remwet_df_allparams <- 
#   read.csv('../output/results/wetloss/global_sum_wetloss_mcmc_paramsrange.csv') %>% 
#   dplyr::select(-X, -drain_area_mkm2) %>% 
#   pivot_wider(id_cols = year, names_from = type, values_from = rem_wet_area_mkm2)
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/  Line plot of LU-potwet overlap                                         -----
# 
# library(lemon)
# 
# 
# sum_overlap <- read.csv( '../output/results/wetloss/sum/sum_wet_overlap_3subgrid.csv') 
# 
# 
# # prep data - inverting sum area from pres wetland cover;
# # this makes the uncertainty band expand further back in time
# sum_overlap_mod <- sum_overlap %>%
#   
#   # sum overlap of each LU
#   group_by(year, overlap) %>%
#   summarise(sum_overlap = sum(sum_overlap)/ 10^6) %>%
#   ungroup() %>%
#   
#   # group by overlap
#   group_by(overlap) %>%
#   arrange(overlap, year) %>%
#   
#   # calculate the cumulative departure from rdm scenario, for avoid & pref
#   mutate(lu_exp = sum_overlap - lag(sum_overlap)) %>%
#   mutate(lu_exp = ifelse(is.na(lu_exp), 0, lu_exp)) %>%
#   
#   mutate(lu_exp_cum = cumsum(lu_exp)) %>%
#   mutate(lu_exp_cum = max(lu_exp_cum) - lu_exp_cum) %>%
#   
#   # combine rdm overlap, with the deviation of pref & avoid in a single column
#   mutate(comb = ifelse(overlap=="rdm", max(sum_overlap) - sum_overlap + 12, lu_exp_cum)) %>%
#   ungroup() %>%
#   
#   dplyr::select(-lu_exp, -sum_overlap, -lu_exp_cum) %>%
#   
#   spread(overlap, comb) %>%
#   
#   mutate(pref_inv_overlap = rdm + pref,
#          avoid_inv_overlap = rdm - avoid)
# 
# 
# # /----------------------------------------------------------------------------#
# #/   Make remwet lineplot                                             -------
# 
# fig2a_wetarea_lineplot <- 
#   
#   ggplot(remwet_df_allparams) +
#   
#   # Error band of full overlap
#   geom_ribbon(data=sum_overlap_mod, 
#               aes(x=year, ymin= avoid_inv_overlap, ymax=pref_inv_overlap), 
#               fill='grey92') +
#   
#   # Error band of MCMC fit
#   geom_ribbon(data= remwet_df_allparams,
#               aes(x=year, ymin= low, ymax= high), 
#               fill='grey65') +
#   
#   # plot nat wet cover line
#   geom_line(data=remwet_df_allparams,
#             aes(x=year, y= best), size=0.4) +
#   
#   # axis labels
#   xlab("Year") +  ylab(expression(paste("Natural wetland area (10"^{6},' km'^{2},")"))) +
#   
#   #facet_rep_wrap(.~ overlap, ncol=1) +
#   
#   # axis scales
#   scale_x_continuous(expand=c(0,0), labels = seq(1700,2000,100), breaks= seq(1700,2000,100)) +
#   scale_y_continuous(expand=c(0,0), limits=c(10, 18), labels = seq(10, 20, 2), breaks= seq(10,20,2)) +
#   
#   line_plot_theme +
#   theme(#legend.position = c(0.2, 0.3), #"none",
#     plot.margin = margin(1,3,1,1,"mm")) #+ ggtitle("Cropland")
# 
# fig2a_wetarea_lineplot

