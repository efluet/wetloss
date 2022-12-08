# Description: This script uses output of MCMC, and produces final outputs
#              using fitted parameters.
#              It runs the same process as tht in the MCMC, but less optimized, and 
#              this version saves outputs to disc (not kept in memory). 

# /----------------------------------------------------------------------------#
#/    GET MCMC PARAMETER RANGES                                          -------
pars_all <- read.csv('../output/results/mcmc/parameters/pars_modFit_v2.csv')  #v1
glimpse(pars_all)

# /----------------------------------------------------------------------------#
#/    config
test_theta = 1       # use test theta values (of 0,0,0)
test_potwet = 0      # use test potwet
update_potwet = 1    # whether to subtract drained area from potwet
save_all_output = 1  # Save all outputs for fig 2 & 3


# /----------------------------------------------------------------------------#
#/ CREATE EMPTY DF OUTPUTS                                               -------

global_sums_allparams_df  <- data.frame()
remwet_sum_allparams_df   <- data.frame()


# INTIALIZE MODMCMC & MODFIT
source('data_proc/overlay/initialize_prefitting.r', local=FALSE)

# /----------------------------------------------------------------------------#
#/   LOOP THROUGH FITTED PARAMETERS

for (i in 1:nrow(pars_all)) {
  
  # Set parameters
  theta_rice    = as.numeric(pars_all[i, 'theta_rice'])
  theta_pasture = as.numeric(pars_all[i, 'theta_pasture'])
  theta_urban   = as.numeric(pars_all[i, 'theta_urban'])
  s_i           = as.numeric(pars_all[i, 's'])
  p_i           = as.numeric(pars_all[i, 'p'])
  
  
  # /--------------------------------------------------------------------------#
  #/.. Run the wetland loss mapping                                     --------
  source('data_proc/overlay/run_draindistrib_loop_jan2021.r', local=FALSE)

  }



# /--------------------------------------------------------------------------#
#/ Estimate RemWet from drained area.                                 --------
# source('data_proc/make_tot_remwet_grid_additive.r', local=TRUE)

# global_sums_df$type    <- params_type
# global_sums_df$preswet <- preswet_name

# global_sums_df$s <- s_i
# global_sums_df$p <- p_i
# global_sums_df$runid <- i 
# global_sums_df$preswet_km2 <- sum(preswet_df, na.rm=T)
# # Append run to df 
# global_sums_allparams_df <- bind_rows(global_sums_allparams_df, global_sums_df)
# 
# 
# remwet_sum$type    <- params_type
# remwet_sum$preswet <- preswet_name 
# remwet_sum$runid   <- i 
# remwet_sum_allparams_df <- bind_rows(remwet_sum_allparams_df, remwet_sum)

# source('./data_proc/overlay/full_serial_formcmc_faster_with_df.r', local=FALSE)
# output_df$params <- params_type
# ludrain_allparams <- bind_rows(ludrain_allparams, output_df)
# # Save LU stack
# saveRDS(s_cumul_out, paste0('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul_', params_type,'.rds'))

# Save the remwet stack 
# saveRDS(s_out, paste0('../output/results/wetloss/grid/remwet_tot_stack_0.5deg_serial_', params_type,'.rds'))

# /--------------------------------------------------------------------------#
#/   Extract the loss at Davidson locations
#.. DATA PROC - extract raster data with poly histcases  and join them to the histcase loss%
# source('./data_proc/hist_cases/extract_raster_wetloss_with_histcases_v5_fromncdf.r', local=TRUE)
# histcasedat <- histcases_wmaploss@data
# histcasedat$param_type <- as.character(params_type)
# histcasedat_allparams<- bind_rows(histcasedat_allparams, histcasedat)


# /----------------------------------------------------------------------------#
#/ Save output as CSV                                    -------
# write.csv(global_sums_allparams_df, '../output/results/wetloss/sum/global_sums_allparams_df.csv')  # for stacked LU plot
# write.csv(ludrain_allparams, '../output/results/wetloss/sum/sum_lu_drain_serialmcmc.csv')  # for stacked LU plot
# write.csv(remwet_df_allparams, '../output/results/wetloss/global_sum_wetloss_mcmc_paramsrange.csv')
# write.csv(histcasedat_allparams, '../output/results/histcases/histcase_mappedwetloss_extracted_v4_serialmcmc.csv')



# a <- global_sums_allparams_df %>% 
#   group_by(year, type, preswet, runid) %>% 
#   summarize(new_drain_km2   = sum(new_drain_km2, na.rm=T),
#             cumul_drain_km2 = sum(cumul_drain_km2, na.rm=T),
#             rem_potwet_km2  = min(rem_potwet_km2, na.rm=T),
#             preswet_km2 = mean(preswet_km2, na.rm=T))
# 
# 
# # d <- subset(global_sums_allparams_df, preswet=='wad2m' & type=='best')
# ggplot(d) +
#   geom_area(aes(x=year, y=cumul_drain_km2, group=lu_type, fill=lu_type), color='white', size=0.03, alpha=0.9, position='stack')
# 
# write.csv(global_sums_allparams_df, '../output/results/wetloss/sum/sum_lu_drain_serialmcmc_wad2m.csv')


#  PLOT UNCERTAINTY OF DRAINAGE - ACROSS WETLAND AREA USED
# ggplot(a) +
#   geom_line(aes(x=year, y=cumul_drain_km2, group=runid, color=preswet))
# geom_ribbon(aes(x=year, ymin=min(cumul_drain_km2), ymax=max(cumul_drain_km2), group=runid, color=preswet))



# # /----------------------------------------------------------------------------#
# #/ REMWET PERC PLOT                                                     --------
# remwet_sum_allparams_df2 <- 
#   remwet_sum_allparams_df %>% 
#   pivot_wider(names_from=type, values_from=remwet_km2) %>% 
#   group_by(year, preswet) %>% 
#   summarise(low= mean(low, na.rm=T),
#             best= mean(best, na.rm=T),
#             high= mean(high, na.rm=T))
# 
# ### WRITE THIS AS CSV: remwet_sum_allparams_df2
# remwet_Mkm2_inset <- 
#   ggplot(remwet_sum_allparams_df2) +
#   geom_ribbon(aes(x=year, ymin=low/10^6, ymax=high/10^6, group=preswet), fill='grey80') +
#   geom_line(aes(x=year, y=best/10^6, group=preswet), color='black') +
#   geom_text(data=subset(remwet_sum_allparams_df2, year==max(year)),
#             aes(x=year+15, y=best/10^6 , label=preswet)) +
#   line_plot_theme +
#   xlab("") + ylab("Wetland area (M km2)")
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/ REMWET AREA PLOT                                                     --------
# remwet_sum_allparams_df_perc <- 
#   remwet_sum_allparams_df %>% 
#   group_by(type, preswet, runid) %>% 
#   mutate(perc_loss = remwet_km2/max(remwet_km2)) %>% 
#   ungroup() %>% 
#   group_by(year) %>% 
#   summarise(perc_loss_min = min(perc_loss),
#             perc_loss_med = median(perc_loss),
#             perc_loss_max = max(perc_loss))
# 
# 
# remwet_perc <-  
#   ggplot(remwet_sum_allparams_df_perc) +
#   geom_ribbon(aes(x=year, ymin=perc_loss_min*100, ymax=perc_loss_max*100), fill='grey80') +
#   geom_line(aes(x=year, y=perc_loss_med*100), color='black') +
#   line_plot_theme +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(limits=c(75, 100))
# 
# 
# 
# library(ggpubr)
# remwet_winset <- ggarrange(remwet_Mkm2_inset, remwet_perc,
#                            ncol=1, labels = c("A", "B"),
#                            align="h")
# 
# 
# saveRDS(remwet_winset, '../output/figures/fig2/fig2_remwet_perc_n_area')
# 
# 
# # /----------------------------------------------------------------------------#
# #/    Save figure to file 
# ggsave('../output/figures/remwet_perc_loss_3preswet_winset.png', 
#        width=90, height=90, dpi=300, units='mm') #type = 'cairo-png')
# dev.off()
# 