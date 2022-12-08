# Make function that takes in theta parameters, 
# does the overlapping, compares to Davidson's sites 

make_wetloss_df <- function(p, s_i, p_i){
  
  theta_rice    = p[1]
  theta_pasture = p[2] 
  theta_urban   = p[3]

  
  # Functions that distribute different types of drainage
  source('./data_proc/overlay/fcn_distrib_drainage_stats_serial_v6_fordf_subnat.r', local=T)  
  
  # Init: Get POTWET AND PRESWET AMONG 3X4 ENSEMBLE
  source('./data_proc/mcmc_init/init_potwet_every_run.r', local=T) 
  
  # Run drainage distrib loop 
  source('./data_proc/overlay/year_lu_distrib_loop.r', local=T)
  
  # Compare maps and case studies 
  source('./data_proc/overlay/compare_lossperc_cs_joined.r', local=T)
  # Not used - Compare to peatland case studies
  # source('./data_proc/overlay/compare_lossperc_cs_joined_peatland.r', local=T)
  
  
  
  # /--------------------------------------------------------------------------#
  #/  CALC RESIDUALS FROM HISTCASES
  
  ### JUNE2021 - exclude New Zealand and Lake Chad from optimization bc statistics are faulty
  cs_joined <- cs_joined %>% filter(!country %in% c('New Zealand', 'Chad'))

  # Calculate residuals
  resid <- cs_joined$map_perc_lost - cs_joined$perc_change_numeric
  
  # Weight residuals; by poly area or remwet
  # resid <- resid * log(cs_joined$remwet_end)  
  weights <- (log(cs_joined$areapoly_mkm2) + abs(min(log(cs_joined$areapoly_mkm2))))
  resid <- resid * weights
  
  # Remove NA's that appear when dividing by 0. This happens to...
  resid = resid[!is.na(resid)]
  
  return(resid)
    
    
  }
  
  # Calculate residuals
  resid <- cs_joined$map_perc_lost - cs_joined$perc_change_numeric
  
  # Weight residuals; by poly area or remwet
  # resid <- resid * log(cs_joined$remwet_end)  
  weights <- (log(cs_joined$areapoly_mkm2) + abs(min(log(cs_joined$areapoly_mkm2))))
  resid <- resid * weights
  
  # Remove NA's that appear when dividing by 0. This happens to...
  resid = resid[!is.na(resid)]
  
  return(resid)
  
  }




# for var in MCMC:
#  can be obtained from the mean squares of fitted residuals. 
#  (e.g. as reported in modFit).

# /--------------------------------------------------------------------------#
#/  COST FUNCTION WITH  FME::modCost; added on 28 Dec 2020
# 
# # Format observation df
# obs_out <- data.frame(name = rep('wetloss', nrow(cs_joined)))
# obs_out$time <- 1:nrow(cs_joined)
# obs_out$val  <- cs_joined$map_perc_lost * (cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2))
# 
# # Format model output df
# model_out <- data.frame(time =   1:nrow(cs_joined))
# model_out$wetloss <- cs_joined$perc_change_numeric * (cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2)) * -1
# 
# # Use modCost function; for input to modFit
# cost <- FME::modCost(model= model_out, obs = obs_out, y = 'val')
# 
# return(cost)


# Loop through years & LU - running wetland loss mapping 
# source('./data_proc/overlay/full_serial_formcmc_faster_with_df_v4.r', local=T)
