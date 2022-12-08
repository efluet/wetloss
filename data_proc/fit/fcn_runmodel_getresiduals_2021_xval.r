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
  
  library(Metrics)
  
  # Weight residuals; by poly area or remwet
  # resid <- resid * log(cs_joined$remwet_end)  
  cs_joined <- 
    cs_joined %>% 
    # Calculate weights
    mutate(weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2)))) %>% 
    # Calculate residuals
    mutate(resid = abs((map_perc_lost) - abs(perc_change_numeric)) *weights) %>% 
    filter(!is.na(resid)) #%>% 
    # # RMSE
    # mutate(RMSE = rmse(map_perc_lost, perc_change_numeric)) %>% 
    # # R^2adj UNWEIGHTED
    # mutate(r.squared = summary(lm(perc_change_numeric ~ 0 + map_perc_lost))$r.squared) %>%  #, data= cs_joined)
    # mutate(adj.r.squared = summary(lm(perc_change_numeric ~ 0 + map_perc_lost))$adj.r.squared) %>%  #, data= cs_joined)
    # mutate(w.r.squared = summary(lm(perc_change_numeric ~ 0 + map_perc_lost,
    #                     weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2)))))$r.squared) %>% #,data= cs_joined))
    # mutate(w.adj.r.squared = summary(lm(perc_change_numeric ~ 0 + map_perc_lost,
    #                                 weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2)))))$adj.r.squared) %>% #,data= cs_joined))
    # mutate(bias = mean(map_perc_lost, na.rm=T) - abs(mean(perc_change_numeric, na.rm=T)))
    # 
  
  

  return(cs_joined)
    
  }


# f <- summary(fitlm)
#### R^2adj WEIGHTED by polygon area
# fitlm = lm(perc_change_numeric ~ 0 + map_perc_lost,
#            weights = log(cs_joined$areapoly_mkm2) + abs(min(log(cs_joined$areapoly_mkm2))),
#            data= cs_joined)
# 
# fw <- summary(fitlm)


# Bias
# bias <- mean(cs_joined$map_perc_lost, na.rm=T) -  abs(mean(cs_joined$perc_change_numeric, na.rm=T))