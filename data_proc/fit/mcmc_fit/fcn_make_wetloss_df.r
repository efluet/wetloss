# Make function that takes in theta parameters, does the overlapping, compares to Davidson's sites 
make_wetloss_df <- function(p, s_i, p_i){
  
  theta_rice    = p[1]
  theta_pasture = p[2] 
  theta_urban   = p[3]

  # Functions that distribute different types of drainage (has to be inside function)
  source('./data_proc/overlay/fcn_distrib_drainage_stats_serial_v6_fordf_subnat.r', local=T)  
  

  # Loop through years & LU - running wetland loss mapping 
  source('./data_proc/overlay/full_serial_formcmc_faster_with_df_v4.r', local=T)

  
  # /-------------------------------------------------------
  #/ CALC RESIDUALS FROM HISTCASES
  # residuals
  resid <- cs_joined$map_perc_lost - cs_joined$perc_change_numeric
  # Weight residuals
  resid <- resid * (cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2))
  # Remove NA's that appear when dividing by 0. This happens to
  resid = resid[!is.na(resid)]
  # return(resid)
  
  
  # /--------------------------------------------------------------------------#
  #/ COST FUNCTION WITH  FME::modCost; added on 28 Dec 2020
  
  # Format observation df
  obs_out <- data.frame(name = rep('wetloss', nrow(cs_joined)))
  obs_out$time <- 1:nrow(cs_joined)
  obs_out$val  <- cs_joined$map_perc_lost * (cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2))
  
  # Format model output df
  model_out <- data.frame(time =   1:nrow(cs_joined))
  model_out$wetloss <- cs_joined$perc_change_numeric * (cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2)) * -1
  
  # Use modCost function; for input to modFit
  cost <- FME::modCost(model= model_out, obs = obs_out, y = 'val')
  
  return(cost)

}



# x = 'time',
# obs_out$name <- 'wetloss'
# obs_out$id <- NULL
# obs_df  <- data.frame(cs_joined$map_perc_lost * cs_joined$areapoly_mkm2)

# 2.(optional) value of the independent variable (default column name = "time")  ### NONE
# 3.value of the observation
# 4.(optional) value of the error