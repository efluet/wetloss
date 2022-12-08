# Theta distrib (unbounded) applies to rice and urban

# /----------------------------------------------------------------------------#
#/ For theta drainage,  just cap drainage to allowable.  no redistribution.  ------
distrib_drainage_theta <- function(rdm_overlay, theta) {
  
  # /--------------------------------------------------------------------------#
  #/  ALLOWABLE drainage grid;  So drainage cannot exceed potwet or LUarea
  #   The POTWET area shouldn't be used for capping, just relative distribution
  allowable = get(paste0(v,'_eligible'))
  names(allowable) <- c('allowable')
  
  # glimpse(allowable)
  
  
  
  # /--------------------------------------------------------------------------#
  #/   Calculate each pixels % of national overlap (of wet-LU)           -------
  #/ Distribute drainage stats  
  
  # print('rdm_overlay')
  # glimpse(rdm_overlay)
  # print('theta')
  # print(theta)
  
  # df <- data.frame(rdm_overlay) %>% mutate(t = .[[1]] * theta) %>% select(t)   
  df <- rdm_overlay[1] * theta    ##  <--- BLOWS UP HERE
  names(df) <- 'drain_distrib'
  
  df <- df %>%  
    bind_cols(., allowable)  %>% 
    # Cutoff ceiling value of drainage
    mutate(excess = ifelse( drain_distrib > allowable, drain_distrib-allowable, 0),
           drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib)) 
  
  # Print sum area
  print(paste0('      - ', round(sum(df$drain_distrib, na.rm=T), 4), ' Mkm2'))
  
  return(df$drain_distrib)
}
