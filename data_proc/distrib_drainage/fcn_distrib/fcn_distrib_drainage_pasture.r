################################################################################
distrib_drainage_pasture <- function(rdm_overlay, theta, year) {
  
  
  #/  ALLOWABLE drainage grid;  So drainage cannot exceed potwet or LUarea
  #   The POTWET area shouldn't be used for capping, just relative distribution
  #   this is why now only the LU area is used as allowable cap
  # allowable = data.frame(get(v)[y])
  allowable = get(paste0(v,'_eligible'))
  names(allowable) <- c('allowable')
  #allowable[allowable>3000] <- 3000   # Cap allowable to the area of gridcell
  
  
  # /--------------------------------------------------------------------------#
  #/   Calculate each pixels % of national overlap (of wet-LU)           -------
  
  #/ Distribute drainage stats  
  df <- rdm_overlay * theta
  
  names(df) <- 'drain_distrib'
  
  df <- df %>%
    # Bind allowable area, as col
    bind_cols(., allowable)  %>% 
    # Bind country code area, as col
    bind_cols(., ciso_df) %>% 
    mutate(year=year) %>% 
    left_join(., cropland_perc_drained, by=c('iso_a3'='iso_a3', 'year'='year')) %>% 
    ### BIG BIG BIG CHANGE - NOV 2021 - RELAX LIMIT ON PASTURE :  removed the    * 1.5 
    mutate(crop_drain_cap = crop_drain_perc  * allowable) %>% 
    mutate(drain_distrib = ifelse( drain_distrib > crop_drain_cap, crop_drain_cap, drain_distrib)) 
  # mutate(drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib)) 
  
  
  print(paste0('      - ', round(sum(df$drain_distrib, na.rm=T), 4), ' Mkm2'))  
  
  return(df$drain_distrib)
}

