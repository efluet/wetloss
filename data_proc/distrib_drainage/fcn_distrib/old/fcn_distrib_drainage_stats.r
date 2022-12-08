# /----------------------------------------------------------------------------#
#/ Function that estimates drainge serially                        ------
#  Meaning that draining at time  t  is affected by drainaged at t-1

distrib_drainage_stats <- function(rdm_overlay,         # raster of LU-WET random overlap 
                                   y,                   # index / year
                                   str_draintype ) {    # string name of drainage in statistics df
  
  # nb_repeats = 10  - this is set in runall script
  nb_redist  = 0  # reset counter of nb or redistributions 
  
  #/  ALLOWABLE drainage grid;  So drainage cannot exceed potwet or LUarea
  #   The POTWET area does not limit drainage, it is only used for distribution
  #   this is why now only the LU area is used as allowable cap
  # allowable = data.frame(get(v)[y])
  allowable = get(paste0(v,'_eligible'))
  names(allowable) <- c('allowable')
  # Conver NAs to 0s
  allowable <- allowable %>% mutate(allowable = ifelse(is.na(allowable), 0, allowable))

  #  For forestry:  Expand allowable forestry because LU data is too small
  if (str_draintype %in% c('forestry', 'peatextr')) { allowable$allowable <- landarea }
  # allowable <- allowable * 2 
  # allowable[allowable > 3000] <- 3000   # Cap allowable to the area of gridcell
  # allowable[allowable$allowable > landarea, 'allowable'] <- landarea
  
  ### DELETE THIS -  RAISE ALLOWABLE FOR TESTING
  # allowable$allowable <- landarea # allowable * 100 
  
  
  #/   Get drainage stats for this LU & time
  drainage_sub <- get_drainage_stat_subset_for_distrib(drainage, str_draintype, y)
  # Subset to two columns; NO FUCKING IDEA WHY THIS IS NEEDED, BUT IT WOULDNT WORK OTHERWISE.
  drainage_sub <- drainage_sub[c('iso_a3', 'HASC_1', 'pred_drained')]
  # if drainage sub is negative (LU area decreases), set to zero
  # Negative for timestep with decline in area (wetcult could?)
  sum_drain_stat <- round(sum(drainage_sub$pred_drained, na.rm = T), 0)
  # if nothing to distribute, return raster of zeros and skip
  if (sum_drain_stat < 1){ return(rep(0, nrow(allowable)));   break }
  
  
  # /--------------------------------------------------------------------------#
  #/   NATIONAL Calculate each pixels % of national overlap (of wet-LU)  -------
  
  if (str_draintype == 'peatextr') { rdm_overlay[[1]] = rdm_overlay[[1]] * pop$pop }
  
  df <- 
    make_perc_overlap(rdm_overlay, str_draintype) %>% 
    dplyr::select(iso_a3, HASC_1, perc_overlap, rdm_overlay) %>% 
    # Fill NAs as 0s
    mutate(perc_overlap = ifelse(is.na(perc_overlap), 0, perc_overlap)) %>%
    left_join(., drainage_sub, by=c('iso_a3', 'HASC_1')) %>% 
    bind_cols(., allowable) %>% 
    #/ Distribute drainage stats  
    mutate(drain_distrib = perc_overlap * pred_drained) %>% 
    # mutate(drain_distrib = ifelse(is.na(drain_distrib), 0, drain_distrib)) %>% 
    bind_cols(., ciso_df) 
  
  
  # Ticker message mid-way
  sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
  init_sum_drain_distrib <- sum_drain_distrib
  print(paste('  - initial drain distrib.: ', sum_drain_distrib))

    
  # CALC EXCESS DRAINAGE
  df <-   df %>% 
    # Calculate excess drainage over the allowable limit in each pixel
    mutate(excess        = ifelse( drain_distrib > allowable, drain_distrib-allowable, 0 ),
           # Cap distrib at allowable where it exceeds allowable
           drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib), 
           # Update percentage overlap when in excess
           perc_overlap  = ifelse( excess > 0, 0, perc_overlap)) %>%
    # Remove the initial drained area, bc now it is functionally replaced by 'excess' area
    dplyr::select(-pred_drained) 
  
  
  sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
  sum_excess_drain  <- round(sum(df$excess, na.rm=T), 1)
  print(paste0('  -n:', nb_redist,'  - total:', sum_drain_distrib + sum_excess_drain, 
               ' =  kept: ', sum_drain_distrib, ' +  excess:', sum_excess_drain))
  
  
  
  # /--------------------------------------------------------------------------#
  #/   LOOP THAT REDISTRIBUTES REMAINING DRAINAGE TO AVOID DRAINAGE > POTWET OR PIX.AREA
  #    while the drained area from stats isn't entirely distributed         -------

  
  while(nb_redist < nb_repeats){   #sum_excess_drain > 0) {
    
    # STOPPING CONDITION: if it tried one loop of redistrib  and got within 10km2 of the target area
    # 1Dec2020 - changed stopping condition to within a percent of initial stats
    # Q. May 2021 - why no 
    # if (nb_redist >= 1 &&  sum_excess_drain < sum_drain_stat * 0.001) { break } 
    if (nb_redist >= 1 &&  (sum_drain_stat - sum_drain_distrib) < sum_drain_stat * 0.005) { break }

    # Loosen the allowable limits for forestry bc LU data is too low
    # if (nb_redist > 2 && str_draintype %in% c('forestry', 'peatextr')) { df$allowable <- df$allowable + 20  }
    

    # /--------------------------------------------------------------
    #/ Sum excess per country;  TODO: Sum by sub-national unit; nah?
    excess_per_country <- 
              df %>% 
              group_by(iso_a3, HASC_1) %>% 
              dplyr::summarize(drain_distrib = sum(drain_distrib, na.rm=T)) %>% 
              dplyr::select(iso_a3, HASC_1, drain_distrib) %>% 
              ungroup() %>%   ## ADDED NOV2020
              left_join(., drainage_sub, by=c('iso_a3', 'HASC_1')) %>% 
              mutate(sum_excess= pred_drained - drain_distrib) %>% 
              mutate(sum_excess= ifelse(sum_excess < 0, 0, sum_excess)) %>% 
              dplyr::select(iso_a3, HASC_1, sum_excess)
    
    # df<- df %>% 
    #   mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap), 10e-12, perc_overlap))  
    #   mutate(allowable = ifelse(allowable < 100,  allowable +10 ,allowable))
    # df$allowable <- df$allowable * 1.2 }

    # If there is excess in a pixel, eliminate the perc_overlap pixel by turning them to 0
    df<- df %>% mutate(perc_overlap = ifelse(excess > 0, 0, perc_overlap))
    # Then set allowable to 0.01 -  WHY???
    # df<- df %>% mutate(perc_overlap = ifelse(allowable>=0, 10e-2, perc_overlap))  # || is.na(perc_overlap)
    
    # Then, give a small perc_overlap to all pixels with some LU, because allowable is setby undrained LU area (& pixel area)
    # i.e. if they're full pixel, give them a little more; by adding to perc_overlap
    if (nb_redist >=1 ){ df<- df %>% mutate(perc_overlap = ifelse(allowable>=0, 10e-2, perc_overlap))  # || is.na(perc_overlap)  }
    
    # After first loop, increase the allowable limit 
    # if (nb_redist >=1 ){  df<- df %>% mutate(perc_overlap = 10e-2, allowable = allowable + 20 )  }
        # Then, give a small perc_overlap to all pixels that had excess
        # mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap) , 10e-2, perc_overlap),
        #        allowable = allowable * 1.2)  # || is.na(perc_overlap)

    
    sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
    remain_drain <- sum_drain_stat <- sum_drain_distrib
    print(paste0('  - remain drain: ', remain_drain))
    
    
    # Spread the excess across the updated overlap grid
    df <- df %>% 
      group_by(iso_a3, HASC_1) %>% 
      # Eliminate overlap pixels when reaching allowable limit
      # mutate(perc_overlap = ifelse(excess > 0, 0, perc_overlap)) %>%
      # OMFG THIS IS REVERTED IF RUN BEFORE THE PREV LINE 
      # Replace 0 with very small values, so that remaining drainage is distributed evenly across pixels
      # mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap), 10e-12, perc_overlap))  %>% 
      # Rescale perc_overlap to sum to 1
      mutate(perc_overlap = perc_overlap / sum(perc_overlap, na.rm=T)) %>%
      ungroup() %>% 
      # Join the excess with the updated perc_overlap
      left_join(., excess_per_country, by=c('iso_a3', 'HASC_1')) %>%
      # update distrib by adding excess
      # 1Dec2020 - instead of distributing the excess (which can 0), distribute the remaining to get to stats
      
      # mutate(drain_distrib = drain_distrib + (perc_overlap * (sum_drain_stat - sum_drain_distrib ))) %>%            
      mutate(drain_distrib = drain_distrib + (perc_overlap * sum_excess )) %>%
      
      mutate(excess        = ifelse( drain_distrib > allowable, drain_distrib-allowable, 0 )) %>%       # Calculate new excess
      mutate(drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib)) %>%   # Cap drainage to allowable
      dplyr::select(-sum_excess) %>% 
      tidyr::unnest()
    
    
    
    sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
    sum_excess_drain  <- round(sum(df$excess, na.rm=T), 1)
    print(paste0('  -n:', nb_redist,'  - total:', sum_drain_distrib + sum_excess_drain, 
                 ' =  kept: ', sum_drain_distrib, ' +  excess:', sum_excess_drain))
    
    nb_redist = nb_redist + 1
  }
  
  return(df$drain_distrib)
}




# Only repeat distribution a few times
# if (nb_redist > (nb_repeats-1)) { break }


# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=rdm_overlay)) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, rdm_overlay ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=perc_overlap)) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, perc_overlap ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=pred_drained)) +
#   scale_fill_distiller(palette='Purples', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, pred_drained ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=drain_distrib)) +
#   scale_fill_distiller(palette='Greens', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, drain_distrib ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
