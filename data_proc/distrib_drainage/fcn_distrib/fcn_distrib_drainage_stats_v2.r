# The loop should redistribute the difference between stat & distrib, rather than excess.
# The reason for not redistributing excess is that it goes to 0 when LU is absent from country,
# causing perc_overlap to all go zero.  The creates an excess of zero, that ends the loop. 





# /----------------------------------------------------------------------------#
#/ Function that estimates drainge serially                        ------
#  Meaning that draining at time  t  is affected by drainaged at t-1

distrib_drainage_stats <- function(rdm_overlay,         # raster of LU-WET random overlap 
                                   y,                   # index / year
                                   str_draintype ) {    # string name of drainage in statistics df
  
  # nb_repeats = 8; this is set in runall now
  nb_redist  = 0  # reset counter of nb or redistributions 
  
  #/  ALLOWABLE drainage grid; set as LU area;  drained area cannot exceed potwet or LUarea
  #   The POTWET area does not limit drainage, it is only used for distribution
  allowable = get(paste0(v,'_eligible'))
  names(allowable) <- c('allowable')
  
  # Convert allowable NAs to 0s; for later convenience
  allowable <- allowable %>% mutate(allowable = ifelse(is.na(allowable), 0, allowable))
  # Increase allowable across all
  # allowable$allowable <- allowable$allowable * 1.2
  
  #  For forestry:  Expand allowable forestry because LU data is too small
  if (str_draintype %in% c('forestry', 'peatextr')) { 
    allowable <- 
      allowable  %>% 
      mutate(allowable= ifelse(allowable > 0 | potwet_df > 0 , landarea, allowable )) } 
  
  

  # /--------------------------------------------------------------------------#
  #/    Get drainage stats for current LUtype & timestep
  drainage_sub <- get_drainage_stat_subset_for_distrib(drainage, str_draintype, y)
  # Subset to two columns; NO FUCKING IDEA WHY THIS IS NEEDED, BUT IT WOULDNT WORK OTHERWISE.
  drainage_sub <- drainage_sub[c('iso_a3', 'HASC_1', 'pred_drained')]
  
  # if drainage sub is negative (LU area decreases), set to zero
  # Negative for timestep with decline in area (wetcult could?)
  
  # Get the total area from drainage statistics; This is the area to distribute over pixels
  sum_drain_stat <- round(sum(drainage_sub$pred_drained, na.rm = T), 0)
  
  # if nothing to distribute, return raster of zeros and skip
  if (sum_drain_stat < 1){ return(rep(0, nrow(allowable)));   break }
  
  # For peatland, use population to prioritize
  if (str_draintype == 'peatextr') { rdm_overlay[[1]] = rdm_overlay[[1]] * pop$pop }
  
  
  # /--------------------------------------------------------------------------#
  #/   Calculate each pixels % of units (national & subnat) overlap (of wet-LU)  -------
  df <- 
    make_perc_overlap(rdm_overlay, str_draintype) %>% 
    dplyr::select(iso_a3, HASC_1, perc_overlap, rdm_overlay) %>% 
    # Fill NAs as 0s
    mutate(perc_overlap = ifelse(is.na(perc_overlap), 0, perc_overlap)) %>%
    # Join the drain stats to the distribution grid 
    left_join(., drainage_sub, by=c('iso_a3', 'HASC_1')) %>% 
    # Append allowable column
    bind_cols(., allowable) %>% 
    #/ !!! Distribute drainage stats; multiply with pred_drained column
    mutate(drain_distrib = perc_overlap * pred_drained)  


    
  # /--------------------------------------------------------------------------#
  #/   CALC EXCESS DRAINAGE & CAP to ALLOWABLE
  df <-   df %>% 
          # Calculate excess drainage over allowable in each pixel
          mutate(excess        = ifelse( drain_distrib > allowable, drain_distrib-allowable, 0 ),
                 # Cap distrib at allowable where it exceeds allowable
                 drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib), 
                 # Update percentage overlap when in excess; turn it off, by assigning 0 value
                 # perc_overlap  = ifelse( excess > 0, 0, perc_overlap)) # %>%
                 perc_overlap  = ifelse( excess > 0, fill_val, perc_overlap)) # %>%
                # Remove the initial drained area, bc now it is functionally replaced by 'excess' area
                 # dplyr::select(-pred_drained) 
  
  
  # PRINT SUMS
  # sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)  # Area to distributed
  # sum_excess_drain  <- round(sum(df$excess, na.rm=T), 1)         # Area exceeding pixel-wise limit
  # pred_remain_drain  <- round(sum_drain_stat - sum_drain_distrib, 1)  # Area remaining to distrib
  # 
  # print(paste0('  n:', nb_redist,'   stat:', sum_drain_stat, ',  distrib: ',
  #              sum_drain_distrib, ',  excess:', sum_excess_drain, ',  remain:', pred_remain_drain))

  source('./data_proc/distrib_drainage/fcn_distrib/fcn_print_distrib_ticker.r', local=T)
  
  
  
  # /--------------------------------------------------------------------------#
  #/   LOOP THAT REDISTRIBUTES TO AVOID DRAINAGE > POTWET OR PIX.AREA
  # while the drained area from stats isn't entirely distributed         -------
  # The loop redistributes the remaining drainage
  
  while(nb_redist < nb_repeats){
    
    # STOPPING CONDITION: when withing 0.5% of the target area from statistics
    if (pred_remain_drain  < sum_drain_stat * 0.005) { break }
    
    # STOPPING CONDITION: when withing 0.5 km^2 of the target area from statistics
    if ( abs(pred_remain_drain - sum_drain_stat) < 0.5) { break }
    


    # /------------------------------------------------------------------------#
    #/ Sum remaining drainage per country - BY SUBNATIONAL?
    remain_per_country <- 
              df %>% 
              # SUM DISTRIB PER SUBNAT (bc joined on HASC_1)
              group_by(iso_a3, HASC_1) %>% 
              # Get total distributed area (before excess)
              dplyr::summarize(drain_distrib = sum(drain_distrib, na.rm=T), .groups='drop') %>% 
              dplyr::select(iso_a3, HASC_1, drain_distrib) %>% 
              ungroup() %>%   ## ADDED NOV2020
      
              # APPEND STATS PER HASC_1 SUBNAT UNIT 
              # 2DEC2020 - THIS IS WHERE THE AREA IS LOST - Q.May2021- What area???
              full_join(., drainage_sub, by=c('iso_a3', 'HASC_1')) %>% 
              mutate(pred_remain = pred_drained - drain_distrib) %>% 
              # prevent negative remain
              mutate(pred_remain = ifelse(pred_remain < 0, 0, pred_remain)) %>% 
              dplyr::select(iso_a3, HASC_1, pred_remain)


    # /------------------------------------------------------------------------#
    #/ Update overlap based on excess/allowable
    #  RELAX THE DISTRIBUTION LIMITS
    df <- 
      df %>% 
      # If there is excess, eliminate the perc_overlap pixel
      mutate(perc_overlap = ifelse(excess > 0, fill_val, perc_overlap)) %>% 
      # If there is LU allowable, expand perc_overlap proportionally to allowable LU
      mutate(perc_overlap = ifelse(allowable>0, allowable * fill_val, perc_overlap)) %>% 
      # If there is any wetland, give some area
      mutate(perc_overlap = ifelse(potwet_df>0, fill_val, perc_overlap))
    
    if(scale_allowable==1){ 
      # Expand allowable by a % change
      df <- df %>% mutate(allowable =  allowable * 1.1 ) }

    # Spread forestry and peat_extr 
    if(expand_perc_overlap ==1 & str_draintype %in% c('forestry', 'peatextr')) {
          df <- df %>% mutate(perc_overlap = 10e-1) #%>% 
            # mutate(allowable = allowable + 10)
        }

    
    # /------------------------------------------------------------------------#
    #/  Spread the excess across the updated overlap 
    df <- df %>% 
      group_by(iso_a3, HASC_1) %>%
      # Rescale perc_overlap to sum to 1
      mutate(perc_overlap = perc_overlap / sum(perc_overlap, na.rm=T)) %>%
      ungroup() %>% 
      # Join the excess with the updated perc_overlap
      left_join(., remain_per_country, by=c('iso_a3', 'HASC_1')) %>%
      
      # update distrib by adding excess
      # 1Dec2020 - instead of distributing the excess (which can drift to 0), distribute the remaining to get to stats
      mutate(drain_distrib = drain_distrib + ( perc_overlap * pred_remain )) %>%
      
      mutate(excess        = ifelse( drain_distrib > allowable, drain_distrib-allowable, 0 )) %>%       # Calculate new excess
      mutate(drain_distrib = ifelse( drain_distrib > allowable, allowable, drain_distrib)) %>%   # Cap drainage to allowable
      # dplyr::select(-excess) %>% 
      tidyr::unnest(col=c()) %>% 
      # Remover sum_drain column, so no duplicates later
      dplyr::select(-pred_remain)
    
    
    # Print ticker
    source('./data_proc/distrib_drainage/fcn_distrib/fcn_print_distrib_ticker.r', local=T)
    
    
    # Update redist loop #
    nb_redist = nb_redist + 1
    
  } # end of redist loop
  
  return(df$drain_distrib)

} # end of function




# mutate(perc_overlap = ifelse(allowable>0, 10e-4, perc_overlap))

# mutate(perc_overlap = ifelse(allowable>=0, 10e-2, perc_overlap*2))   
# mutate(perc_overlap = ifelse(allowable>0 | potwet_df > 0, 10e-2, perc_overlap*2))
# mutate(allowable = ifelse(allowable>=0 | potwet_df > 0, 10e-2, allowable + 100)) 

### THIS WAS BEST SO FAR
# if (nb_redist>=5){ df <- df %>% mutate(perc_overlap = 10e-2, allowable = allowable * 2 ) }

# After 
# if (nb_redist>=5){ df <- df %>% mutate(perc_overlap = 10e-6, allowable = allowable + 20 ) }

# append country code column - needed?
# bind_cols(., ciso_df) 

# mutate(drain_distrib = ifelse(is.na(drain_distrib), 0, drain_distrib)) %>% 

# # Ticker message mid-way
# sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
# init_sum_drain_distrib <- sum_drain_distrib
# print(paste('  - initial drain distrib.: ', sum_drain_distrib))


# mutate(drain_distrib = drain_distrib + (perc_overlap * (sum_drain_stat - sum_drain_distrib ))) %>%

# Eliminate overlap pixels when reaching allowable limit
# mutate(perc_overlap = ifelse(excess > 0, 0, perc_overlap)) %>%
# OMFG THIS IS REVERTED IF RUN BEFORE THE PREV LINE 
# Replace 0 with very small values, so that remaining drainage is distributed evenly across pixels
# mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap), 10e-12, perc_overlap))  %>% 

# sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)
# pred_remain_drain  <- sum_drain_stat - sum_drain_distrib
# print(paste0('  - remain drain: ', pred_remain_drain))

# 1Dec2020 - changed to within a percent of initial stats
# if (nb_redist >= 1 &&  sum_excess_drain < sum_drain_stat * 0.001) { break }
# if (nb_redist >= 1 &&  sum_drain_stat - sum_drain_distrib  < sum_drain_stat * 0.005) { break }
# Loosen the allowable limits for forestry bc LU data is too low
# if (nb_redist > 2 && str_draintype %in% c('forestry', 'peatextr')) { df$allowable <- df$allowable + 20  }

# Then, give a small perc_overlap to all pixels with some LU, because allowable is related to undrained LU
# i.e. if they're full pixel, give them a little more; by adding to perc_overlap
# if (nb_redist >=1 ){
#   df<- df %>% mutate(perc_overlap = ifelse(allowable>=0, 10e-2, perc_overlap))  # || is.na(perc_overlap)
# }
# Then, give a small perc_overlap to all pixels that had excess
# mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap) , 10e-2, perc_overlap),
#        allowable = allowable * 1.2)  # || is.na(perc_overlap)
