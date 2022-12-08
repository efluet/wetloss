
get_drainage_stat_subset_for_distrib <- function(drainage, str_draintype, y){
  
  # /--------------------------------------------------------------------------#
  #/  Get drainage stats to calculate the new drainage to map           -------
  drainage_sub <- drainage %>% 
                  # Filter data
                  filter(year == years[y], type == str_draintype) %>%
                  # join numeral code of country
                  # left_join(., isolookup2, by=c('iso_a3'='iso_a3')) %>%
                  distinct() %>%
                  # Remove rows without a numeric country code 
                  filter(!is.na(iso_a3)) %>% 
                  dplyr::select(iso_a3, HASC_1, pred_drained)
  
  # /--------------------------------------------------------------------------#
  #/ If after 1700, get drainage from previous year
  if (y > 1 ){
    
    drainage_sub_prev <-  drainage %>% 
                          filter(year == years[y-1], type == str_draintype) %>%
                          # left_join(., isolookup2, by=c('iso_a3'='iso_a3')) %>%
                          distinct() %>%
                          filter(!is.na(iso_a3)) %>% 
                          # dplyr::select(iso_a3, pred_drained) %>%   # ID,
                          plyr::rename(c('pred_drained' = 'prev_pred_drained')) %>%  
                          dplyr::select(iso_a3, HASC_1, prev_pred_drained)
                        
    # /------------------------------------------------------------------------#
    #/  Calculate the new drained area for this time step
    #   As the difference between current and previous drainage.
    drainage_sub <- left_join(drainage_sub,  drainage_sub_prev, by=c('iso_a3','HASC_1')) %>%
                    mutate(pred_drained = pred_drained - prev_pred_drained) 
    
    
    }
  
  drainage_sub <- drainage_sub %>% distinct()
  
  new_drain_from_stats <- round(sum(drainage_sub$pred_drained, na.rm = T),0)
  print(paste('  - new drain from stats: ', new_drain_from_stats))
 
  
  return(drainage_sub) 
  }