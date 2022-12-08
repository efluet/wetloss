

# /----------------------------------------------------------------------------#
#/   Calculate each pixels % of national overlap (of wet-LU)               -----
#    by dividing the pixels by the national total of overlap.
#    Why are there negative values?  becaus of negative pot-wet?

make_perc_overlap <- function(rdm_overlay, str_draintype) {
  
  names(rdm_overlay) <- 'rdm_overlay'
  
  # Subset the geo_unit df
  geo_units_sub <- geo_units_alltypes %>%  filter(type == str_draintype)
  
  # Bind rdm overlay with country index
  z <-  bind_cols(geo_units_sub, rdm_overlay) %>%    #, ciso_df) %>% 
    group_by(GID_0, HASC_1)  %>%   #iso_a3) %>% 
    # Calculate fraction of rdm overlap per country
    mutate(perc_overlap = rdm_overlay / sum(rdm_overlay, na.rm=T)) %>% 
    # Fill NAs as 0s
    mutate(perc_overlap = ifelse(is.na(perc_overlap), 0, perc_overlap)) %>%
    ungroup()
  
  # Use column name that'll work with the rest... eventually clean this up
  z$iso_a3 <- z$GID_0
  
  # perc_overlap <- rdm_overlay / z$overlap_sum
  return(z)
}