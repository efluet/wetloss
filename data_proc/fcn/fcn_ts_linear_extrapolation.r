
# make function that extrapolates linearly from 2 first/last points 


# templates:  df with lenght
# templates:  df with all the years
# n_obs: how many observations use to make the linear extrapolation

# templates = country_ts_length
# yearly = country_ts_template
# n_obs = 2

linear_extrapol <- function(templates, yearly, n_obs){
  
  # create ouput df
  output_df <- slice(yearly, 0)
  
  # loop through individual countries x types
  for (c in 1:nrow(templates)){
    
    print(paste(templates[c, 'country_name'], templates[c, "type"]))
    
    # subset the ts data to the single-looped country x type
    temp_yearly <- yearly %>%
      filter(country_name == templates[c, 'country_name'] &
               type == templates[c, "type"])
    
    
    
    
    ###  EXTRAPOL BEFORE            ---------------------------------------------
    
    # filter to keep only data points
    # only the last n
    temp_yearly_wdata_before <- temp_yearly %>% 
                                filter(valtype == "data") %>%
                                top_n(n = -n_obs, wt= year)
    
    
    # do the  extrapol
    drain_extrapol_before <- data.frame(approxExtrap(temp_yearly_wdata_before$year,
                                              temp_yearly_wdata_before$ts_drained_area_tot,
                                              xout=subset(temp_yearly$year, temp_yearly$year < min(temp_yearly_wdata_before$year))))
                                              # xout=subset(hyde_yrs, hyde_yrs < min(temp_yearly_wdata_before$year))))
    
    names(drain_extrapol_before) = c("year", "drained_area")
    
    print(drain_extrapol_before)
    
    
    
    ###  EXTRAPOL AFTER            ---------------------------------------------
    
    temp_yearly_wdata_after <- temp_yearly %>% 
                              filter(valtype == "data") %>%
                              top_n(n = n_obs, wt= year)
    
    
    # do the  extrapol
    drain_extrapol_after <- data.frame(approxExtrap(temp_yearly_wdata_after$year,
                                              temp_yearly_wdata_after$ts_drained_area_tot,
                                              xout=subset(hyde_yrs, hyde_yrs > max(temp_yearly_wdata_after$year))))
    
    names(drain_extrapol_after) = c("year", "drained_area")
    
    
    print(drain_extrapol_after)
    
    
    
    ### RECOMBINE                       ----------------------------------------
    
    drain_extrapol <- bind_rows(drain_extrapol_before, drain_extrapol_after)
    
    
    temp_yearly <- left_join(temp_yearly, drain_extrapol, by="year") %>%
                   mutate(drained_area=ifelse(drained_area<0, 0, drained_area))
    
    output_df<- bind_rows(output_df, temp_yearly)
    
  }
  return(output_df)
}
