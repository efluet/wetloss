

# Preprocess the drainage data
if(0){
  # drained <-  read.csv('../data/artif_drained/all_drain_stat_comb_v5.csv') %>%
  drained <-  read.csv('../data/artif_drained/all_drain_stat_comb_v7.csv') %>%
    # Convert are to km2
    mutate(drained_area_tot = as.numeric(drained_area_tot)) %>%
    mutate(drained_area_tot_km2 = drained_area_tot / 100 * 1000) %>%
    # add country codes
    mutate(iso_a3 = countrycode(country,'country.name','iso3c',warn=F)) %>%
    mutate(country_name = countrycode(iso_a3,'iso3c','country.name',warn=F)) %>%
  
    # calc decade
    mutate(decade=round(year,-1)) %>% 
    # Convert to title case
    mutate(type = str_to_title(type)) %>% 
    # add label of country groups to use
    mutate(continent = countrycode(country_name, "country.name", "region")) %>%
    mutate(continent = ifelse(continent %in% c("Central America","South America", "Caribbean"),"Central & South America",continent)) %>%
    mutate(continent = ifelse(continent %in% c("Western Europe","Southern Europe", "Northern Europe"), "Western Europe", continent)) %>%
    mutate(continent = ifelse(continent %in% c("Western Asia","Central Asia"), "Western & Central Asia", continent)) %>%
    mutate(continent = ifelse(continent %in% c("Northern America"), "North America", continent)) %>%
    mutate(continent = ifelse(continent %in% c("Western Africa", "Southern Africa","Eastern Africa","Middle Africa"), "Africa", continent))
  
      
  # Save to file
  write.csv(drained, '../data/artif_drained/all_drain_stat_comb_v7.csv', row.names=F)

}



# /----------------------------------------------------------------------------#
#/  PEAT EXTRACTION ; calculate cumulative weights          --------------------

drained_peatex_dat <- read.csv('../data/artif_drained/all_drain_stat_comb_v7.csv') %>% 
                  filter(type=='Peat Extraction') %>% 
                  filter(!is.na(drained_weight)) %>% 
                  filter(!is.na(country_name)) %>%
                  filter(unit != 'subnational') %>%  # Added Sept 2022 
                  # Exlude rows with import data (not production)
                  filter(., !grepl("Import*", Comment)) %>% 
                  distinct() %>%
                  # add empty rows for each missing year, where the interpolation will be inserted
                  # fill with all the years with data and the decadal intervals from hyde
                  complete(., nesting(country_name, type), year=unique(c(seq(1700, 2010, 10), unique(d$year)))) %>%
                  # sort by country and year
                  arrange(country_name, type, year)


# /----------------------------------------------------------------------------#
#/  Interpolate annual peat extraction the NA between years with data     -------
drained_peatex <- drained_peatex_dat %>%
        #  ungroup %>%
        group_by(country_name, type) %>%
        # convert from '1000 tonnes' to tonnes 
        mutate(drained_weight = drained_weight * 1000) %>%
        # interpolate linearly within 
        mutate(drained_weight = na.approx(drained_weight,  method="linear", na.rm=F)) %>% 
        ungroup() %>% 
        filter(!is.na(year)) %>%
        # Calculate decade from year
        mutate(decade=round(year,-1)) %>% 
        group_by(country_name, type, decade) %>%
        # average data & interpolated value per decade
        dplyr::summarize(drained_weight = mean(drained_weight, na.rm=T))


# Filter to time period with data 
drained_peatex <- drained_peatex %>%
        group_by(country_name) %>%
        # Make new column to determine first decade w/ data
        mutate(min_decade = ifelse(!is.na(drained_weight), decade, NA)) %>%
        mutate(min_decade = min(min_decade, na.rm=T)) %>%
        ungroup() %>%
        # complete(., nesting(country_name), decade=unique(seq(1700, 2010, 10))) %>%
        # Extrapolate peat extraction to prior; Doubling every decade
        arrange(country_name, decade)

# Extrapolate to period before first data point 
# Loop and repeat the halving every 10 year prior to first data point
for(i in seq(1700, 2020, 10)){
  drained_peatex <- drained_peatex %>%
        mutate(drained_weight = ifelse(is.na(drained_weight) & decade < min_decade, lead(drained_weight)/2, drained_weight))
  }

# Format data, removing empty rows; set lower limit of 10tonnes for the extrapolation
drained_peatex <- drained_peatex %>% 
                  filter(!is.na(drained_weight), drained_weight > 10) 
  

# Get cumulative sum in tonnes of peat, base on annual extraction rates
# drained_peatex_int <- 
#         drained_peatex %>%
#         ## fill with all year, so to sum annual extraction
#         complete(., nesting(country_name, type), decade=unique(seq(1700, 2010, 1))) %>%
#         mutate(drained_weight = na.approx(drained_weight,  method="linear", na.rm=F)) %>%
#         filter(!is.na(drained_weight)) %>%
#         ungroup()

drained_peatex_int <- drained_peatex %>% 
        # Calculate cumulative sum
        group_by(country_name) %>%
        mutate(drained_weight_cumsum = ave(drained_weight, country_name, FUN=cumsum)) %>% 
        # Remove the non decadal rows with this trick
        filter(!is.na(min_decade)) %>%
        # Then remove the decade column
        dplyr::select(-min_decade) %>%
        # Group by decade; averaging multiple records
        # Calculate decade from year
        # mutate(decade=round(year,-1)) %>% 
        # group_by(country_name, type, decade) %>% 
        # dplyr::summarize(drained_weight_cumsum = mean(drained_weight_cumsum, na.rm=T)) %>% 
        ungroup() %>%
        filter(!is.na(drained_weight_cumsum)) %>%
        group_by(country_name) %>%
        # Convert tonnes to area with 300000 tonnes / km2
        # Multiply cumsum by 10, because it represents annual rate for each decade 
        mutate(drained_area_peat = ifelse(!is.na(drained_weight_cumsum), drained_weight_cumsum * 10 / 300000)) %>%
        ungroup()

# Get global totals
if(1){
  p <- drained_peatex_int %>%
    group_by(country_name) %>% 
    filter(decade == max(decade, na.rm=T))
  
  # cumulative tonnage of peat:
  # Fixed:  800 million tonnes since 1700 
  sum(p$drained_weight_cumsum / 10^6)
  # Cumulative area million km2
  sum(p$drained_area_peat / 10^3)
  # 26 thousand km^2 when using 310,000 tons per km2
}


# Peat extraction data for Alex Berthelmes
if(1){
  
  drained_peatex_int2 <- 
    drained_peatex_int %>%
    # Annual rate
    # mutate(drained_weight = drained_weight / 10^3) %>%
    rename(peatextr_weight_tonsperyear = drained_weight) %>%
    # convert units of weight
    # mutate(drained_weight_cumsum = drained_weight_cumsum / 10^3) %>%
    rename(peatextr_weight_cumultons = drained_weight_cumsum) %>%
    # Area
    mutate(drained_area_peat = drained_area_peat * 10^3) %>%
    rename(peatextr_area_km2 = drained_area_peat)

  glimpse(drained_peatex_int2)
  
  # Save processed peat extraction data, for Alex B.
  write.csv(drained_peatex_int2, '../output/results/artif_drainage/peat_extr_mass_area_interpolated_v10.csv')
  }

