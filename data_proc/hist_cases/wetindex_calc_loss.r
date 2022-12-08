# Description: This script calculates %loss per WET index site 

# read in WET database
wetdb <- read.csv("../data/WETindex/WET_database_2017_FINAL_151217_geo.csv", 
                  stringsAsFactors = F) %>%

  # select columns that are unique per SITE, not different years of data
  dplyr::select(record_ID, WET.update, Country, Country.scale., Locality, Year, Time.of.year, Area..ha., Drivers,
                Natural.artificial, WET.classification, Reference) %>%
  mutate(Area..ha. = as.numeric(Area..ha.)) %>% 
  # Remove duplicates
  unique() %>% 
  group_by(record_ID, Country, Country.scale., Locality, Natural.artificial, WET.classification, Reference) %>% 
  summarize(year_start = min(Year),
            year_end = max(Year),
            period_length_years = max(Year) - min(Year),
            Area_change_km2 =  (max(Area..ha.) - min(Area..ha.)) *0.01,
            Area_start_km2 = max(Area..ha.)*0.01,
            perc_loss =  round((max(Area..ha.) - min(Area..ha.))/ max(Area..ha.) *100, 2 )) %>% 
  ungroup()




glimpse(wetdb)


# /----------------------------------------------------------------------------#
#/ Filter 
wetdb_filt <- wetdb %>% 
        dplyr::filter(period_length_years >= 30, 
               Area_start_km2 >= 1000,
               Natural.artificial == 'Natural', 
               WET.classification %in% c('Mixed inland wetland', 'Marshes on peat soils',
                                         'Mixed wetland types', 'Lakes, pools & marshes'))


write.csv(wetdb_filt, "../data/WETindex/WETdb_filt.csv")
 




