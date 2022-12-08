# output:  filtered drainage data table 

# read the national drainage database
drained <-  read.csv('../data/artif_drained/all_drain_stat_comb_v4.csv') %>% 
            
            filter(exclude != "exclude") %>%
            filter(region  == "") %>%
            filter(unit  == "national") %>%
  
            # remove total drainage stats
            filter(type != "total") %>%
  
            # convert weight to ha:  10t dry peat per hectare
            # mutate(drained_area_tot = ifelse(!is.na(drained_weight), drained_weight /3000 /1000, drained_area_tot)) %>%
  

            # conert area from 1000 ha to km^2  ( / 100 * 1000)
            mutate(drained_area_tot = as.numeric(drained_area_tot)) %>%
            mutate(drained_area_tot = drained_area_tot / 100 * 1000) %>%
  
            # add country codes
            mutate(iso_a3 = countrycode(country,'country.name','iso3c',warn=F)) %>%
            mutate(country_name = countrycode(iso_a3,'iso3c','country.name',warn=F)) %>%
  
            # calc decade
            mutate(decade=round(year,-1)) %>%         
            
            # get the average per type x country x year
            group_by(type, iso_a3, country_name, year, Comment, Source) %>%
            dplyr::select_if(., is.numeric) %>%
            summarise_all(funs(mean)) %>%
            ungroup() %>% 
            select(-id, -Arable.land.and.permanent.crops, -currently_expl) %>% 
            filter(!is.na(country_name))




# filter data to the major drained area timelines 
drained <- drained %>%
            filter(!is.na(drained_area_tot)) %>%   # has non-null data
            filter(drained_area_tot > 0) %>%       # is bigger than 10
            filter(!is.na(year)) %>%               # has a year
            filter(!is.na(iso_a3))                 # is a country code


# after all the filtering, add a unique  record_id  for each row.
drained <- as.data.frame(drained) %>% mutate(rec_id = row.names(drained))
