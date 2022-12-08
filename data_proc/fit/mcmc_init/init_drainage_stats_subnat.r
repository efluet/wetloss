# /----------------------------------------------------------------------------#
#/   Read drained area time-series; interpolated from national statistics.

# d = "../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_nov2020.csv"
d = "../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_march2021.csv"


drainage <- read.csv(d) %>% 
            filter(!is.na(country_name)) %>% 
            mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F)) %>% 
            # dplyr::select(-c(X)) %>% 
            unique() %>% 
            filter(!is.na(pred_drained))

# REMOVE DUPLICATE ROWS - THIS SHOULD HAPPEN UPSTREAM OF HERE !!!
drainage <- drainage[!duplicated(drainage[,c('year','country_name','type')]),]



# /----------------------------------------------------------------------------#
#/   READ SUBNAT
drainage_subnat <- read.csv('../data/artif_drained/all_drain_stat_comb_v7_subnatperc.csv')
# glimpse(drainage_subnat)


# /----------------------------------------------------------------------------#
#/  JOIN NAT & SUBNAT DATA
#   Join subnat % of drainage to National Area drained
drainage <- left_join(drainage, drainage_subnat, by=c('iso_a3', 'type')) %>% 
            # calculate subnational drainage 
            mutate(pred_drained = ifelse(!is.na(perc_subnat_drain), pred_drained * perc_subnat_drain, pred_drained)) %>%
            # mutate(pred_drained_subnat = ifelse(!is.na(perc_subnat_drain), pred_drained * perc_subnat_drain, pred_drained)) %>%
            # Remove rows without a numeric country code 
            filter(!is.na(iso_a3)) %>% 
            filter(!is.na(pred_drained)) %>%
  
            # TEMP FIX - THERE ARE SUBNAT DUPLICATES IN THE DRAINAGE STATS DATABASE
            group_by(year, country_name, type, continent, iso_a3, country, HASC_1, subnat_id, nat_id) %>% 
            dplyr::summarise(pred_drained        = mean(pred_drained, na.rm=T)) %>% 
                             # pred_drained_subnat = mean(pred_drained_subnat, na.rm=T)) %>% 
            ungroup()



# /----------------------------------------------------------------------------#
#/   Rename types so it matches object names
drainage[drainage$type=='Forestry', 'type'] <- 'forestry'
drainage[drainage$type=='Peat Extraction', 'type'] <- 'peatextr'
drainage[drainage$type=='Cropland', 'type'] <- 'cropland'
drainage[drainage$type=='Wetland Cultiv.', 'type'] <- 'wetcultiv'

