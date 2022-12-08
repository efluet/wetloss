# /----------------------------------------------------------------------------#
#/   Read drained area time-series; interpolated from national statistics.
#    Country level

# d = '../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb.csv'
# drained_wetcult_km2_sigmoidpred_nov2020
d = "../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_nov2020.csv"

drainage <- read.csv(d) %>% 
            filter(!is.na(country_name)) %>% 
            mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F)) %>% 
            dplyr::select(-c(X)) %>% 
            unique()

# REMOVE DUPLICATE ROWS - THIS SHOULD HAPPEN UPSTREAM OF HERE !!!
drainage <- drainage[!duplicated(drainage[,c('year','country_name','type')]),]

# Rename types so it matches object names
drainage[drainage$type=='Forestry', 'type'] <- 'forestry'
drainage[drainage$type=='Peat Extraction', 'type'] <- 'peatextr'
drainage[drainage$type=='Cropland', 'type'] <- 'cropland'
drainage[drainage$type=='Wetland Cultiv.', 'type'] <- 'wetcultiv'