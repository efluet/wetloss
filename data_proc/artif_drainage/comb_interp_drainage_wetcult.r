# /-----------------------------------------------------------------------------
#/   Read interpolated drainage area table
drainage <- read.csv('../output/results/artif_drainage/drained_wetcult_km2_sigmoidpred_march2021.csv') #%>% dplyr::select(-X)


# Get Wetcult interpolated
wetcult <- read.csv('../output/results/artif_drainage/wetcult_ha_interpol_v2.csv') %>%
            mutate(type = 'Wetland Cultiv.') %>%
            # remove columns
            dplyr::select(-c(zone, perc_wetcult, cropland_area_km2)) %>% 
            # Make ISO code column
            mutate(country_name = countrycode(iso_a3,'iso3c','country.name',warn=F))


names(wetcult) <- c('year', 'iso_a3', 'pred_drained', 'type', 'country_name')

## Bind them together
drainage_wetcult <- bind_rows(drainage, wetcult)


# Save to file
write.csv(drainage_wetcult, '../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_march2021.csv', row.names=F)
