
# /----------------------------------------------------------------------------#
#/  CALCULATE % CROPLAND DRAINED PER COUNTRY & YEAR AS CAP FOR PASTURE DRAINAGE


# /----------------------------------------------------------------------------#
#/  Get total area of cropland per country x year 
cropland_nat <- 
        # Bind cropland area griddf to griddf of country IDs
        bind_cols(cropland, ciso_df) %>% 
        dplyr::select(-x, -y) %>% 
        pivot_longer(cols=X1700:X2000, names_to='year', values_to='cropland_area') %>% 
        mutate(year=as.numeric(substring(year, 2, 5))) %>% 
        # filter(!is.na(nat_id)) %>% 
        group_by(iso_a3, year) %>% 
        dplyr::summarise(cropland_area = sum(cropland_area, na.rm=T), .groups="drop") %>% 
        ungroup()


# /----------------------------------------------------------------------------#
#/  Get area of drained cropland

drainage_cropland <-  drainage %>%
        filter(type=='cropland') %>%
        group_by( year, iso_a3) %>% 
        summarise(pred_drained = mean(pred_drained, na.rm=T), .groups="drop") %>% 
        # left_join(., isolookup2, by=c('iso_a3'='iso_a3')) %>%
        ungroup() %>% 
        distinct() %>%
        filter(!is.na(iso_a3)) %>%
        dplyr::select(iso_a3, year, pred_drained)



# /----------------------------------------------------------------------------#
#/   Join cropland cap to drainage data; calculate % drained per country & year
#    use as cap to pasture drainage  -------

cropland_perc_drained <- 
        left_join(cropland_nat, drainage_cropland, by=c('iso_a3'='iso_a3', 'year'='year')) %>% 
        mutate(crop_drain_perc = pred_drained/cropland_area) %>% 
        # Cap % at 1
        mutate(crop_drain_perc = ifelse(crop_drain_perc>1, 1, crop_drain_perc)) %>% 
        mutate(crop_drain_perc = ifelse(is.na(crop_drain_perc), 0, crop_drain_perc)) %>% 
        dplyr::select(iso_a3, year, crop_drain_perc)

