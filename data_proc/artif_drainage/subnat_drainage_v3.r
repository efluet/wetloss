
# read the national drainage database
drained_subnat <-  read.csv('../data/artif_drained/all_drain_stat_comb_v7.csv') %>% 
  
  # filter(exclude != "exclude") %>%
  filter(region != '' & HASC_1 != '') %>% 
  filter(type %in% c('Cropland','Peat Extraction', 'Forestry', 'Wetland Cultiv.')) %>% 
  filter(unit == 'subnational') %>% 

  group_by(type, country, region, HASC_1) %>% 
  
  # Within the last 25 years prior to latest data point
  # to use more recent subnational distribution over older ones (i.e. in USA) 
  filter(year > (max(year)-25) | is.na(year)) %>%  # 2
  
  # Calculate percentage of national drainage
  dplyr::summarize(drained_area_tot = mean(as.numeric(drained_area_tot), na.rm=T),
                   drained_weight = mean(drained_weight, na.rm=T),
                   drain_length = mean(as.numeric(drain_length), na.rm=T )) %>%
  ungroup() %>% 
  
  group_by(type, country) %>% 
  
  # Calculate % subnat from length
  mutate(perc_subnat_drain_area_tot     = drained_area_tot / sum(drained_area_tot, na.rm=T)) %>% 
  mutate(perc_subnat_drain_length_tot   = drain_length     / sum(drain_length,     na.rm=T)) %>% 
  mutate(perc_subnat_drained_weight_tot = drained_weight   / sum(drained_weight, na.rm=T))   %>% 
  
  # Combine all in same column
  mutate(perc_subnat_drain = perc_subnat_drain_area_tot) %>%
  mutate(perc_subnat_drain = ifelse(is.na(perc_subnat_drain), perc_subnat_drain_length_tot, perc_subnat_drain)) %>%
  mutate(perc_subnat_drain = ifelse(is.na(perc_subnat_drain), perc_subnat_drained_weight_tot, perc_subnat_drain)) %>%
  
  ungroup() %>%
  dplyr::select(type, country, region, HASC_1, perc_subnat_drain) %>% 

  # Where multiple sub-units are grouped within one HASC1, sum the percentages
  group_by(type, country, HASC_1) %>% 
  mutate(region = paste0(region, collapse = ", ")) %>% 
  dplyr::summarize(perc_subnat_drain = sum(perc_subnat_drain, na.rm=T), region= first(region)) %>% 
  
  # Where mutltiple subdivisions are nested within a data row, unlist into multiple rows
  # The drained are is then divided equally among rows
  mutate(HASC_1 = strsplit(as.character(HASC_1), ", ")) %>% 
  unnest(HASC_1) %>% 
  # Remove 0 drainage; this becomes an assumption that absent subnational areas are 0s 
  # filter(perc_subnat_drain_area_tot > 0)
  mutate(subnat_id = as.numeric(as.factor(HASC_1))) %>% 
  mutate(nat_id    = as.numeric(as.factor(country))) %>% 
  mutate(iso_a3 = countrycode(country, 'country.name','iso3c',warn=F))


# /----------------------------------------------------------------------------
# Save as CSV
write.csv(drained_subnat, '../data/artif_drained/all_drain_stat_comb_v7_subnatperc.csv', row.names = F)



# get list of countries-types with subnational data
# l <-  drained_subnat %>% 
#       dplyr::select(type, country) %>% 
#       distinct()


# Prep data for the map --------------------------------------------------------
library(sf); library(fasterize)

# Read subnat polygons
gadm1_st <- st_read("../data/gadm36", "gadm36_1")


# Loop ------------------------------------------------------
# Save spatial subnat data per type
type_ls = c('Cropland', 'Forestry', 'Peat Extraction', 'Wetland Cultiv.')

for (t in type_ls){
  
  # Filter by type (bc diff countries have diff subnational drainainge data)
  drained_subnat_type <- drained_subnat %>% filter(type==t)# %>% filter(country=='Finland')
  
  ### Join data & filter down to 
  gadm1_st_f <- inner_join(gadm1_st, drained_subnat_type, by='HASC_1') %>% 
                dplyr::select(NAME_0, NAME_1, HASC_1, type, country, region, perc_subnat_drain)
  
  # THIS NEEDS TO BE SEPARATE FROM PREV STEPS BC IT WON'T FUCKING REALIZE THE NB OR ROWS HAS CHANED
  gadm1_st_f <- gadm1_st_f %>% 
                mutate(subnat_id = c(1:nrow(gadm1_st_f))) %>% 
                mutate(nat_id = as.numeric(as.factor(country)))
  
  #-------------------------------------------------------
  # Save shapefile
  st_write(gadm1_st_f, paste0('../output/results/artif_drainage/subnational/poly/subnat_perc_', t, '.shp'), delete_dsn = T)
  
  
  
  ### FASTERIZE PERCENTAGE DRAINAGE
  gadm1_st_f_r_perc <- fasterize(gadm1_st_f, template, field = "perc_subnat_drain", fun="first")
  ### FASTERIZE SUBNAT ID
  gadm1_st_f_r_subnat_id <- fasterize(gadm1_st_f, template, field = "subnat_id", fun="first")
  ### FASTERIZE NAT ID
  gadm1_st_f_r_nat_id <- fasterize(gadm1_st_f, template, field = "nat_id", fun="first")
  
  # Stack rasters 
  gadm1_st_f_r <- stack(gadm1_st_f_r_nat_id, gadm1_st_f_r_subnat_id, gadm1_st_f_r_perc)
 
  # Save raster
  writeRaster(gadm1_st_f_r, paste0('../output/results/artif_drainage/subnational/raster/subnat_perc_', t, '.tif'), overwrite=TRUE)
  
}





# gadm1_st_f <- gadm1_st_f %>% mutate(subnat_id = c(1:nrow(gadm1_st_f)))
# add_rownames(var = "subnat_id") %>% 
# mutate(subnat_id = as.numeric(subnat_id))