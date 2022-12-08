# This script constructs Wetcultivation time series as a static fraction of the
# cropland intersecting with wetlands. 

# Questions:  
#     - Does spate irrigation count as wetland cultivation?
#     - Does irrigated lowland include rice? If so, should excludes...


# /----------------------------------------------------------------------------#
#/   Read the 
wc <- read.csv("./output/results/artif_drainage/drained_wetcult_ha.csv") %>%
  
  filter(type %in% c("Spate irrig.",
                     "Cultiv.not-equip.", 
                     "Cultiv.equip.", 
                     "Flood recession"  )) %>%
  
  group_by(country_name, year) %>%
  summarize(drained_area_irrig = sum(drained_area_irrig)) %>%
  ungroup() %>%
  
  # Convert from 1000ha to km^2
  mutate(drained_area_irrig = drained_area_irrig * 1000 / 100) %>%
  # Round year to decade, for joining to HYDE cropland area. 
  mutate(year_rnd = round(year, -1)- 10) %>%
  # Make ISO code column
  mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F))


# Get grid of isocodes
source("./scripts/data_proc/make_grid_isocodes.r")


# Save to file
write.csv(wc, "./output/results/artif_drainage/drained_wetcult_km2_onlydata.csv")





# /----------------------------------------------------------------------------#
#/     Get cropland area at those years
h <- './output/results/hyde_resampled/hyde32_allvars_05deg.nc'

# Read stack of cropland (all years)
c <- stack(h, varname="cropland")


# Sum LU-potwet overlapping area per country
zt <-   as.data.frame(zonal(c, ciso, 'sum')) %>% 
        # convert from wide to long table format
        gather(key="year", value="value", -zone) %>%
        # join the ISO codes
        left_join(., isolookup, by=c("zone"="val")) #%>% 

# rename columns
names(zt) <- c("zone", "year", "cropland_area_km2", "iso_a3")  

# Convert year string "Xyear" to numeric YEAR
zt <- zt %>%  mutate(year = as.numeric(substr(year, 2,5)))


# Calculate the % of cropland in wetlands, get the median
wc2 <- left_join(wc, zt, by=c("iso_a3"="iso_a3", "year_rnd"="year")) %>%
       mutate(perc_wetcult = drained_area_irrig/ cropland_area_km2) %>%
       
      group_by(iso_a3) %>%
      summarize(perc_wetcult = median(perc_wetcult)) %>%
      ungroup()



# Apply the percentage to the full time series
country_wetcult_area <- zt %>%
              left_join(., wc2, by="iso_a3") %>%
              mutate(wetcult_area_km2 = cropland_area_km2 * perc_wetcult)


# Save to file
write.csv(country_wetcult_area, "./output/results/artif_drainage/wetcult_ha_interpol.csv")


