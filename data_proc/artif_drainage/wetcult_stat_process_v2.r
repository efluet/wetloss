# This script constructs Wetcultivation time series as a static fraction of the
# cropland intersecting with wetlands. 
# Questions:  
#     - Does spate irrigation count as wetland cultivation?
#     - Does irrigated lowland include rice? If so, should excludes...


# /----------------------------------------------------------------------------#
#/ Get grid of isocodes
source("./data_proc/distrib_drainage/make_grid_isocodes.r")


# /----------------------------------------------------------------------------#
#/     Get cropland area at those years

h <- '../output/results/hyde_resampled/hyde32_allvars_05deg_to2020.nc'

# Read stack of cropland (all years)
c <- stack(h, varname="cropland")

# Rename last column (it's technically from 2017 in hyde)
names(c[[nlayers(c)]]) <- 'X2020'

# Sum LU area per country
zt <-   as.data.frame(zonal(c, ciso, 'sum')) %>% 
        # convert from wide to long table format
        gather(key="year", value="value", -zone) %>%
        # join the ISO codes
        left_join(., isolookup, by=c("zone"="val")) #%>% 

# rename columns
names(zt) <- c("zone", "year", "cropland_area_km2", "iso_a3")  

# Convert year string "Xyear" to numeric YEAR
zt <- zt %>%  mutate(year = as.numeric(substr(year, 2, 5)))


# FIX ADDED MARCH 2021 - REPLICATE CROPLAND FOR 2010 AND 2020
# JUNE 2021 - REMOVED BC THE HYDE STACK WAS EXTENDED TO 2020
# zt <- zt %>%  bind_rows(., zt %>% filter(year==2000) %>% mutate(year=2020))


# /----------------------------------------------------------------------------#
#/   Read the wetland cultivation data table
wc <- read.csv('../data/artif_drained/all_drain_stat_comb_v7.csv') %>%
        filter(type == c('Wetland Cultiv.')) %>% 
        # Get the sum of different types of wetland cultivation 
        group_by(type, country, iso_a3, year) %>% 
        summarise(drained_area_tot_km2 = sum(drained_area_tot_km2, na.rm=T)) %>% 
        ungroup() %>% 
        mutate(decade= round(year, -1)) 
        

# Calculate the % of cropland in wetlands, get the maximum perc. (because many years are partial)
wc2 <- left_join(wc, zt, by=c("iso_a3"="iso_a3", "decade"="year")) %>%
       mutate(perc_wetcult = drained_area_tot_km2 / cropland_area_km2) %>%
       group_by(iso_a3) %>%
       summarise(perc_wetcult = median(perc_wetcult, na.rm=T)) %>%
        # summarize(perc_wetcult = median(perc_wetcult, probs=c(0.75), na.rm=T), type=4) %>%
       ungroup()

# Apply the percentage to the full time series
country_wetcult_area <- zt %>%
                        left_join(., wc2, by="iso_a3") %>%
                        mutate(wetcult_area_km2 = cropland_area_km2 * perc_wetcult)

# /------------------------------
#/ Save to file
write.csv(country_wetcult_area, "../output/results/artif_drainage/wetcult_ha_interpol_v2.csv", row.names=F)
