

# read in WET database
wetdb <- read.csv("./data/WETindex/WET_database_2017_FINAL_151217_geo.csv", 
                  stringsAsFactors = F) %>%

  # select columns that are unique per SITE, not different years of data
  dplyr::select(record_ID, WET.update, Country, Country.scale., Continent.scale.,
                Ramsar.region, WET.Subregion, Locality, Latitude, Longitude,
                Land.cover..from.paper., Natural.artificial,
                Ramsar.type, Ramsar.category, WET.classification,
                WETclass_binomial, Binomial, Management, Discusses.drivers.,
                Drivers, Indicates.what.transition.was.from.to., Transition.to...) %>%
  
  # Remove duplicates
  unique()


glimpse(wetdb)


# /--------------------------------------------------------------------
#/ split DB into point and polygon locations 
wetdb_pts <- wetdb %>% 
  
  filter(Country.scale. != "yes") %>%

  # convert coordinates to numeric values
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) %>% 
  
  # remove blank coordinates
  filter(!is.na(Longitude) &  !is.na(Latitude)) %>%
  
  # correct erroneous coordinates
  mutate(Longitude=ifelse(Longitude>180 | Longitude< -180, Longitude/10, Longitude),
         Latitude=ifelse(Latitude>90 | Latitude< -90, Latitude/10, Latitude)) 


# /----------------------------------------------------------------------------#
#/ KEEP ONLY NATURAL WETLANDS, OVER THE CONTINENT                     ------

###  Process the points to spatial data
wetindex_spdf <- SpatialPointsDataFrame(wetdb_pts[,c("Longitude", "Latitude")], wetdb_pts)

# get the ensemble wetloss grid
wetloss_stack <- readRDS("./output/results/wetloss/grid/wetloss_Mkm2_stack_0.5deg_mean_year.rds")

# calculate percentage loss between 1700 and 1970
wetloss_1700to1970_km2 <- (wetloss_stack$mean_year_1970 - wetloss_stack$mean_year_1700) * 10^6
wetloss_perc_1700to1970 <- wetloss_1700to1970_km2 / (wetloss_stack$mean_year_1700 * 10^6) 


wetloss_perc_1700to1970[wetloss_perc_1700to1970 > 200] <- NA

# Extract value at point locations
wetindex_spdf$wetloss_perc_1700to1970 <- raster::extract(wetloss_perc_1700to1970, wetindex_spdf)

wetindex_df <- data.frame(wetindex_spdf)

# Filter to remove increases in area in area
#wetindex_df <-  wetindex_df %>% filter(wetloss_perc_1700to1970 < 1000)

# save data to file
write.csv(wetindex_df, "./output/results/WETindex_cases_wmappedwetloss1700to1970.csv")
  

# plot the change between 1700-1970 
ggplot() +
  geom_histogram(data=wetindex_df, aes(x=wetloss_perc_1700to1970), fill="red", stat="bin")
  geom_histogram(aes(x=values(wetloss_perc_1700to1970)), fill="blue", alpha=0.3, stat="bin")



#==============================================================================#
###      Process the country polygons            -------------------------------
#==============================================================================#


hist(wetindex_df$wetloss_perc_from1700to1970)

wetdb_poly <- wetdb %>% filter(Country.scale. == "yes")

