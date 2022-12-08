# /----------------------------------------------------------------------------#
#/    Read in WET database                       ------
wetdb <- read.csv("../data/WETindex/WET_database_2017_FINAL_151217_geo.csv", 
                  stringsAsFactors = F) %>%
  
  # select columns that are unique per SITE, not different years of data
  dplyr::select(record_ID, WET.update, Country, Country.scale., Continent.scale.,
                Ramsar.region, WET.Subregion, Locality, Latitude, Longitude,
                Land.cover..from.paper., Natural.artificial,
                Ramsar.type, Ramsar.category, WET.classification,
                WETclass_binomial, Binomial, Management, Discusses.drivers.,
                Drivers, Indicates.what.transition.was.from.to., Transition.to...) %>%
  unique()


wetdb$Latitude <- as.numeric(wetdb$Latitude)
wetdb$Longitude <- as.numeric(wetdb$Longitude)


# /----------------------------------------------------------------------------#
#/    Split DB into point and polygon locations                          -------
wetdb_pts <- wetdb %>% 
  
  filter(Country.scale. != "yes") %>%
  
  # convert coordinates to numeric values
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) %>% 
  
  # remove blank coordinates
  filter(!is.na(Longitude) &  !is.na(Latitude)) %>%
  
  mutate(Longitude=as.numeric(Longitude),
         Latitude=as.numeric(Latitude)) %>%
  
  # correct erroneous coordinates
  mutate(Longitude=ifelse(Longitude>180 | Longitude< -180, Longitude/10, Longitude),
         Latitude=ifelse(Latitude>90 | Latitude< -90, Latitude/10, Latitude))
  


# /----------------------------------------------------------------------------#
#/
wetindex_pts <- SpatialPointsDataFrame(coords=wetdb_pts[c("Longitude","Latitude")], data=wetdb_pts)
crs(wetindex_pts) <- CRS("+proj=longlat +datum=WGS84")
wetindex_pts_robin <- spTransform(wetindex_pts, CRS("+proj=robin"))
wetindex_pts_robin_df <- data.frame(wetindex_pts_robin)


# filter to inland  &  non-man-made  wetlands
wetindex_pts_robin_df <- wetindex_pts_robin_df %>%
  # mutate(wetloss_perc_1700to1970 <- wetloss_perc_1700to1970 * -1) %>% 
  filter(Ramsar.type != 'Human-made') %>% 
  filter(! Land.cover..from.paper. %in%  c('Seagrass','Mangroves', 'Oyster reef')) %>% 
  dplyr::select(-record_ID)


