# Prep peatland histcases


# /-----------------------------------------------------------------
#/  PEATLANDS
# cs_peat = 1
# if (cs_peat==1){}

# read data hist cases Shapefile
hist_peat_poly <- readOGR("../data/hist_records/histcases_peat_simp.shp" , GDAL1_integer64_policy=T)

# remove records without a paired source_id
hist_peat_poly <- subset(hist_peat_poly, rec_id != 0)


# /----------------------------------------------------------------------------#
#/  read the data table; manually curated 
hist_data <- 
  read.csv("../data/hist_records/histcases_2021.csv") %>%
  dplyr::select(-c(full.citation, Comment, ef_comment))


# /----------------------------------------------------------------------------#
#/   join poly and data on id
hist_peat <- merge(hist_peat_poly, hist_data, by.x="rec_id", by.y="rec_id")

# exclude polygons without a start date 
hist_peat <- hist_peat[!is.na(hist_peat$yr_start),]

# Calculate area of polygons from m^2  to  million km2
hist_peat$areapoly_mkm2 <- areaPolygon(hist_peat) /10^5 /10^6

# Get area df
histpeat_area <- 
  hist_peat@data %>% 
  dplyr::select(rec_id, areapoly_mkm2) %>% 
  group_by(rec_id) %>% 
  summarize(areapoly_mkm2= sum(areapoly_mkm2, na.rm=T))



# /----------------------------------------------------------------------------#
#/   Convert histcases to raster_df
hist_peat_recid = fasterize(st_as_sf(hist_peat), template, field='rec_id', fun='last')
hist_peat_df <- raster2df(hist_peat_recid)
names(hist_peat_df) <- c('rec_id','x','y')
hist_peat_df <- left_join(maxlncr_df_xy, hist_peat_df, by=c('x','y'))



# /----------------------------------------------------------------------------#
#/   fix to correct continent column for African cases
hist_peat_df <- 
  left_join(hist_peat_df, hist_data, by=c('rec_id'='rec_id')) %>%  
  left_join(., histpeat_area, by='rec_id') %>%  
  mutate(country_code = countrycode(country, 'country.name', 'iso3c')) %>%
  mutate(continent = countrycode(country, 'country.name', 'continent')) %>% 
  # Round years to decade
  mutate(yr_start_rnd = round(yr_start, -1), yr_end_rnd = round(yr_end, -1)) %>%           
  mutate(yr_end_rnd = ifelse(yr_end_rnd==2010, 2000, yr_end_rnd))


# Save raster df
write.csv(hist_peat_df, "../output/results/histcases/histpeat_wdata_2021_rasterdf.csv", row.names=FALSE)



