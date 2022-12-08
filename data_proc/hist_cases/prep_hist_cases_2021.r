# /----------------------------------------------------------------------------#
#/    GET DAVIDSON2014 CASES  

# read data hist cases Shapefile
hist_poly <- readOGR("../data/hist_records/histcases_2021_simp.shp", GDAL1_integer64_policy=T)

# remove records without a paired source_id
hist_poly <- subset(hist_poly, src_id != 0)  


# /----------------------------------------------------------------------------#
#/  read the data table; manually curated 
cs_joined <- read.csv(paste0('../output/results/histcases/residuals/cs_joined_s', s_i, '_p', p_i, '_t', test_theta, '_', pars, '.csv'))


hist_data <- 
  read.csv("../data/hist_records/histcases_2021.csv") %>%
  dplyr::select(-c(full.citation, Comment, ef_comment)) %>% 
  mutate(continent = countrycode(country, origin='country.name', destination='continent'))

# /----------------------------------------------------------------------------#
#/   join poly and data on id
# histcasesdata <- merge(hist_poly, hist_data, by.x="src_id", by.y="rec_id")
# JULY 1ST 2021 - NOW USING CS_JOINED FOR POLYGON DATA, BC ALL CONTINENT LABELS ARE FIXED
histcasesdata <- merge(hist_poly, cs_joined, by.x="src_id", by.y="rec_id")


# /----------------------------------------------------------------------------#
#/    save as RDS again
saveRDS(histcasesdata, "../data/hist_records/davidson_sites_gis/histcases_wdata_2021.rds")




# /----------------------------------------------------------------------------#
#/  Get histcases polygon

# histcases <- readRDS("../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod.rds") # Manually produced on 9 Sept 2019
histcases <- readRDS("../data/hist_records/davidson_sites_gis/histcases_wdata_2021.rds") # Updated April 2021

# exclude polygons without a start date 
histcases <- histcases[!is.na(histcases$yr_start_rnd),]

# REmove certain polygons; to remove any overlap
histcases <- histcases[!histcases$src_id %in% c(31, 92, 129, 104, 136, 144),]

# Calculate area of polygons from m^2  to  million km2
histcases$areapoly_mkm2 <- areaPolygon(histcases) /10^5 /10^6

# Get area df
histcases_area <- 
    histcases@data %>% 
    dplyr::select(src_id, areapoly_mkm2) %>% 
    group_by(src_id) %>% 
    summarize(areapoly_mkm2= sum(areapoly_mkm2, na.rm=T))





# FIXED POLYGONS - RASTER NOT NEEDED AFTER JULY 1ST 2021
# /----------------------------------------------------------------------------#
#/   Convert histcases to raster_df
# histcases_recid = fasterize(st_as_sf(histcases), template, field='src_id', fun='last')
# histcases_df <- raster2df(histcases_recid)
# names(histcases_df) <- c('src_id','x','y')
# histcases_df <- left_join(maxlncr_df_xy, histcases_df, by=c('x','y'))
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/   fix to correct continent column for African cases
# histcases_df <- 
#   # left_join(histcases_df, histcases@data, by='src_id') %>%  # Because of multipart polygon, this join doesn't work; creates too many rows
#   left_join(histcases_df,   hist_data, by=c('src_id'='rec_id')) %>%
#   left_join(.,   histcases_area, by=c('src_id'='src_id')) %>%
#   mutate(rec_id = src_id) %>%  
#   mutate(country = ifelse(country=='Niger/Mali', 'Niger', country)) %>%
#   mutate(country_code = countrycode(country, 'country.name', 'iso3c')) %>%
#   mutate(continent = countrycode(country, 'country.name', 'continent')) %>% 
#   # Round years to decade
#   mutate(yr_start_rnd = round(yr_start, -1),
#          # yr_start_rnd = floor(yr_start/10) * 10,
#          yr_end_rnd = round(yr_end, -1))            
#   # mutate(yr_end_rnd = ifelse(yr_end_rnd==2010, 2000, yr_end_rnd))   ### ? removed April2021 - why limit to 2000?
# 
# 
# # Save raster df
# write.csv(histcases_df, "../output/results/histcases/histcases_wdata_2021_rasterdf.csv", row.names=FALSE)




