# library(geosphere)
# library(fasterize)
# 

# 
# # /----------------------------------------------------------------------------#
# #/  Get histcases polygon
# 
# # histcases <- readRDS("../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod.rds") # Manually produced on 9 Sept 2019
# histcases <- readRDS("../data/hist_records/davidson_sites_gis/histcases_wdata_2021.rds") # Updated April 2021
# 
# # exclude polygons without a start date 
# histcases <- histcases[!is.na(histcases$yr_start),]
# 
# histcases <- histcases[histcases$src_id %in% c(31, 92, 129,  136, 144),]
# 
# # Calculate area of polygons from m^2  to  million km2
# histcases$areapoly_mkm2 <- areaPolygon(histcases) /10^5 /10^6
# 
# 
# # /-------------------------------------------------------------
# #/   Convert histcases to raster_df
# histcases_recid = fasterize(st_as_sf(histcases), template, field='src_id')
# histcases_df <- raster2df(histcases_recid)
# names(histcases_df) <- c('src_id','x','y')
# histcases_df <- left_join(maxlncr_df_xy, histcases_df, by=c('x','y'))
# 
# 
# # /----------------------------------------------------------------------------#
# #/   fix to correct continent column for African cases
# histcases_df <- 
#   left_join(histcases_df, histcases@data, by='rec_id') %>%
#   mutate(country = ifelse(country=='Niger/Mali', 'Niger', country)) %>%
#   mutate(country_code = countrycode(country, 'country.name', 'iso3c')) %>%
#   mutate(continent = countrycode(country, 'country.name', 'continent')) %>% 
#   # Round years to decade
#   mutate(yr_start_rnd = round(yr_start, -1), yr_end_rnd = round(yr_end, -1)) %>%           
#   mutate(yr_end_rnd = ifelse(yr_end_rnd==2010, 2000, yr_end_rnd))
# 


histcases_df <- read.csv("../output/results/histcases/histcases_wdata_2021_rasterdf.csv") 
