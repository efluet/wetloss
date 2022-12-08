
# Extract mapped wetland loss in each polygon            ---------------------

# get hyde years
hyde_yrs <- seq(1700, 2010, 10)

# get histcases polygon
#histcases <- readRDS("./data/hist_records/davidson_sites_gis/davidson_sites_wdata.rds")  # Manually produced
histcases <- readRDS("./data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod.rds")   # Manually produced on Sept 9
#histcases <- readOGR("./data/hist_records/davidson_sites_gis/davidson_sites_wdata.shp")  # Manually produced
#histcases <- readRDS("./data/hist_records/davidson_sites_gis/davidson_sites.rds")  # Manually produced
# histcases <- readOGR("./output/results/histcases_mod/historical_cases_poly_comb2.shp")
# histcases <- readOGR("./output/results/histcases/histcases_mod_sep2019.shp", stringsAsFactors = F)
# histcases <- readRDS("./output/results/histcases/histcases_poly_mod_sep2019.rds")

h=histcases@data

histcases <- histcases[!is.na(histcases$yr_start),]

# get raster stacks of remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg.rds')


# create output df for extracted data
output_df <- data.frame()


# loop through histcases
for (i in seq(1, nrow(histcases))) {

  # get variables of single histcase
  t_country  <- histcases@data[i,"country"]
  t_region   <- histcases@data[i,"region.x"]
  t_src_id   <- histcases@data[i,"src_id"]
  t_rec_id   <- histcases@data[i,"rec_id"]
  t_yr_start <- histcases@data[i,"yr_start"]
  t_yr_end   <- histcases@data[i,"yr_end"]
  wet_type   <- histcases@data[i,"wet_type"]
  
  
  
  # select single histcase polygon
  #histcases_poly <- histcases_poly[!is.na(histcases_poly@data$src_id),]
  t_cases_poly <- histcases[histcases$src_id==t_src_id,]
  
  # if there is no start year, assume it's 1700
  if (length(t_yr_start) == 0){ t_yr_start <- 1700 }
  if (length(t_yr_start) == 0){ t_yr_start <- 2000 }
  
  # find closest time value in hyde years to histcase year
  hyde_yr_start <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_start))]
  hyde_yr_end   <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_end))]
  
  
  # get rasters for start & end years by matching names
  r_start <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_start, names(remwet_Mkm2_stack))]]
  r_end   <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_end,   names(remwet_Mkm2_stack))]]
  
  # extract the value from wetloss rasters
  sum_r_start <- raster::extract(r_start, t_cases_poly, fun=sum, na.rm=T, df=T)
  sum_r_end <-   raster::extract(r_end,   t_cases_poly, fun=sum, na.rm=T, df=T)
  
  
  sum_r_start <- sum_r_start %>% gather(rast_source, remwet_start, 2:ncol(sum_r_start)) %>% dplyr::select(-one_of("ID"))
  sum_r_end   <- sum_r_end   %>% gather(rast_source, remwet_end, 2:ncol(sum_r_end)) %>% dplyr::select(-one_of("ID"))
  
  
  # add start & end years to columns
  sum_r_start$hcase_start_year  <- t_yr_start
  sum_r_end$hcase_end_year      <- t_yr_end
  sum_r_start$hyde_start_yr     <- hyde_yr_start
  sum_r_end$hyde_end_yr         <- hyde_yr_end
  
  
  # add row to extracted data output df
  sum_r <- bind_cols(sum_r_start, sum_r_end)  
  
  sum_r$src_id  <- t_src_id
  sum_r$rec_id <- t_rec_id
  sum_r$region  <- t_region
  sum_r$country <- t_country
  sum_r$wet_type<- wet_type
  
  
  # print ticker
  print(paste0(t_country, " - ", t_src_id, " ", t_yr_start, "-", t_yr_end, " ; ",
               "hyde: ", hyde_yr_start,"-", hyde_yr_end))
  
  
  # add row to extracted data output df
  output_df <- bind_rows(output_df, sum_r)
  
}


# write extracted output
write.csv(output_df, "./output/results/histcase_mappedwetloss_extracted_v3.csv")


# delete objects 
rm(output_df, hyde_yrs, remwet_Mkm2_stack)
rm(hyde_yr_start, hyde_yr_end, r_start, r_end, sum_r_start, sum_r_end, sum_r)
rm(i, t_country, t_region, t_src_id, t_yr_start, t_yr_end, t_cases_poly)




### Prep extracted wetloss-map from overlay         ---------------------------------


f<-"./output/results/histcase_mappedwetloss_extracted_v3.csv"
cs_extract <- read.csv(f, stringsAsFactors = F) %>%
  
  # exclude hiscases starting before 1700
  #filter(hyde_start_yr != hyde_end_yr) %>%
  filter(remwet_start != 0) %>%
  
  # group by combinations of case study and reconstruction
  group_by(src_id, rec_id, region, rast_source) %>%
  
  # sum to combine the multipart polygons (e.g. indonesia)
  mutate(remwet_start= sum(remwet_start),
         remwet_end=   sum(remwet_end)) %>%
  
  distinct(src_id, rast_source, .keep_all=TRUE) %>%
  ungroup() %>%
  
  # calculate % difference
  mutate(map_wetloss_prc = (remwet_start-remwet_end)/remwet_start*100) %>%
  
  # group by columns to keep
  group_by(src_id, rec_id, country, region, hcase_start_year, 
           hyde_start_yr, hcase_end_year, hyde_end_yr) %>%
  
  summarise_at(.vars=c("map_wetloss_prc"), .funs=c("min","mean","max")) %>%
  
  # rename columns
  rename(map_wetloss_prc_min  = min,
         map_wetloss_prc_mean = mean,
         map_wetloss_prc_max  = max) %>%
  
  ungroup() %>%
  
  mutate(label = ifelse(is.na(region),
                        paste0(country,"\n", hyde_start_yr,"-", hyde_end_yr),
                        paste0(region, "\n", hyde_start_yr,"-", hyde_end_yr))) %>%
  
  
  
  ### Rejoin to the polygons

histcases_wmaploss <- merge(histcases, cs_extract, by=c("src_id", "rec_id"))


histcases_wmaploss@data$perc_change_numeric  <- as.numeric(histcases_wmaploss@data$perc_change_numeric)*-1

# convert field
# mutate(perc_change_numeric = as.numeric(perc_change_numeric)*-1) %>%
#   filter(perc_change_numeric > 0)



# save as RDS again
saveRDS(histcases_wmaploss, "./data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod_wmaploss.rds")






# # /----------------------------------------------------------------------------#
# #/   Rejoin the historical cases % loss data                 ------
# # that variable was dropped from the table at some point...
# 
# # get file
# f <- './data/hist_records/wetland_loss_cases_combined_v2_manmod.csv'
# 
# #
# histcases <- read.csv(f, stringsAsFactors = F, na.strings=c("", " ","NA")) %>%
# 
#   filter(compiler == "Davidson 2014") %>%
# 
#   # drop some unused columns
#   dplyr::select(-one_of("X","rec_id","Comment","Source","nb_yrs","perc_change_original",
#                         "ef_comment","full.citation","period")) %>%
# 
#   # convert field
#   mutate(perc_change_numeric = as.numeric(perc_change_numeric)*-1)
# 
# # join mapped and histcase data together =======================================
# cs_joined <- left_join(cs_extract, histcases, by='src_id')
# 
# # keep only hist cases with positive change (loss)
# cs_joined <- cs_joined %>% filter(perc_change_numeric > 0)
# 
# # save to CSV file 
# write.csv(cs_joined , "./output/results/histcase_wetloss_joined.csv")
# 
