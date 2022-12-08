# extract remwet  value for each polygon =======================================


# get hyde years
hyde_yrs <- readRDS("./output/results/hyde_yrs_post1700.rds")

# get histcases polygon
histcases <- readRDS("./output/results/histcases_poly.rds")

# get raster stacks of remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/remwet_Mkm2_stack_wetchimp.rds')

# create output df for extracted data
output_df <- data.frame()



# loop through histcases
for (i in seq(1, nrow(histcases))) {

  # get variables of current histcase
  t_country  <- histcases@data[i,"country"]
  t_region   <- histcases@data[i,"region"]
  t_rec_id   <- histcases@data[i,"rec_id"]
  t_yr_start <- histcases@data[i,"yr_start"]
  t_yr_end   <- histcases@data[i,"yr_end"]
  
  
  # select single polygons
  #histcases_poly <- histcases_poly[!is.na(histcases_poly@data$rec_id),]
  t_cases_poly <- histcases[histcases$rec_id==t_rec_id,] 
  
  # find closest time value in hyde years
  hyde_yr_start <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_start))]
  hyde_yr_end   <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_end))]
  
  # get rasters for start & end years by matching names
  r_start <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_start, names(remwet_Mkm2_stack))]]
  r_end   <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_end,   names(remwet_Mkm2_stack))]]    
  
  
  # extract the value from wetloss rasters
  sum_r_start <- raster::extract(r_start, t_cases_poly, fun=sum, na.rm=T, df=T)
  sum_r_end <-   raster::extract(r_end,   t_cases_poly, fun=sum, na.rm=T, df=T)
  
  
  sum_r_start <- sum_r_start %>% gather(rast_source, remwet_start, 2:ncol(sum_r_start))  %>% dplyr::select(-one_of("ID"))
  sum_r_end   <- sum_r_end   %>% gather(rast_source, remwet_end, 2:ncol(sum_r_end)) %>% dplyr::select(-one_of("ID"))
  
  
  # add start & end years to columns
  sum_r_start$hcase_start_year  <- t_yr_start
  sum_r_end$hcase_end_year      <- t_yr_end
  sum_r_start$hyde_start_yr     <- hyde_yr_start
  sum_r_end$hyde_end_yr         <- hyde_yr_end
  
  
  # add row to extracted data output df
  sum_r <- bind_cols(sum_r_start, sum_r_end)  
  
  
  sum_r$rec_id  <- t_rec_id
  sum_r$region  <- t_region
  sum_r$country <- t_country
  
  
  # print ticker
  print(paste0(t_country, " - ", t_rec_id, " ", t_yr_start, "-", t_yr_end, " ; ",
               "hyde: ", hyde_yr_start,"-", hyde_yr_end))
  
  
  # add row to extracted data output df
  output_df <- bind_rows(output_df, sum_r)
  
}


# write extracted output
write.csv(output_df, "./output/results/histcase_remwet_extracted.csv")


# delete objects 
rm(output_df, hyde_yrs, histcases, remwet_Mkm2_stack)
rm(hyde_yr_start, hyde_yr_end, r_start, r_end, sum_r_start, sum_r_end, sum_r)
rm(i, t_country, t_region, t_rec_id, t_yr_start, t_yr_end, t_cases_poly)