# extract remwet  value for each polygon =======================================


# get hyde years
hyde <- nc_open('./output/results/hyde_resampled/hyde32_0.5.nc')
hyde_yrs <- sort(hyde$dim$time$vals) # get years
hyde_yrs <- hyde_yrs[hyde_yrs>= 1700]
rm(hyde)
# save as rds
saveRDS(hyde_yrs, "./output/results/hyde_yrs.rds")



# get histcases polygon
histcases <- readRDS("./output/results/histcases_poly.rds")

# get raster stacks of remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/remwet_Mkm2_stack_wetchimp.rds')


rast_list <- c('weta_1_DLEM', 'weta_2_DLEM', 'fmax', 'weta_1_SDGVM', 'weta_2_SDGVM')



# create output df for extracted data
output_df <- data.frame()


histcases <- histcases[1:30,]


for(w in rast_list){


  # subset stack for testing
  remwet_Mkm2_stack_sel <- remwet_Mkm2_stack[[grep(pattern=w, names(remwet_Mkm2_stack))]]
  
  
  for (i in seq(1, nrow(histcases))) {
  
    # get variables of current histcase
    t_country  <- histcases@data[i,"country"]
    t_region   <- histcases@data[i,"name"]
    t_id       <- histcases@data[i,"rec_id"]
    t_yr_start <- histcases@data[i,"yr_start"]
    t_yr_end   <- histcases@data[i,"yr_end"]
    
    # select single polygons
    #histcases_poly <- histcases_poly[!is.na(histcases_poly@data$rec_id),]
    t_cases_poly <- histcases[histcases$rec_id==t_id,] 
    
    # find closest time value in 
    hyde_yr_start <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_start))]
    hyde_yr_end   <- hyde_yrs[which.min(abs(hyde_yrs - t_yr_end))]
    
    # get rasters for start & end years by matching names
    r_start <-remwet_Mkm2_stack_sel[[grep(pattern=hyde_yr_start, names(remwet_Mkm2_stack_sel))]]
    r_end   <-remwet_Mkm2_stack_sel[[grep(pattern=hyde_yr_end, names(remwet_Mkm2_stack_sel))]]
    
    # extract the value from wetloss rasters
    start_extract <- raster::extract(r_start, t_cases_poly, weights=T, normalizeWeights=FALSE, df=T)
    end_extract   <- raster::extract(r_end,   t_cases_poly, weights=T, normalizeWeights=FALSE, df=T)
    
    # calculate sum of wetland area within the polygon at start and end year of the histcase
    sum_r_start <- sum(start_extract[2], na.rm=T)
    sum_r_end   <- sum(end_extract[2], na.rm=T)
    
    # print ticker
    print(paste0(t_country, " - ", t_id, " ", t_yr_start, "-", t_yr_end, " ; ",
                 "hyde: ", hyde_yr_start,"-", hyde_yr_end))
    
    
    # add row to extracted data output df
    output_df <- bind_rows(output_df,
                           
                 data.frame(name           = w,
                            rec_id         = t_id, 
                            t_country      = t_country,
                            year_start     = t_yr_start,
                            year_end       = t_yr_end,
                            hyde_year_start= hyde_yr_start, 
                            hyde_year_end  = hyde_yr_end, 
                            remwet_start   = sum_r_start,
                            remwet_end     = sum_r_end))
  }
}

# write extracted output
write.csv(output_df, "./output/results/histcase_remwet_extracted.csv")
