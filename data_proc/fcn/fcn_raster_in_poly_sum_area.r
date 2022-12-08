# Description: loop through polygons, find closest HYDE year, 
# then extract the total area from raster within each polygon.



### Temporary variables 
# polygons <- artdrain_forest_natpoly
# raster_stack <- wood_harvest
# year_vector <- c(1500,1600,1700,seq(1800,2000,10))



get_raster_closest_yr <- function(draintype, polygons, raster_stack, year_vector){
  
  # create output df for extracted data
  output_df <- data.frame()
  
  
  
  # loop through histcases
  for (i in seq(1, nrow(polygons))) {
    
    # select single histcase polygon
    temp_poly <- polygons[i,]
    
    # find closest time value in hyde years to histcase year
    closest_year <- year_vector[which.min(abs(year_vector - temp_poly$year))]
    
    #t <- match(closest_year, year_vector)
    
    # get rasters of closest year (by matching names)
    temp_raster <-raster_stack[[grep(pattern=closest_year, names(raster_stack))]]
    
    
    # extract the value from wetloss rasters             -----------------------
    
    sum_raster <- raster::extract(temp_raster, temp_poly, fun=sum, na.rm=T, df=T)
    names(sum_raster) <- c("id", draintype)
    
    # add row to extracted data output df
    sum_r <- bind_cols(temp_poly@data, sum_raster[draintype], data.frame(closest_lu_year = closest_year))  
    
    # print ticker
    print(paste0(temp_poly@data$country_name, " in ", closest_year, ":  ", round(sum_raster[draintype], 1), " km^2"))
    
    # add row to extracted data output df
    output_df <- bind_rows(output_df, sum_r)
    
  }
  
  return(output_df)
}