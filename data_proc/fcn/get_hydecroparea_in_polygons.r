# Description: 
# loop through polygons, find closest HYDE year, 
# then extract the total area from raster within each polygon.



get_raster_closest_yr <- function(draintype, polygons, raster_stack){
  
  # create output df for extracted data
  output_df <- data.frame()
  
  # loop through histcases (combinations of locations x period)
  for (i in seq(1, nrow(polygons))) {
    
    # select single histcase polygon
    temp_poly <- polygons[i,]
    
    # find closest time value in hyde years to histcase year
    closest_hyde_year <- hyde_yrs[which.min(abs(hyde_yrs - temp_poly$year))]
    
    # get the index of the closest year
    t <- match(closest_hyde_year, hyde_yrs)
    
    # get rasters of closest year (by matching names)
    temp_raster <-raster_stack[[grep(pattern=closest_hyde_year, names(raster_stack))]]
    #temp_raster <- raster(h, varname=draintype, band = hyde_indx[t], level=4)      # get cropland; in km2
    
    # extract the value from wetloss rasters
    sum_raster <- raster::extract(temp_raster, temp_poly, fun=sum, na.rm=T, df=T)
    
    # add row to extracted data output df
    sum_r <- bind_cols(temp_poly@data, sum_raster[draintype], data.frame(closest_lu_year = closest_hyde_year))  
    
    # print ticker
    print(paste0(temp_poly@data$country_name, " in ", closest_hyde_year, ":  ", round(sum_raster[draintype]), " km^2"))
    
    
    # add row to extracted data output df
    output_df <- bind_rows(output_df, sum_r)
    
  }
  return(output_df)
}