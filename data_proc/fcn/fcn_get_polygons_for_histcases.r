# create function selecting shapefile for histcases 
#
#  histcases = the table
#  shapefile = shapefile
#  join field from table
#  join field from shapefile
#  id = "rec_id"



get_histcase_shapefile <- function(histcases, shapefile, byx, byy, id){
  
  
  # loop through histcases
  for (i in seq(1, nrow(histcases))) {
    
    # get the record id of the histcase
    t_rec_id       <- histcases[i,id]
    
    print(t_rec_id)
    
    # extract the single histcase row 
    temp_case <- histcases[histcases$rec_id==t_rec_id,] 
    
    # reset the polygon from which selection is made
    shapefile_forjoin <- shapefile
    
    # join the data to the polygons
    shapefile_forjoin <- merge(shapefile_forjoin, temp_case, by.x= byx, by.y= byy)
    
    # remove polygons without match
    t_cases_poly <- shapefile_forjoin[!is.na(shapefile_forjoin@data$rec_id),]
    
    # if the first loop iteration
    if (i==1){
      # create output
      outpoly <- t_cases_poly
      # else append to output
    } else { outpoly <- rbind(outpoly, t_cases_poly, makeUniqueIDs = TRUE) }
  }
  
  
  return(outpoly)
}