# DESCRIPTION ---------------------
# Create A function selecting shapefile for each individual table row 


### ARGUMENTS -----------------
#  histcases = the table
#  shapefile = shapefile
#  join field from table
#  join field from shapefile
#  id = record identifier column  (e.g. "rec_id")


### TEMP VARIABLES FOR DEBUGGING  --------
# table <- drained_type
# byx <- "iso_a3"
# shapefile <- countries
# byy <- "iso_a3"
# id <- "rec_id"



### FUNCTION -----------------
sel_countries_in_shp <- function(table, shapefile, byx, byy, id){
  
  # convert the coutnry code field to character, for later matching
  shapefile@data[,byy] <- as.character(shapefile@data[,byy])
  
  # filter shapefile by adm country codes, reduces the pool to loop from later on
  t <- c(table[, byx])
  subset_shp <- subset(shapefile, iso_a3 %in% t)  # the [[]] removes the names from the  vector
  
  
  # loop through table & extract remwet value for each polygon
  for (i in seq(1, nrow(table))) {
    
    print(i)
    
    # subset row
    temp_row <- table[i,]
      
    # get record id of single
    temp_id <- as.numeric(temp_row[,id])

    # reset the polygon from which selection is made
    shp_forjoin <- subset_shp
    
    # select polygon matching the iso code
    temp_poly <- shp_forjoin[shp_forjoin@data[,byy] == as.character(temp_row[,byx]),]
  
    # print(paste0(shp_forjoin@data[,byy], " ", as.character(temp_row[,byx])))
    
    # if one or more polygon is a match (of ISO code)  
    if (dim(temp_poly)[1] > 0){
      
      # write the id
      temp_poly@data$rec_id <- temp_id
      
      # if the first loop iteration, create the output shp instead of appending
      
      # create output
      if (i==1){ outpoly <- temp_poly 
      
      # else, if not the 1st loop, append the polygon to output
      } else { outpoly <- rbind(outpoly, temp_poly, makeUniqueIDs = TRUE) }
    }
    
  }
  
  # outputs the selected shapefile
  return(outpoly)
}
