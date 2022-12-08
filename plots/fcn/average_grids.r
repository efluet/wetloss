
# create a function that averages raster stacks by year
average_grids <- function(year_vector, raster_stack, list_to_subset){ 
  
  # if 
  if (exist(list_to_subset)){
    remwet_Mkm2_stack <- sel.by.pattern(remwet_Mkm2_stack, 
                                        paste(toMatch,collapse="|")) }  
  
  # create empty stack
  mean_stack <- stack()
  
  # loop through years
  for (y in year_vector){
    
    # subset by year
    sel_r <- sel.by.pattern(raster_stack, y)
    
    # average grids
    mean_sel_r <- mean(sel_r)
    
    # name grids
    names(mean_sel_r) <- paste0("mean_year_", y)
    
    # add averaged grid to output stack
    mean_stack <- stack(mean_stack, mean_sel_r)
  }
  
  # return the stack of averaged grids
  return(mean_stack)
}
