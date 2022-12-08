### ggplot map of remwet




global_map_ggplot <- function(t, pal){

  t <- as(t, "SpatialPixelsDataFrame")
  t <- as.data.frame(t)
  t$layer <- t[,names(t)[1]]
  tno0 <- t[t$layer > 0,]
  
  plot<- ggplot() +
    
    geom_tile(data=t, 
              aes_string(x='x', y='y'), fill='grey90') +
    geom_tile(data=tno0, 
              aes_string(x='x', y='y', fill=names(t)[1])) +
    coord_equal() +
    theme_minimal() +
    scale_fill_distiller(palette=pal, direction=1) +
    theme(legend.position=c(0.1, 0.4),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
 
  return(plot)
   
}