

### Create map ggplot theme_opts ---------------------------------------------------- 

theme_raster_map <- function(base_size = 6){ 
  
  theme_bw(base_size=base_size) +
    
    theme(plot.title =   element_text(face='bold',size=14,hjust=0),
           
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill='white'),
          plot.background = element_rect(fill='white'),
          panel.border = element_blank(),
          
          axis.text =  element_blank(), 
          axis.title = element_blank(),
          axis.line = element_line(colour='white'),
          axis.ticks = element_blank(),
          
          
          #legend.key =   element_blank(),
          #legend.title = element_blank(),
          #legend.position="bottom", 
          #legend.box="horizontal",
          legend.background = element_blank(),
          legend.title = element_text(size=6, color="black"),
          legend.text=element_text(size=6, color="black"), 
          legend.direction = "vertical", 
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(2.5, "mm")) }
