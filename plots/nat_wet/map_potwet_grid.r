# Description: Makes facet grid of potential wetland 

# /----------------------------------------------------------------------------#
#/    make map matrix of potwet

# Function converting format & proj 
# Question: is the the one that prevents exceeded area in robinson proj?
WGSraster2dfROBIN <- function(r){
  library(terra)
  crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  r <- terra::rast(r)
  r_robin <- terra::project(r, '+proj=robin', method='near', mask=T)
  r_robin_df <- as.data.frame(r_robin, xy=TRUE, na.rm=TRUE) 
  return(r_robin_df) }


# /----------------------------------------------------------------------------#
#/    Compile potwet
potwet_compil_df <- data.frame()

names(preswet_max_stack) <- c('WAD2M', 'GLWD', 'GIEMS2')
names(simwet_stack) <- c('ORCHIDEE', 'SDGVM', 'DLEM', 'LPJ-wsl')

for(p_i in 1:3 ){
  for(s_i in 1:4){
    
    
    preswet <- preswet_max_stack[[p_i]]
    simwet  <- simwet_stack[[s_i]]
    
    potwet <- simwet - preswet
    potwet[potwet<0] <- NA
    potwet[potwet==0] <- NA
    potwet <- potwet / area(potwet)
    potwet[potwet>1] <- 1
    
    potwet_df  <- WGSraster2dfROBIN(potwet)
    potwet_df$simwet <- names(simwet)
    potwet_df$preswet <- names(preswet)
    
    
    potwet_compil_df <- bind_rows(potwet_compil_df, potwet_df)
  }
}



potwet_grid <-
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  geom_tile(data=potwet_compil_df, aes(x=x, y=y, fill=layer)) +
  
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  facet_grid(simwet~preswet) +
  # scale_fill_manual(values = my_palette ) +
  scale_fill_distiller(palette= 'YlGnBu', direction = 1) +
  # scale_fill_steps()
  
  guides(fill = guide_colorsteps(#nbin=10, #raster=F,
    barheight = 0.7, barwidth=10,
    frame.colour=c('black'), frame.linewidth=0.7,
    ticks.colour='black',  direction='horizontal',
    title = expression(paste('Potential wetland (gridcell fraction)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        strip.text = element_text(face = 'bold'))  # size = 12, color = 'red', 

# /----------------------------------------------------------------------------#
#/ save plot 
ggsave(plot= potwet_grid,  
       '../output/figures/potwet_map_grid.png',
       width=190, height=180, dpi=300, units='mm')
