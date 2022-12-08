  

# /----------------------------------------
#/ Get % fractions on map
peatmap <- raster("../data/natwet/xu2018_PEATMAP/PEATMAP_global_0.5deg_fraction.tif") * 100
peat_frac_df <-  WGSraster2dfROBIN(peatmap)
  


# /----------------------------------------
#/  Assign names to numeric codes
peatmapdat <- data.frame(ID=seq(1,6), Name=c('Amazon','Malaysia & Indonesia','Congo','Canada','Siberia','Northern Europe'))

# Get regions
peatmap_df <- read.csv('../output/results/wettype/peatmap_regions.csv')


peatmap_df <-  WGSraster2dfROBIN(peatmap) %>% 
  left_join(., peatmapdat, by=c('layer'='ID')) # layer

  
# /----------------------------------------------------------------------------#
#/   Get regional boxes 
library(sp)

sea_poly <- Polygon(cbind(x=c(95, 120, 120, 95, 95), y=c(-5, -5, 7, 7, -5)))
na_poly  <- Polygon(cbind(x=c(-110, -50, -50, -110, -110), y=c(48, 48, 65, 65, 48)))
sa_poly <- Polygon(cbind(x=c(-80, -55, -55, -80, -80), y=c(-10, -10, 0, 0, -10)))
congo_poly <- Polygon(cbind(x=c(15, 25, 25, 15, 15), y=c(-5, -5, 5, 5, -5)))
si_poly <- Polygon(cbind(x=c(60, 90, 90, 60, 60), y=c(55, 55, 70, 70, 55)))
eu_poly <- Polygon(cbind(x=c(-5, 50, 50, -5, -5), y=c(50, 50, 70, 70, 50)))


Pls <- Polygons(list(sea_poly, na_poly, sa_poly, congo_poly, si_poly, eu_poly), ID=c('peatpoly'))
SPls <- SpatialPolygons(list(Pls))
crs(SPls) =  CRS("+init=epsg:4326")
SPDF_robin <- spTransform(SPls, CRS("+proj=robin"))
SPDF_robin_df <- fortify(as(SPDF_robin, 'SpatialPolygonsDataFrame'))


# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted
library(ggnewscale)

map_peat_regions <- 
  
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +

  # Add peatland fraction
  geom_raster(data=peat_frac_df, aes(x=x, y=y, fill=PEATMAP_global_0.5deg_fraction)) +
  
  scale_fill_gradient(low='grey80', high='grey40',
                      breaks=c(1, 25, 50, 75, 100),
                      limits=c(0, 100)) +
  
  new_scale_fill() +
  
  # Add high wetland regions
  geom_raster(data=peatmap_df, aes(x=x, y=y, fill=Name)) +
  # Region outlines
  geom_path(data=SPDF_robin_df, aes(long, lat, group=group), color='black', size=0.3) +

  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

map_peat_regions




# /----------------------------------------------------------------------------#
#/ SAVE
ggsave('../output/figures/wettype/peatmapregions/map_peat_regions_wframes_v2.png', map_peat_regions,
       width=190, height=100, dpi=300, units='mm') #type = 'cairo-png')
dev.off()

ggsave('../output/figures/wettype/peatmapregions/map_peat_regions_wframes_v2.pdf', map_peat_regions,
       width=190, height=100, dpi=300, units='mm') #type = 'cairo-png')
dev.off()



# /----------------------------------------------------------------------------#
#/  For legend
if(0){
  
  ggplot()+
    
    # Add peatland fraction
    geom_raster(data=peat_frac_df, aes(x=x, y=y, fill=PEATMAP_global_0.5deg_fraction)) +
    
    scale_fill_gradient(low='grey80', high='grey40',
                        breaks=c(1, 25, 50, 75, 100),
                        limits=c(0, 100)) +
    
    guides(fill = guide_colorbar(#nbin=6, raster=F,
                                 barheight = 0.4, barwidth=7,
                                 # reverse=T,
                                 frame.colour=c('black'), frame.linewidth=0.7,
                                 ticks.colour='black',  direction='horizontal', 
                                 title = expression(paste('Peatland cover (% of cell)')))) +
    theme_raster_map() +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal')
  
  
  
  ggsave('../output/figures/wettype/peatland_fraction_map_legend.pdf',
         width=190, height=130, dpi=600, units='mm' )
  dev.off()
  
}
