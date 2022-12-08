# Get Davidson data
source('./plots/map/get_davidson_histcase_polygons.r')
# Get WET index sites
source('./plots/map/get_wetindex_points.r')


# /----------------------------------------------------------------------------#
#/  MAP HISTORICAL CASES COUNTRIES                               ---------

histcase_map <- 
  ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +

  # add outline of background countries
  #geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
    
  # add countries with wetloss data; colored by value
  geom_polygon(data=histcases_poly_df, aes(long, lat, group=group), alpha=1, fill="blue") +
  
  # add outline of country with data 
  geom_path(data=histcases_poly_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Wet Index points
  geom_point(data=wetindex_pts_robin_df, aes(Longitude.1, Latitude.1), color='red', size=0.02) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.25) +
  
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(palette = 3) +
  theme(legend.position="top") +
  theme(plot.margin = unit(c(-2,-1,-2,-1), "mm"))

histcase_map


# # /----------------------------------------------------------------
# #/ Save figure to file              -----
# ggsave('./output/figures/hist_cases/map_histcase_davidson_wetindex.png',
#        width=87, height=60, dpi=800, units="mm", type = "cairo-png")
# dev.off()




# delete objects
rm(histcases_poly, histcases_poly_df, histcases_poly_robin, 
   countries_robin_df, bbox_robin_df, map)
