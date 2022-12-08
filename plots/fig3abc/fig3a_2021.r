# Get Davidson data
source('./plots/map/get_davidson_histcase_polygons.r')
# Get WET index sites
source('./plots/map/get_wetindex_points.r')




# /----------------------------------------------------------------------------#
#/  MAP HISTORICAL CASES COUNTRIES                                     ---------

library(ggnewscale)

fig3a_histcase_map <- 
  
  ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  
  # add countries with wetloss data; colored by value
  # geom_polygon(data=histcases_poly_df, aes(long, lat, group=group), alpha=1, fill="blue") +
  geom_polygon(data=histcases_poly_df, aes(long, lat, group=group, fill='Davidson 2014')) +
  scale_fill_manual(values='blue', name='') +
  
  # add outline of country with data 
  geom_path(data=histcases_poly_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Wet Index points
  geom_point(data=wetindex_pts_robin_df, aes(Longitude.1, Latitude.1, 
                                             color='Wetland Extent\nTrends (WET) Index\n(Darrah et al. 2019)'), size=0.02) +
  scale_color_manual(values='red', name='') +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.25) +
  # scale_fill_distiller(palette = 3) +
  
  # new_scale_fill() +

  ## Make fake layer for legend
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  coord_equal() + theme_fig() +
  theme(legend.position=c(0., 0.45),
        # legend.direction = 'vertical',
        legend.box = "vertical",
        legend.key.size = unit(.5, 'cm'),
        legend.background = element_rect(fill=NA,
                                         size=0.2, linetype="solid", 
                                         colour ="white")) +  # "top"
  theme(plot.margin = unit(c(-2,-5,-2, 4), "mm"))


fig3a_histcase_map



# # /----------------------------------------------------------------
# #/ Save figure to file              -----
# ggsave('./output/figures/hist_cases/map_histcase_davidson_wetindex.png',
#        width=87, height=60, dpi=800, units="mm", type = "cairo-png")
# dev.off()




# delete objects
# rm(histcases_poly, histcases_poly_df, histcases_poly_robin, 
#    countries_robin_df, bbox_robin_df, map)
