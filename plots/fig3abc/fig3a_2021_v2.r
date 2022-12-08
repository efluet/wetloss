# # Get Davidson data
# source('./plots/map/get_davidson_histcase_polygons.r')
# # Get WET index sites
# source('./plots/map/get_wetindex_points.r')


histcases_poly <- readRDS("../data/hist_records/davidson_sites_gis/histcases_wdata_2021.rds") # Updated April 2021


# histcases_poly <- spTransform( histcases_poly, CRS( "+init=epsg:4326" ) )
# histcases_poly = rgeos::gBuffer(histcases_poly, byid=TRUE, width=0)

# project shapefile to Robinson for plotting
histcases_poly_robin <- sp::spTransform(histcases_poly, CRS("+proj=robin"))

# Apply buffer to fix self-intersecting geometry
histcases_poly_robin = rgeos::gBuffer(histcases_poly_robin, byid=TRUE, width=0)

# add id column
histcases_poly_robin@data$id <- as.numeric(rownames(histcases_poly_robin@data))

# histcases_poly_robin = rgeos::gBuffer(histcases_poly_robin, byid=TRUE, width=0)
# set_RGEOS_CheckValidity(histcases_poly_robin)

# convert to df
histcases_poly_df <- fortify(histcases_poly_robin, region = 'id')
# merge the attribute table back to spatial df
histcases_poly_df <- merge(histcases_poly_df, histcases_poly_robin@data, by="id")


histcases_poly_df <- histcases_poly_df %>% filter(!is.na(continent))


# /----------------------------------------------------------------------------#
#/  MAP HISTORICAL CASES COUNTRIES                                     ---------


fig3a_histcase_map <- 
  
  ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  
  # add countries with wetloss data; colored by value
  geom_polygon(data=histcases_poly_df, aes(long, lat, group=group, fill=continent)) +

  # add outline of country with data 
  geom_path(data=histcases_poly_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  # # Wet Index points
  # geom_point(data=wetindex_pts_robin_df, 
  #            aes(Longitude.1, Latitude.1, color='Wetland Extent\nTrends (WET) Index\n(Darrah et al. 2019)'), size=0.02) +
  # scale_color_manual(values='red', name='') +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +

  ## Make fake layer for legend
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  scale_fill_brewer(palette = 'Set1') +
  
  coord_equal() + theme_fig() +
  
  
  theme(legend.position= 'none', #c(0., 0.45),
        # legend.direction = 'vertical',
        legend.box = "vertical",
        legend.key.size = unit(.5, 'cm'),
        legend.background = element_rect(fill=NA,
                                         size=0.2, linetype="solid", 
                                         colour ="white")) +  # "top"
  theme(plot.margin = unit(c(-3,-3,-3, -2), "mm"))
  # theme(plot.margin = unit(c(-0,-1,-1, 1), "mm"))


# fig3a_histcase_map


# 
# # # /----------------------------------------------------------------
# # #/ Save figure to file              -----
# ggsave('../output/figures/hist_cases/map_histcase_davidson_wetindex_2021_v2.png',
#        width=87, height=60, dpi=400, units="mm", type = "cairo-png")
# dev.off()




# delete objects
# rm(histcases_poly, histcases_poly_df, histcases_poly_robin, 
#    countries_robin_df, bbox_robin_df, map)
