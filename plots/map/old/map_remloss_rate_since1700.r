# get raster of gridcell area, accounting for projection
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")


# read data
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg.rds')


# subset stack of rasters; remwet expressed in Mkm2 per gridcell
remwet_rast_1700 <- sel.by.pattern(remwet_Mkm2_stack, "1700")
remwet_rast_2000 <- sel.by.pattern(remwet_Mkm2_stack, "2000")

# average rasters
remwet_rast_1700_mean = mean(remwet_rast_1700)
remwet_rast_2000_mean = mean(remwet_rast_2000)

# calculate wetloss rate
wetlossrate_1700_to_2000 <- (remwet_rast_2000_mean - remwet_rast_1700_mean) / 300 * 10^6

# mask areas with little change in either direction
wetlossrate_1700_to_2000[wetlossrate_1700_to_2000 < 0.001 & wetlossrate_1700_to_2000 > -0.001] <- NA

# prep for plotting
wetlossrate_1700_to_2000_robin <- prep_raster_into_robin_map(wetlossrate_1700_to_2000)



#  cut into layers
cutpts <- c(-8, -2, -1, 0, 1, 2, 5)
wetlossrate_1700_to_2000_robin$layercut <- cut(wetlossrate_1700_to_2000_robin$layer, breaks=cutpts)



# replace the categories stings to make them nicer in the legend
wetlossrate_1700_to_2000_robin$layercut <- gsub("\\(|\\]", "", wetlossrate_1700_to_2000_robin$layercut)
wetlossrate_1700_to_2000_robin$layercut <- gsub("\\,", " to ", wetlossrate_1700_to_2000_robin$layercut)
# wetlossrate_1700_to_2000_robin <- wetlossrate_1700_to_2000_robin %>% mutate(layercut=ifelse(layercut=="100 to 500",
#                                                                                             ">100",layercut))


# set order (from last to first )
lengend_order <- c("-8 to -2", "-2 to -1","-1 to 0", "0 to 1", "1 to 2", "2 to 5")
wetlossrate_1700_to_2000_robin$layercut <- factor(wetlossrate_1700_to_2000_robin$layercut, levels = lengend_order)



nb_cuts <- length(unique(wetlossrate_1700_to_2000_robin$layercut))
ccD <- scales::div_gradient_pal(low="red", high="blue",  
                               space="Lab")(seq(0,1,length.out=nb_cuts))

#ccD[which(lengend_order=="-1 to -0.1")] <- "grey90"


# MAP HISTORICAL CASES COUNTRIES ===============================================
map_remloss_rate_since1700 <- ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  
  # add raster  
  geom_tile(data=wetlossrate_1700_to_2000_robin, aes(x=x, y=y, fill=layercut)) +
  
  # add outline of background countries
  # geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccD, name="Wetland cover\nchange over\n1700-2000\n(km^2 year-1)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))

map_remloss_rate_since1700


# save plot as object
saveRDS(map_remloss_rate_since1700, './output/results/plot_obj/map_remloss_rate_since1700.rds')


### Save figure to file --------------------------------------------------------
# ggsave('./output/figures/map_wetloss_rate_km2peryear_1700_to_2000.png', map_remloss_rate_since1700,
#        width=178, height=100, dpi=600, units="mm", type = "cairo-png")
# dev.off()
