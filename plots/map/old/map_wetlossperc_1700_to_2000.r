# fig 2.   overall map

# get mapping theme
source('./scripts/r/plots/themes/map_theme.r')
# get libraries
source('./scripts/r/plots/map_libraries.r')


remwet_Mkm2_stack <- readRDS('./output/results/remwet_Mkm2_stack_wetchimp.rds')

# names(remwet_Mkm2_stack)
# names(temp_rast)


# subset stack of rasters
remwet_rast_1700 <- raster::subset(remwet_Mkm2_stack, grep('1700', names(remwet_Mkm2_stack), value = T))
remwet_rast_2000 <- raster::subset(remwet_Mkm2_stack, grep('2000', names(remwet_Mkm2_stack), value = T))

remwet_rast_1700_mean = mean(remwet_rast_1700)
remwet_rast_2000_mean = mean(remwet_rast_2000)



wetlossrate_1700_to_2000 <- (remwet_rast_2000_mean - remwet_rast_1700_mean) / remwet_rast_1700_mean * 100

wetlossrate_1700_to_2000_robin <- projectRaster(wetlossrate_1700_to_2000, crs=CRS("+proj=robin"))


wetlossrate_1700_to_2000_robin <- as(wetlossrate_1700_to_2000_robin, "SpatialPixelsDataFrame")
wetlossrate_1700_to_2000_robin <- as.data.frame(wetlossrate_1700_to_2000_robin)



#  cut into layers

cutpts <- c(-100 , -75 , -50, -25 , 0, 25, 50, 75, 100)
wetlossrate_1700_to_2000_robin$layercut <- cut(wetlossrate_1700_to_2000_robin$layer, breaks=cutpts)


# replace the categories stings to make them nicer in the legend
wetlossrate_1700_to_2000_robin$layercut <- gsub("\\(|\\]", "", wetlossrate_1700_to_2000_robin$layercut)
wetlossrate_1700_to_2000_robin$layercut <- gsub("\\,", " to ", wetlossrate_1700_to_2000_robin$layercut)
wetlossrate_1700_to_2000_robin <- wetlossrate_1700_to_2000_robin %>% mutate(layercut=ifelse(layercut=="100 to 500",
                                                                                        ">100",layercut))


# set order (from last to first )
lengend_order <- rev(c("-100 to -75", "-75 to -50", "-50 to -25","-25 to 0", "0 to 25", "25 to 50", "50 to 75", "75 to 100"))      



wetlossrate_1700_to_2000_robin$layercut <- factor(wetlossrate_1700_to_2000_robin$layercut, levels = lengend_order)
levels(wetlossrate_1700_to_2000_robin$layercut)



# get the chose
source('./scripts/r/plots/get_country_bbox_shp_for_ggplot_map.r')

nb_cuts <- length(unique(wetlossrate_1700_to_2000_robin$layercut))

cc <- scales::div_gradient_pal(low="red", mid="grey", high="blue",  
                               space="Lab")(seq(0,1,length.out=nb_cuts))



# MAP HISTORICAL CASES COUNTRIES
ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  
  # add raster  
  geom_tile(data=wetlossrate_1700_to_2000_robin, aes(x=x, y=y, fill=layercut)) +
  
  # add outline of background countries
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_manual(values=cc,
                    name="Percentage wetland cover change\n over 1700-2000") +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.title = element_text(size=7, color="black")) +
  theme(plot.margin = unit(c(-5,-1,-5,-1), "mm"))





### Save figure to file --------------------------------------------------------
ggsave('./output/figures/map_wetlossperc_1700_to_2000.png',
       width=178, height=100, dpi=600, units="mm", type = "cairo-png")
dev.off()
