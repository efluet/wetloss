# /----------------------------------------------------------------------------#
#/    Make global map of wetland loss 1700-200


# /----------------------------------------------------------------------------#
#/    get grid area 
#     originally in km2
# get raster of gridcell area, accounting for projection
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif") / 10^6


# /----------------------------------------------------------------------------#
#/    Read wetland loss grid
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg.rds')

# subset to rasters with "1700" in raster name
r_natwet1700 <-remwet_Mkm2_stack[[grep(pattern="1700", names(remwet_Mkm2_stack))]]

# average over stack, then convert to % of pixel
r_natwet1700 <- mean(r_natwet1700) / area


# /----------------------------------------------------------------------------#
#/    Masking

# make mask of areas with little-to-no wetland
zeromask <- r_natwet1700
zeromask[zeromask < 0.01] <- NA

# apply the mask
r_natwet1700 <- mask(r_natwet1700, zeromask)


# /----------------------------------------------------------------------------#
#/   reproject the grid to robinson projection

# declare incoming CSR (should be done wayyyyyyy earlier than this)
crs(r_natwet1700) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
r_natwet1700_robin <- projectRaster(r_natwet1700, crs=CRS("+proj=robin"))

# /----------------------------------------------------------------------------#
#/    reformat rasters  for graph in ggplot
r_natwet1700_robin <- as(r_natwet1700_robin, "SpatialPixelsDataFrame")
r_natwet1700_robin <- as.data.frame(r_natwet1700_robin)



# /----------------------------------------------------------------------------#
#/     cut values into bins (for cleaner discrete colors)

cutpts <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
#pretty_breaks(n = 5)(r_natwet1700_robin$layer)

r_natwet1700_robin$layercut <- cut(r_natwet1700_robin$layer, breaks=cutpts)


# remove symbols from category-strings to make them nicer in the legend
r_natwet1700_robin$layercut <- gsub("\\(|\\]", "", r_natwet1700_robin$layercut)
r_natwet1700_robin$layercut <- gsub("\\,", " to ", r_natwet1700_robin$layercut)
#r_natwet1700_robin <- r_natwet1700_robin %>% mutate(layercut=ifelse(layercut=="800 to 1e+03", ">800",layercut))


# /----------------------------------------------------------------------------#
#/      set order of bins in legend (from last to first )
# lengend_order <- c("-600 to -200", "-200 to -100", "-100 to 0", "0 to 100", "100 to 200", "200 to 600" , ">600")
# lengend_order <- c("-6 to -5", "-5 to -4", "-4 to -3","-3 to -2", "-2 to -1", "-1 to 0", "0 to 1", "1 to 2")
# r_natwet1700_robin$layercut <- factor(r_natwet1700_robin$layercut, levels = lengend_order)
# levels(r_natwet1700_robin$layercut)


# /----------------------------------------------------------------------------#
#/     Create color scales

nb_cuts <- length(unique(r_natwet1700_robin$layercut))

ccB <- scales::div_gradient_pal(low="#b3b3ff", mid="#3333ff", high="#000080",  
                               space="Lab")(seq(0,1,length.out=nb_cuts))



# /----------------------------------------------------------------------------#
#/     MAP HISTORICAL CASES COUNTRIES
map_natwet_in1700ad <- ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90') +
  
  # add raster  
  geom_tile(data=r_natwet1700_robin, aes(x=x, y=y, fill=layercut)) +
  
  # add outline of background countries
  # geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccB, name="Wetland cover\nin 1700AD\n(% gridcells)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))



# save to R-data
saveRDS(map_natwet_in1700ad, './output/results/plot_obj/map_natwet_in1700ad.rds')


# /----------------------------------------------------------------------------#
#/    Save figure to file 
ggsave('./output/figures/nat_remwet_in1700ad.png', map_natwet_in1700ad,
       width=178, height=100, dpi=600, units="mm", type = "cairo-png")
dev.off()
