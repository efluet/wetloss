
# get grid area - originally in m^2; convert to km2
area <- raster(l, varname="area") / 10^4 / 10^6


remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_2.5deg.rds')

r_natwet6000bc <-remwet_Mkm2_stack[[grep(pattern="6000", names(remwet_Mkm2_stack))]]

r_natwet6000bc <- mean(r_natwet6000bc) / area

#r_natwet6000bc <- crop(r_natwet6000bc_robin, extent(bbox))

# reproject the grid to robinson projection
crs(r_natwet6000bc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
r_natwet6000bc_robin <- projectRaster(r_natwet6000bc, crs=CRS("+proj=robin"))

# prepare grid for graph in ggplot
r_natwet6000bc_robin <- as(r_natwet6000bc_robin, "SpatialPixelsDataFrame")
r_natwet6000bc_robin <- as.data.frame(r_natwet6000bc_robin)



#  cut into layers


cutpts <- c(0, 0.05, 0.1, 0.2, 0.4, 0.8)
r_natwet6000bc_robin$layercut <- cut(r_natwet6000bc_robin$layer, breaks=cutpts)
# replace the categories stings to make them nicer in the legend
r_natwet6000bc_robin$layercut <- gsub("\\(|\\]", "", r_natwet6000bc_robin$layercut)
r_natwet6000bc_robin$layercut <- gsub("\\,", " to ", r_natwet6000bc_robin$layercut)
#r_natwet6000bc_robin <- r_natwet6000bc_robin %>% mutate(layercut=ifelse(layercut=="800 to 1e+03", ">800",layercut))



r_natwet6000bc_robin$layercut[r_natwet6000bc_robin$layer <= 0.01] <- NA



# set order (from last to first )
# lengend_order <- c("-600 to -200", "-200 to -100", "-100 to 0", "0 to 100", "100 to 200", "200 to 600" , ">600")
# lengend_order <- c("-6 to -5", "-5 to -4", "-4 to -3","-3 to -2", "-2 to -1", "-1 to 0", "0 to 1", "1 to 2")
# r_natwet6000bc_robin$layercut <- factor(r_natwet6000bc_robin$layercut, levels = lengend_order)
# levels(r_natwet6000bc_robin$layercut)



nb_cuts <- length(unique(r_natwet6000bc_robin$layercut))

ccA <- scales::div_gradient_pal(low="#ccccff", mid="#6666ff", high="#0000e6",  
                               space="Lab")(seq(0,1,length.out=nb_cuts))



# MAP HISTORICAL CASES COUNTRIES
map_natwet_in6000bc <- ggplot() +
  
  # add background country polygons
  # geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  geom_tile(data=r_mask_robin, aes(x=x, y=y), fill="grey85") +
  
  # add raster  
  geom_tile(data=r_natwet6000bc_robin, aes(x=x, y=y, fill=layercut)) +
  
  # add outline of background countries
  # geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccA, name="Wetland cover\nin 6000BC\n(% gridcells)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))



# save object
# saveRDS(map_natwet_in6000bc, './output/results/plot_obj/map_natwet_in6000bc.rds')


# ### Save figure to file --------------------------------------------------------
# ggsave('./output/figures/nat_remwet_in6000bc.pdf', map_natwet_in6000bc,
#        width=178, height=100, dpi=600, units="mm")#, type = "cairo-pdf")
# dev.off()