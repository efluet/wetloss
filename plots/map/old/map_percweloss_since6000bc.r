

# get grid area from Beni's ncdf- originally in m^2; convert to km2 ============================================================
l <- './data/nat_wetland_map/trace21_129.cdf'
area <- raster(l, varname="area") / 10^4




# read remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_2.5deg.rds')


# make a mask of the full extent in LPX =========================================
r_start <-max(remwet_Mkm2_stack[[grep(pattern="6000", names(remwet_Mkm2_stack))]])
r_end <- max(remwet_Mkm2_stack[[grep(pattern="1980", names(remwet_Mkm2_stack))]])

# make function
# introduce na in rst1 for all locations that are non-na in rst2
r_mask <- overlay(r_start, r_end, fun = function(x, y) {
  x[!is.na(y[])] <- 99
  x[!is.na(x[])] <- 1
  return(x) })

# get start and end years
r_start <-remwet_Mkm2_stack[[grep(pattern="6000", names(remwet_Mkm2_stack))]]
r_end <-remwet_Mkm2_stack[[grep(pattern="1700", names(remwet_Mkm2_stack))]]

# replacing NA's by zero
r_start[is.na(r_start[])] <- 0 
r_end[is.na(r_end[])] <- 0 

# average the selected rasters
r_start <- mean(r_start)
r_end <- mean(r_end)


# calc % change from 6000BC
r_percwetloss <- (r_end / r_start) * 100

# mask to the broadest continent mask (incl. glaciers, etc.)
r_percwetloss <- mask(r_percwetloss, r_mask)


r_wetlossrate[r_wetlossrate < 0.1 & r_wetlossrate > -0.1] <- NA
r_wetlossrate[r_wetlossrate < 0.1 & r_wetlossrate > -0.1] <- NA




# reproject the grid to robinson projection
r_wetlossrate_robin <- prep_raster_into_robin_map(r_wetlossrate)
r_mask_robin <- prep_raster_into_robin_map(r_mask)

#  cut into layers
cutpts <- c(-600, -100, -50, -25, 0, 50, 100, 900) #pretty_breaks(n = 5)(r_wetlossrate_robin$layer)
r_wetlossrate_robin$layercut <- cut(r_wetlossrate_robin$layer, breaks=cutpts)


# replace the categories stings to make them nicer in the legend
r_wetlossrate_robin$layercut <- gsub("\\(|\\]", "", r_wetlossrate_robin$layercut)
r_wetlossrate_robin$layercut <- gsub("\\,", " to ", r_wetlossrate_robin$layercut)
#r_wetlossrate_robin <- r_wetlossrate_robin %>% mutate(layercut=ifelse(layercut=="800 to 1e+03", ">800",layercut))


# set order (from last to first )
lengend_order <- c("-600 to -100", "-100 to -50", "-50 to -25", "-25 to 0","0 to 50", "50 to 100", "100 to 900" , ">600")


r_wetlossrate_robin$layercut <- factor(r_wetlossrate_robin$layercut, levels = lengend_order)
levels(r_wetlossrate_robin$layercut)



nb_cuts <- length(unique(r_wetlossrate_robin$layercut))
ccC <- scales::div_gradient_pal(low="red", high="blue",  
                               space="Lab")(seq(0,1,length.out=nb_cuts))



# MAKE GGPLOT2 MAP =============================================================
map_remloss_rate_since6000bc <- ggplot() +
  
  # add background country polygons
  #geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  geom_tile(data=r_mask_robin, aes(x=x, y=y), fill="grey85") +
  
  # add raster  
  geom_tile(data=r_wetlossrate_robin, aes(x=x, y=y, fill=layercut)) +
  
  # add outline of background countries
  #geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccC, name="Wetland cover\nchange over\n6000BC-1700AD \n(km^2 year-1)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))




# saveRDS(map_remloss_rate_since6000bc, './output/results/plot_obj/map_remloss_rate_since6000bc.rds')

### Save figure to file --------------------------------------------------------
# ggsave('./output/figures/map_wetloss_rate_km2peryear_6000BC_to_1700AD.png',
#        width=178, height=100, dpi=600, units="mm", type = "cairo-png")
# dev.off()