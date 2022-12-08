
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
r_start[is.na(r_start[])] <- 0.00001
r_end[is.na(r_end[])] <- 0.00001

# replace 0 by NA, because it makes -Inf when divided by it
#r_start[r_start == 0] <- NA

# average the selected rasters
r_start <- mean(r_start)
r_end <- mean(r_end)


# calculate percentage change
r_perc_remwet <- r_end / r_start * 100



# mask to the broadest continent mask (incl. glaciers, etc.)
r_perc_remwet <- mask(r_perc_remwet, r_mask)


# make % wetland at start and end.
no_wet_mask_start <- r_start / (area / 10^6)  * 100
no_wet_mask_end <- r_end / (area / 10^6)  * 100


r_perc_remwet[no_wet_mask_start < 0.02 & no_wet_mask_end < 0.02] <- NA


r_perc_remwet <- mask(r_perc_remwet, r_mask)



# PLOT the map =================================================================

r_perc_remwet <- prep_raster_into_robin_map(r_perc_remwet)


cutpts <- c(0, 25, 50, 75, 95, 105, 1000000)
r_perc_remwet$layercut <- cut(r_perc_remwet$layer, breaks=cutpts, include.lowest=T)


# replace the categories stings to make them nicer in the legend
r_perc_remwet$layercut <- gsub("\\(|\\]", "", r_perc_remwet$layercut)
r_perc_remwet$layercut <- gsub("\\(|\\[", "", r_perc_remwet$layercut)
r_perc_remwet$layercut <- gsub("\\,", " to ", r_perc_remwet$layercut)
r_perc_remwet <- r_perc_remwet %>% mutate(layercut=ifelse(layercut=="105 to 1e+06", ">105",layercut))



#set order (from last to first )
#lengend_order <- rev(c(">100",  "75 to 100", "50 to 75", "25 to 50",  "0 to 25"))
lengend_order <- (c("0 to 25",  "25 to 50","50 to 75", "75 to 95", "95 to 105", ">105"))
r_perc_remwet$layercut <- factor(r_perc_remwet$layercut, levels = lengend_order)
levels(r_perc_remwet$layercut)



nb_cuts <- length(unique(r_perc_remwet$layercut))
#ccD <- scales::div_gradient_pal(low="red", high="lightgreen", space="Lab")(seq(0,1,length.out=nb_cuts))

# make custom color scheme
ccD <- c("#660000", "#cc0000", "#ff3333", "#ff9999", "#80b3ff", "#80ff80")


# map wetland  
#wet_plt <- 
ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90') +
  
  #geom_tile(data=glacier, aes(x=x, y=y), fill='grey80') +
  # add background mask
  geom_tile(data=r_perc_remwet, aes(x=x, y=y, fill=layercut)) +
  
  coord_equal() +
  theme_minimal() +
  # scale_fill_brewer(palette='YlOrRd',direction=-1, 
  #                   name= "Remaining % \nof 1700 wetland \n cover in 2000") +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccD, name="Change in wetland cover\n6000BC-1700AD (%)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))





### save plot ------------------------------------------------------------------
ggsave("./output/figures/map_perc_remwetw_since6000bc.png",
       dpi=800, width=87, height=70, units='mm' , type = "cairo-png")

ggsave("./output/figures/map_perc_remwetw_since6000bc.pdf",
       dpi=800, width=87, height=70, units='mm')

dev.off()

