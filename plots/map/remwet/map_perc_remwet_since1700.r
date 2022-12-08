# get hydeyrs since 1700
hyde_yrs <-readRDS('./output/results/hyde_yrs/hyde_yrs_since1700.rds')


# get grid area - originally in km2, convert to Mkm2
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif") / 10^6


# read remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg_mean_year.rds')


remwet_2000 <- remwet_Mkm2_stack$mean_year_2000
remwet_1700 <- remwet_Mkm2_stack$mean_year_1700

remwet_2000[remwet_2000 < 0] <- 0
remwet_1700[remwet_1700 < 0] <- 0
remwet_1700[remwet_1700 < 0] <- NA

# calculate % difference between 1700 and 2000 grids
perc_remwet_in2000from1700 <- remwet_2000 / remwet_1700 *100

# remove areas with zero change between the two periods
perc_remwet_in2000from1700[perc_remwet_in2000from1700 == 0] <- NA


# make mask of areas with little-to-no wetland
zeromask <- remwet_Mkm2_stack$mean_year_1700 / area
zeromask[zeromask < 0.2] <- NA


# apply the mask
perc_remwet_in2000from1700 <- mask(perc_remwet_in2000from1700, zeromask)




# PLOT the map =================================================================

perc_remwet_in2000from1700 <- prep_raster_into_robin_map(perc_remwet_in2000from1700)


cutpts <- c(0, 25, 50, 75, 95, 105, 400)
perc_remwet_in2000from1700$layercut <- cut(perc_remwet_in2000from1700$layer, breaks=cutpts, include.lowest=T)


# replace the categories stings to make them nicer in the legend
perc_remwet_in2000from1700$layercut <- gsub("\\(|\\]", "", perc_remwet_in2000from1700$layercut)
perc_remwet_in2000from1700$layercut <- gsub("\\(|\\[", "", perc_remwet_in2000from1700$layercut)
perc_remwet_in2000from1700$layercut <- gsub("\\,", " to ", perc_remwet_in2000from1700$layercut)
perc_remwet_in2000from1700 <- perc_remwet_in2000from1700 %>% mutate(layercut=ifelse(layercut=="75 to 500", ">75",layercut))



#set order (from last to first )
#lengend_order <- rev(c(">100",  "75 to 100", "50 to 75", "25 to 50",  "0 to 25"))
lengend_order <- (c("0 to 25",  "25 to 50","50 to 75", "75 to 95", "95 to 105", "105 to 400"))
perc_remwet_in2000from1700$layercut <- factor(perc_remwet_in2000from1700$layercut, levels = lengend_order)
levels(perc_remwet_in2000from1700$layercut)



nb_cuts <- length(unique(perc_remwet_in2000from1700$layercut))
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
  geom_tile(data=perc_remwet_in2000from1700, aes(x=x, y=y, fill=layercut)) +
  
  coord_equal() +
  theme_minimal() +
  # scale_fill_brewer(palette='YlOrRd',direction=-1, 
  #                   name= "Remaining % \nof 1700 wetland \n cover in 2000") +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccD, name="Wetland cover \nof 1700 lost (%)") +
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))





### save plot ------------------------------------------------------------------
ggsave("./output/figures/map_perc_remwetw_since1700.png",
       dpi=800, width=87, height=70, units='mm' , type = "cairo-png")

ggsave("./output/figures/map_perc_remwetw_since1700.pdf",
       dpi=800, width=87, height=70, units='mm')

dev.off()




# some other weird plots =======================================================

ggplot(perc_remwet_in2000from1700) +
  geom_histogram(aes(x=layer, fill= layercut)) +
  scale_fill_manual(values=ccD) +
  xlab("Wetland cover of 1700 lost (%)") + 
  ylab("pixel count")



y <- stack(remwet_1700, remwet_2000, perc_remwet_in2000from1700)

y <- as(y, "SpatialPixelsDataFrame")
y <- as.data.frame(y)

head(y)


ggplot(y) +
  geom_point(aes(x=mean_year_1700, mean_year_2000)) +
  xlab("wetland  area in 1700 (Mkm^2)") +
  ylab("wetland  area in 2000 (Mkm^2)")
  
ggsave("./output/figures/scatterplot_remwet_in1700n2000.png",
       dpi=800, width=87, height=70, units='mm' , type = "cairo-png")

