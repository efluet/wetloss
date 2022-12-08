# /----------------------------------------------------------------------------#
#/   Make mask of high-wetland region (for map)

# pres_wet <- raster("../output/results/potwet/pres_wet.tif") / 10^6
pres_wet <- raster('../output/results/natwet/grid/swampsglwd_preswet.tif') #/ 10^6
pixarea <- raster::area(pres_wet)

pres_wet_f <- pres_wet / pixarea
cutoff_f = 0.2
pres_wet_f[pres_wet_f < cutoff_f] <- NA
pres_wet_f[pres_wet_f >= cutoff_f] <- 1
plot(pres_wet_f)

# declare incoming CSR (should be done wayyyyyyy earlier than this)
crs(pres_wet_f) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
pres_wet_f_robin <- projectRaster(pres_wet_f, crs=CRS("+proj=robin"), method='ngb', over=TRUE)

pres_wet_f_robin <- as(pres_wet_f_robin, "SpatialPixelsDataFrame")
pres_wet_f_robin <- as.data.frame(pres_wet_f_robin)


# /----------------------------------------------------------------------------#
#/  GET 2000 Wetloss

# drain_Mkm2_stack <- readRDS('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul.rds')
remwet_Mkm2_stack <- readRDS('../output/results/wetloss/grid/remwet_tot_stack_0.5deg_serialbestvar.rds')

remwet2000 <- remwet_Mkm2_stack[[grep(pattern='2000', names(remwet_Mkm2_stack))]]
remwet1700 <- remwet_Mkm2_stack[[grep(pattern='1700', names(remwet_Mkm2_stack))]]
sum_loss2000 <- remwet1700 - remwet2000



# /----------------------------------------------------------------------------#
#/   reproject the grid to robinson projection

sum_loss2000[sum_loss2000 <= 1] <- NA

# declare incoming CSR (should be done wayyyyyyy earlier than this)
crs(sum_loss2000) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
sum_loss2000_robin <- projectRaster(sum_loss2000, crs=CRS("+proj=robin"), method='ngb', over=TRUE)


# /----------------------------------------------------------------------------#
#/    reformat rasters  for graph in ggplot
sum_loss2000_robin <- as(sum_loss2000_robin, "SpatialPixelsDataFrame")
sum_loss2000_robin <- as.data.frame(sum_loss2000_robin)

sum_loss2000_robin <- sum_loss2000_robin %>% filter(layer>0)

### compress color ramap 
sum_loss2000_robin <- sum_loss2000_robin %>% mutate(layer = ifelse(layer>1000, 1000, layer))
sum_loss2000_robin <- sum_loss2000_robin %>% filter(layer>=10)


# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted

arealossmap <- ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90') +
  
  
  # Add high wetland regions
  geom_tile(data=pres_wet_f_robin, aes(x=x, y=y), fill='#80bcff') +
  
  # Add wetloss raster  
  geom_tile(data=sum_loss2000_robin, aes(x=x, y=y, fill=layer)) +
  
  # add outline of background countries
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey20', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  scale_fill_gradient(low="#ffffcc", high="#e60000", 
                      trans="log", 
                      breaks=c(10^0, 10^1, 10^2, 10^3, 5*10^3, 10^4),
                      labels=c(10^0, 10^1, 10^2, 10^3, 5*10^3, 10^4),
                      limits=c(10^1, 0.1*10^4)) +
  #limits=c(10^0, 2.2*10^4)) +
  
  guides(fill = guide_colorbar(nbin=10, raster=F, 
                               barheight = 0.7, barwidth=10, 
                               frame.colour=c("black"), frame.linewidth=0.7, 
                               ticks.colour="black",  direction="horizontal",
                               title = expression(paste("Wetland area\ndrained or converted (km"^2*")")))) +
  
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

arealossmap
