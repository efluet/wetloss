


drain_Mkm2_stack <- readRDS('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumulbestvar.rds')
# Save LU stack
# saveRDS(s_cumul_out, paste0('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul',params_type,'.rds'))


# get 7 drainage rasters for year 2000
stack2000 <- drain_Mkm2_stack[[grep(pattern='2000', names(drain_Mkm2_stack))]]

# rename
names(stack2000) <- c("Irrig.Rice", "Wet.Cultiv.", "Cropland",
                      "Urban",  "Pasture",  "Peatland Extr.", "Forestry")

# sum_loss2000 <- sum(stack2000, na.rm = T)

maxdriver <- which.max(stack2000)

sum_loss2000[sum_loss2000 <= 1] <- NA


# mask to areas of highloss
maxdriver <- raster::mask(maxdriver, sum_loss2000)


# /----------------------------------------------------------------------------#
#/     Fig  1-D : MAP DRAINAGE DRIVER

# declare incoming CSR (should be done wayyyyyyy earlier than this)
crs(maxdriver) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
maxdriver_robin <- projectRaster(maxdriver, crs=CRS("+proj=robin"), method='ngb', over=TRUE)

#  reformat rasters  for graph in ggplot
maxdriver_robin <- as(maxdriver_robin, "SpatialPixelsDataFrame")
maxdriver_robin <- as.data.frame(maxdriver_robin)


driver_names <- c("Cropland",
                  "Wet.Cultiv.",
                  "Forestry",
                  "Peatland Extr.", 
                  "Irrig.Rice", 
                  "Pasture", 
                  "Urban")

maxdriver_robin$layer <- factor(maxdriver_robin$layer, 
                                levels=c('3','2','7','6','1','5','4'),
                                labels= driver_names) #levels=driver_names)

# /-----------------------------------
#/   PLOT 
drivermap <- ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90') +
  
  # add raster  
  geom_tile(data=maxdriver_robin, aes(x=x, y=y, fill=layer)) +
  
  # add outline of background countries
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey20', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  coord_equal() +  theme_raster_map() +
  
  scale_y_continuous(limits=c(-6600000, 8953595)) +

  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
        'Wet.Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
        'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
        'Peatland Extr.' = '#ff7f00',#'brown',    # Peatland
        'Irrig.Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
        'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
        'Urban'    = '#a65628'),  #e0dd6e'), # Urban
    name="Driver of\nwetland loss") +

  theme(legend.position = "bottom",
        legend.direction = "horizontal")

drivermap

