# /----------------------------------------------------------------------------#
#/   Make mask of high-wetland region (for map)


lu_cumul_drained <- bind_cols(lu_cumul_drained, cumul_drained_for_fig2c_map) %>% 
                    mutate(max_driver_num = as.numeric(as.factor(max_driver))) %>% 
                    filter(X2000 > 10)


### AS RASTER
r <- rasterFromXYZ(as.data.frame(lu_cumul_drained)[, c('x', 'y', 'max_driver_num')])
crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
lu_cumul_drained_robin <- projectRaster(r, crs=CRS('+proj=robin'), method='ngb', over=TRUE)

lu_cumul_drained_robin <- as(lu_cumul_drained_robin, 'SpatialPixelsDataFrame')
lu_cumul_drained_robin_df <- as.data.frame(lu_cumul_drained_robin)

glimpse(lu_cumul_drained_robin_df)

driver_lookup <- data.frame(max_driver_num= c(1:7),
           max_driver_name=c("Cropland", "Forestry", "Irrig.Rice","Pasture","Peatland Extr.","Urban","Wet.Cultiv."))


lu_cumul_drained_robin_df <- lu_cumul_drained_robin_df %>% 
                              left_join(., driver_lookup, by='max_driver_num') 


# /-----------------------------------
#/   PLOT 
drivermap <-
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # add max driver raster  
  geom_tile(data=lu_cumul_drained_robin_df, aes(x=x, y=y, fill=max_driver_name)) +
  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  
  coord_equal() +  theme_raster_map() +
  
  # scale_y_continuous(limits=c(-6600000, 8953595)) +

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




#
# filter(!is.na(max_driver_name))

# get 7 drainage rasters for year 2000
# stack2000 <- drain_Mkm2_stack[[grep(pattern='2000', names(drain_Mkm2_stack))]]

# rename
# names(stack2000) <- c("Irrig.Rice", "Wet.Cultiv.", "Cropland", "Urban",  "Pasture",  "Peatland Extr.", "Forestry")

# sum_loss2000 <- sum(stack2000, na.rm = T)

# maxdriver <- which.max(stack2000)

# sum_loss2000[sum_loss2000 <= 1] <- NA


# mask to areas of highloss
# maxdriver <- raster::mask(maxdriver, sum_loss2000)


# /----------------------------------------------------------------------------#
#/     Fig  1-D : MAP DRAINAGE DRIVER
# 
# # declare incoming CSR (should be done wayyyyyyy earlier than this)
# crs(maxdriver) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# maxdriver_robin <- projectRaster(maxdriver, crs=CRS("+proj=robin"), method='ngb', over=TRUE)
# 
# #  reformat rasters  for graph in ggplot
# maxdriver_robin <- as(maxdriver_robin, "SpatialPixelsDataFrame")
# maxdriver_robin <- as.data.frame(maxdriver_robin)
# 
# 
# driver_names <- c("Cropland",
#                   "Wet.Cultiv.",
#                   "Forestry",
#                   "Peatland Extr.", 
#                   "Irrig.Rice", 
#                   "Pasture", 
#                   "Urban")
# 
# maxdriver_robin$layer <- factor(maxdriver_robin$layer, 
#                                 levels=c('3','2','7','6','1','5','4'),
#                                 labels= driver_names) #levels=driver_names)
