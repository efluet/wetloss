# /----------------------------------------------------------------------------#
#/   Get input grid drain perLU

f_out <- paste0('../output/results/wetloss/grid/grid_cumul_drain_perlu/grid_cumul_drain_perlu_s', s_i, '_p', p_i,'_t',test_theta, '_', pars, '_v1.csv')
grid_drain_perlu <- read.csv(f_out) %>% 
                    # mutate(sum_drain = wetcultiv+ir_rice+cropland+urban+pasture+peatextr+forestry) %>% 
                    mutate(sum_drain = sum(wetcultiv,ir_rice,cropland,urban,pasture,peatextr,forestry, na.rm = T)) %>% 
                    # filter(sum_drain > 10) %>% 
                    # Set static numeric code for LU; otherwise it varies depending on missing LU
                    # mutate(max_driver_num = as.numeric(as.factor(max_driver))) %>% 
                    mutate(max_driver_num= ifelse(max_driver=='cropland', 1, NA), 
                           max_driver_num= ifelse(max_driver=='forestry', 2, max_driver_num), 
                           max_driver_num= ifelse(max_driver=='ir_rice',  3, max_driver_num), 
                           max_driver_num= ifelse(max_driver=='pasture',  4, max_driver_num),
                           max_driver_num= ifelse(max_driver=='peatextr', 5, max_driver_num), 
                           max_driver_num= ifelse(max_driver=='urban',    6, max_driver_num), 
                           max_driver_num= ifelse(max_driver=='wetcultiv',7, max_driver_num))



### AS RASTER
r <- rasterFromXYZ(as.data.frame(grid_drain_perlu)[, c('x', 'y', 'max_driver_num')])

# Reproject
grid_drain_perlu_robin_df <- WGSraster2dfROBIN(r)

# Make table of lookup for  LU codes
driver_names = c("Cropland", "Forestry", "Rice","Pasture","Peat Extraction","Urban","Wetland Cultiv.")
# Link names with numerical codes
driver_lookup <- data.frame(max_driver_num= c(1:7), max_driver_name = driver_names)

# Join lookup to df, assigning category names
grid_drain_perlu_robin_df <-  grid_drain_perlu_robin_df %>% 
                              left_join(., driver_lookup, by='max_driver_num') %>% 
                              mutate(max_driver_num = as.factor(max_driver_num))


# /----------------------------------------------------------------------------#
#/  GET REMWET PERC FOR MASK
f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '_v1.csv')
grid_remwet_peryear <- read.csv(f) %>% mutate(cumloss_perc = (X1700 - X2020)/X1700) 
# calculate % loss
# grid_remwet_peryear <- bind_cols(grid_remwet_peryear, ciso_df)
grid_remwet_peryear <- bind_cols(grid_remwet_peryear, landarea_df)
grid_remwet_peryear <- 
    grid_remwet_peryear %>% 
    mutate(Fwet1700 = X1700 / landarea)  # Calculate wetland as % of gridcell 
# Make raster from df
r <- rasterFromXYZ(as.data.frame(grid_remwet_peryear)[, c('x', 'y', c('cumloss_perc', 'Fwet1700'))])

# Filter 
grid_remwet_perc_robin_df <-  WGSraster2dfROBIN(r)

# Apply masks used in Fig2b
grid_drain_perlu_robin_df <- 
      grid_drain_perlu_robin_df %>% 
      left_join(., grid_remwet_perc_robin_df, by=c('x','y')) %>% 
      filter(cumloss_perc > map_cumullossperc_floor/100 &
               Fwet1700 > map_Fwet1700_floor/100)


# /----------------------------------------------------------------------------#
#/   Max Driver Robin Map
fig2d <-
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # add max driver raster;  geom_tile
  geom_raster(data=grid_drain_perlu_robin_df, aes(x=x, y=y, fill=max_driver_name)) +  # max_driver_name
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#CC3311',
        'Wetland Cultiv.' = '#33BBEE',
        'Forestry'   = '#228833',
        'Peat Extraction' = '#EE7733',
        'Rice'     = '#AA4499',
        'Pasture'  = '#DDCC77',
        'Urban'    = '#332288'),
    name="Driver of\nwetland loss") +
  

  
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

fig2d


# scale_fill_manual(#labels = driver_names,
#   values =
#     c('Cropland' = '#CC6677',
#       'Wetland Cultiv.' = '#33BBEE',
#       'Forestry'   = '#009988',
#       'Peat Extraction' = '#EE7733',
#       'Rice'     = '#AA4499',
#       'Pasture'  = '#DDCC77',
#       'Urban'    = '#332288'),
#   name="Driver of\nwetland loss") +

# scale_fill_manual(#labels = driver_names,
#   values =
#     c('Cropland' = '#e41a1c',         #'#ff5b4f',  # Cropland
#       'Wetland Cultiv.' = '#377eb8',  # '#507dc7',  # Wetcultiv - blue
#       'Forestry'   = '#4daf4a',     #'#8df0a9',  # Forestry
#       'Peat Extraction' = '#ff7f00',  #'brown',    # Peatland
#       'Rice'     = '#984ea3',      #'#a177e0',  # Irrig Rice
#       'Pasture'  = '#ffff33',   #'#95f5f0',  # Pasture
#       'Urban'    = '#a65628'),  #e0dd6e'), # Urban
#   name="Driver of\nwetland loss") +


