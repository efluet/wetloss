
# /----------------------------------------------------------------------------#
#/  MAP CUMUL DRAIN                                         ----------

# Bind to country code column, if columns have not been added yet 
if(ncol(grid_cumul_drain) < (33 + 5)) { grid_cumul_drain <- bind_cols(grid_cumul_drain, ciso_df) }


ggplot(grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=X2020)) +
  geom_tile(data= subset(grid_cumul_drain, X2020 <1), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='YlOrRd', direction=0.001, na.value="grey90") +
  theme_raster_map()

# Total cumulative drainage
sum(grid_cumul_drain$`X2020`)/10^6



# /-------------------------------------------------------------------
#/ P
# Bind to country code column, if columns have not been added yet 
if(ncol(grid_drain_peryear) < (33 + 5)){
  grid_drain_peryear <- bind_cols(grid_drain_peryear, ciso_df) }



# Calculate total drainage between each year and 2020
diff_drain <- grid_drain_peryear[,1:33] %>% mutate_all( ~ X2020 - .)

# Add drainage to preswet, to calculate remwet at each timestep
remwet <- diff_drain + preswet_df

glimpse(total_drain_perlu_peryear)

glimpse(grid_cumul_drain)



# /-------------------------------------------------------------------
#/ PLOT RASTER OF EACH LU DRIVER DRAINAGE


cropland_grid_cumul_drain <- cbind(cropland_grid_cumul_drain, ciso_df)

ggplot(cropland_grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=cropland_grid_cumul_drain)) +
  geom_tile(data= subset(cropland_grid_cumul_drain, cropland_grid_cumul_drain <000.1), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value="grey90") +
  theme_raster_map()

#-------------
#/ Peat extraction map
peatextr_grid_cumul_drain <- cbind(peatextr_grid_cumul_drain, ciso_df)

ggplot(peatextr_grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=peatextr_grid_cumul_drain)) +
  geom_tile(data= subset(peatextr_grid_cumul_drain, peatextr_grid_cumul_drain ==0), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='Blues', direction=1, na.value="grey90") +
  theme_raster_map()

#-------------
forestry_grid_cumul_drain <- cbind(forestry_grid_cumul_drain, ciso_df)
ggplot(forestry_grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=forestry_grid_cumul_drain)) +
  geom_tile(data= subset(forestry_grid_cumul_drain, forestry_grid_cumul_drain<.1), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='Greens', direction=1, na.value="grey90") +
  theme_raster_map()

a <- forestry_grid_cumul_drain  %>% group_by(country_name) %>% summarize(sum_forestry=round(sum(forestry_grid_cumul_drain, na.rm=T))) %>% filter(sum_forestry>0)

#------------------------
ir_rice_grid_cumul_drain <- cbind(ir_rice_grid_cumul_drain, ciso_df)
ggplot(ir_rice_grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=ir_rice_grid_cumul_drain)) +
  geom_tile(data= subset(ir_rice_grid_cumul_drain, ir_rice_grid_cumul_drain==0), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='Purples', direction=1, na.value="grey90") +
  theme_raster_map()

#------------------------
# Pasture
pasture_grid_cumul_drain <- cbind(pasture_grid_cumul_drain, ciso_df)
ggplot(pasture_grid_cumul_drain) +
  geom_tile(aes(x=x, y=y, fill=pasture_grid_cumul_drain)) +
  geom_tile(data= subset(pasture_grid_cumul_drain, pasture_grid_cumul_drain==0), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='YlOrBr', direction=1, na.value="grey90") +
  theme_raster_map()

# Wet Cyltiv
ggplot(lu_cumul_drained) +
  geom_tile(aes(x=x, y=y, fill=wetcultiv)) +
  geom_tile(data= subset(lu_cumul_drained, wetcultiv==0), aes(x=x,y=y), fill='grey90') +
  scale_fill_distiller(palette='Blues', direction=1, na.value="grey90") +
  theme_raster_map()

# Total cumulative drainage
sum(total_cumul_drained$`X2000`)/10^6

# Total cropland drainage
sum(lu_cumul_drained$cropland)/10^6








# df<- df %>% 
#   mutate(perc_overlap = ifelse(perc_overlap==0 || is.na(perc_overlap), 10e-12, perc_overlap))  
#   mutate(allowable = ifelse(allowable < 100,  allowable +10 ,allowable))
# df$allowable <- df$allowable * 1.2 }

# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=rdm_overlay)) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, rdm_overlay ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=perc_overlap)) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, perc_overlap ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=pred_drained)) +
#   scale_fill_distiller(palette='Purples', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, pred_drained ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()
# 
# ggplot(df) +
#   geom_tile(aes(x=x,y=y, fill=drain_distrib)) +
#   scale_fill_distiller(palette='Greens', direction=1, na.value="grey90") +
#   geom_tile(data= subset(df, drain_distrib ==0), aes(x=x,y=y), fill='grey60') +
#   theme_raster_map()

