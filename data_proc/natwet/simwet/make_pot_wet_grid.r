library("rlist")

# wetchimp common directory
d <- './data/nat_wetland_map/wetchimp/'
e1 <- 'exp1/amax_weta/'
e2 <- 'exp2/amax_weta/'
e3 <- 'exp3/amax_weta/'

# make list of  data
l <- c(paste0(d, e1, (list.files(path = paste0(d, e1), pattern = ".nc"))),
       paste0(d, e2, (list.files(path = paste0(d, e2), pattern = ".nc"))),
       paste0(d, e3, (list.files(path = paste0(d, e3), pattern = ".nc"))))


l <- list.remove(l, c(8,27))


# /----------------------------------------------------------------------------#
#/    Make potential wetland grid                                  -------------
# Notes:
# most exp1,2 have lower area, making them unusable as "early" larger wetland area.
# use the difference between optimal and transient for 1993-2004
# amax_weta_2_Orchidee.nc  - amax_weta_3_Orchidee.nc


# Orchidee - Experiment #3 : 1993-2004
# Needs rescaling to 0.5 grid
#pres_wet <- aggregate(raster(l[44], varname="amax_weta"), fact=2, fun=sum)
orchidee3 <- raster(l[44], varname="amax_weta")

# Orchidee - Experiment #2 : 1993-2004
orchidee2 <- raster(l[26], varname="amax_weta")
orchidee2 <- disaggregate(orchidee2, fact=2, method="") / 4

# Calcualte the difference in km^2
diff_wet <- (pot_wet - pres_wet) / 10^6

# set negativs to 0
diff_wet[diff_wet < 0] <- 0



# save raster to GeoTiff
writeRaster(pres_wet, "./output/results/potwet/pres_wet.tif",
            format="GTiff", overwrite=TRUE, options=c("COMPRESS=NONE", "TFW=YES"))

# save raster to GeoTiff
writeRaster(diff_wet, "./output/results/potwet/potwet.tif",
            format="GTiff", overwrite=TRUE, options=c("COMPRESS=NONE", "TFW=YES"))


# /----------------------------------------------------------------------------#
#/   Plot diff wetland grid
diff_wet_df <- as.data.frame(diff_wet, xy = TRUE) %>%
                mutate(wet_diff_km2 = layer) %>% 
                filter(!is.na(wet_diff_km2)) %>%
                mutate(wet_diff_km2 = ifelse(wet_diff_km2 < 0, 0, wet_diff_km2)) %>%
                filter(wet_diff_km2 != 0)



# /----------------------------------------------------------------------------#
#/     Map difference in wetland area

ggplot(diff_wet_df) +
  
  geom_raster(aes(x = x, y = y, fill =  wet_diff_km2)) + 
  
  theme_raster_map() +
  coord_equal() +
  scale_fill_gradient2(midpoint = 0, 
                       low="blue", 
                       mid="gray95",
                       high="red", 
                       space ="Lab",
                       guide="legend",
                       breaks=seq(0, 4000, 500))




rm(diff_wet, pot_wet, pres_wet)



# Orchidee_alt* = ORCHIDEE model results for all water table depths using the internal TOPMODEL based approach for
# inter- and intra-annual wetland extents. 
# Orchidee_altsat = ORCHIDEE model results for only saturated areas using the internal TOPMODEL based approach for
# inter- and intra-annual wetland extents.
# Orchidee = ORCHIDEE model results for all water table depths using the prescribed Papa et al. 2010
# inter- and intra-annual wetland extents.
# Orchidee_sat = ORCHIDEE model results for only saturated areas using the prescribed Papa et al. 2010
# inter- and intra-annual wetland extents.