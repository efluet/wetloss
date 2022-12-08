# /----------------------------------------------------------------------------#
#/     ORCHIDEE                                             --------------------
library("rlist")

# wetchimp common directory
d <- '../data/natwet/wetchimp/'
e1 <- 'exp1/amax_weta/'
e2 <- 'exp2/amax_weta/'
e3 <- 'exp3/amax_weta/'

# make list of  data
l <- c(paste0(d, e1, (list.files(path = paste0(d, e1), pattern = ".nc"))),
       paste0(d, e2, (list.files(path = paste0(d, e2), pattern = ".nc"))),
       paste0(d, e3, (list.files(path = paste0(d, e3), pattern = ".nc"))))

#-------------------------------------------------------------------------------
# Orchidee - Experiment #2 : 1993-2004
# orchidee2 <- raster(l[26], varname="amax_weta")
orchidee2 <- stack(l[28], varname="amax_weta")
orchidee2[[1]]
orchidee2 <- max(orchidee2)
orchidee2 <- disaggregate(orchidee2, fact=4, method="") / 16
NAvalue(orchidee2) <- 0
orchidee2[orchidee2<0] <- 0
orchidee2_km2 <- orchidee2 / 10^6
# orchidee2_km2[orchidee2_km2 > 769.315] <- 769.315
plot(orchidee2_km2)
sum_raster(orchidee2_km2 / 10^6)   # 17.13


#-------------------------------------------------------------------------------
#  SDGVM exp2
SDGVM2 <- stack(l[32], varname="amax_weta")
SDGVM2[[1]]
SDGVM2 <- max(SDGVM2)
SDGVM2 <- disaggregate(SDGVM2, fact=2, method="") / 4
NAvalue(SDGVM2) <- 0
SDGVM2[SDGVM2<0] <- 0
SDGVM2_km2 <- SDGVM2 / 10^6
# SDGVM2_km2[SDGVM2_km2 > 769.315] <- 769.315
plot(SDGVM2_km2)
sum_raster(SDGVM2_km2 / 10^6)   # 43.43


#-------------------------------------------------------------------------------
#  DLEM norice exp2
dlem2 <- stack(l[25], varname="amax_weta")
dlem2[[1]]
dlem2 <- max(dlem2)
dlem2 <- disaggregate(dlem2, fact=2, method="") / 4
NAvalue(dlem2) <- 0
dlem2[dlem2<0] <- 0
dlem2_km2 <- dlem2 / 10^6
# dlem2_km2[dlem2_km2 > 769.315] <- 769.315
plot(dlem2_km2)
sum_raster(dlem2_km2 / 10^6)   # 11.95



# /----------------------------------------------------------------------------#
#/       Read grid of cell area at 0.5;  This has the right global surface area ~510.066 Mkm2
area_05_km2 <- raster("../data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")



# READ FMAX ====================================================================

#  Fmax derived from hybrid approach using SWAMPS and GLWD for present-day.
#  Fmax = max(GLWD, max(SWAMPGLWD))

fmax <- '../data/natwet/zhang_2016/fmax.nc'
zhang_fmax <- stack(fmax, varname="parameter.fmax.without.openwater.fraction")
zhang_amax <- zhang_fmax * area_05_km2
zhang_amax <- disaggregate(zhang_amax, fact=2, method="") / 4
NAvalue(zhang_amax) <- 0
zhang_amax[zhang_amax<0] <- 0
plot(zhang_amax)
sum_raster(zhang_amax / 10^6)   # 10.87



#------------------------------------------------------------------------------#
# Zhang2016 LPJ-wsl  Wet pot

# wpot =  ratio of MAMax1980-2010 & long-term potential maximum wetland area 
# (Fwet max/. Higher value represents higher availability for sub-grids to be inundated)
wpot <- '../data/natwet/zhang_2016/wetland_potential.nc'
zhang_wpot <- stack(wpot, varname="Fw")
zhang_wpot <- zhang_wpot * area_05_km2
zhang_wpot <- disaggregate(zhang_wpot, fact=2, method="") / 4
NAvalue(zhang_wpot) <- 0
zhang_wpot[zhang_wpot<0] <- 0
plot(zhang_wpot)
sum_raster(zhang_wpot / 10^6)   # 64.55




#------------------------------------------------------------------------------
#  Save as stack

simwet_stack <- stack(orchidee2_km2, SDGVM2_km2, dlem2_km2, zhang_wpot)
names(simwet_stack) <- c('orchidee2_km2', 'SDGVM2_km2', 'dlem2_km2', 'zhang_wpot')
plot(simwet_stack)
writeRaster(simwet_stack, '../output/results/natwet/simwet/simwet_stack.tif')
