# make raster sum function (shorten)
sum_raster <- function(raster){sum(cellStats(raster, stat="sum"))}

# create empty df for output
output_df <- data.frame(name=character(),
                        wet_Mkm2 =numeric(),
                        year_start=numeric(),
                        year_end=numeric())


# read gridcell area
area <- raster("../../data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")


# read Zhang's 1900-2000 potential wetland 1901–2013 ===========================

# READ POT WETLAND =============================================================
# potential map: ratio of mean annual maximum wetland extent averaged for 1980–2010
# and the long-term potential maximum wetland area (Fwet max). # see Zhang et al. 2016 - Figure 9
pot <- '../../data/nat_wetland_map/zhang_2016/wetland_potential.nc'
# read zhang's layers as rasters
zhang_pot <- raster(pot, varname="Fw")

output_df <- rbind(output_df, 
                   # get the filename at end of data directory
                   data.frame(name    = "Zhang - potential",
                              wet_Mkm2= sum_raster(zhang_pot * area  / 10^6),
                              year_start = 1980,
                              year_end = 2010))



# READ FMAX ====================================================================
# Fmax derived from hybrid approach using SWAMPS and GLWD for present-day.
fmax <- '../../data/nat_wetland_map/zhang_2016/fmax.nc'

# read zhang's layers as rasters
zhang_fmax <- raster(fmax, varname="parameter.fmax.without.openwater.fraction")

output_df <- rbind(output_df, 
             # get the filename at end of data directory
             data.frame(name    = "Zhang - fmax",
                        wet_Mkm2= sum_raster(zhang_fmax * area  / 10^6),
                        year_start = 1993,
                        year_end = 2004))




# wetland area from WETCHIMP ---------------------------------------------------
# DLME, 0.5deg
# ORCHIDEE, 1.0deg
# SDGVM, 0.5deg

# wetchimp common directory
d <- '../../data/nat_wetland_map/wetchimp/'
e1 <- 'exp1/amax_weta/'
e2 <- 'exp2/amax_weta/'

# experiment 1: equilibrium run for 1901-1931
wet_DLEM_exp1     = paste0(d, e1, 'amax_weta_1_DLEM.nc')
wet_SDGVM_exp1    = paste0(d, e1, 'amax_weta_1_SDGVM.nc')
wet_orchidee_exp1 = paste0(d, e1, 'amax_weta_1_Orchidee.nc')


# experiment 2: transient simulation for 1932-2009
# data: 12 monthly values, starting in 1993 (probably averages for 1993-2004)
wet_DLEM_exp2     = paste0(d, e2, 'amax_weta_2_DLEM.nc')
wet_SDGVM_exp2    = paste0(d, e2, 'amax_weta_2_SDGVM.nc')
wet_orchidee_exp2 = paste0(d, e2, 'amax_weta_2_Orchidee.nc')



# make list of data sources to sum =============================================
wetchimp_list <- c(wet_DLEM_exp1, wet_SDGVM_exp1, wet_orchidee_exp1, 
                   wet_DLEM_exp2, wet_SDGVM_exp2, wet_orchidee_exp2)



# LOOP AND CALCULATE SUMs ======================================================
# loop through the list of 
for (i in wetchimp_list){

  # read max raster
  wet <- raster(i, varname="amax_weta")
  # sum global in million km2
  wet_Mkm2 <- sum_raster(wet / 10^6 / 10^6)
  # read as ncdf
  temp_nc <- nc_open(i)
  year_start <- 1901
  year_end <- 1932
  
  # if multiple month, get the maximum value across all months
  if (length(temp_nc$dim$time$vals) > 1){
        wet <- max(brick(i, varname="amax_weta"))
        wet_Mkm2 <- sum_raster(wet / 10^6 / 10^6)
        year_start <- 1993
        year_end <- 2004}
  
  output_df <- rbind(output_df, 
                # get the filename at end of data directory
                data.frame(name    = tail(strsplit(i, "/")[[1]],n=1),
                           wet_Mkm2= wet_Mkm2,
                           year_start = year_start,
                           year_end = year_end))
  
}


# write out 
write.csv(output_df, "../../output/results/global_sum_nat_wetland_20th.csv")
