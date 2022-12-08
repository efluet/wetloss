# Description: 


### Read ORCHIDEE data ==================================================================== 
# wetchimp data originally expressed in m^2


# function that reads in wetchimp data
# no data is expressed as  -9999
read_wetchimp_raster <- function(input_dir){
                        if(length(nc_open(input_dir)$dim$time$vals)>1){
                          t=max(brick(input_dir), variable="amax_weta") /10^6
                        } else{ t     = raster(input_dir) /10^6 }
                        t@file@name <- basename(input_dir)
                        return(t)}


# wetchimp common directory
d <- './data/nat_wetland_map/wetchimp/'
e1 <- paste0(d, 'exp1/amax_weta/')
e2 <- paste0(d, 'exp2/amax_weta/')


# experiment 1: equilibrium run for 1901-1931 --> year 1900
# experiment 2: transition over 1992-2004  --> year 2000
wet_DLEM_exp1 <- read_wetchimp_raster(paste0(e1, 'amax_weta_1_DLEM.nc'))
wet_DLEM_exp2 <- read_wetchimp_raster(paste0(e2, 'amax_weta_2_DLEM.nc'))


names(wet_DLEM_exp1) <- "year1900"
names(wet_DLEM_exp2) <- "year2000"

# make empty brick
r <- brick(ncols=ncol(wet_DLEM_exp1), nrows=nrow(wet_DLEM_exp1), nl=31)
# rename layers in brick
year_seq <- seq(1700,2000,10)
for(i in seq(1, length(year_seq))){ year_seq[i] <- paste0('year',year_seq[i])}
names(r) <- year_seq

# subset the brick
r_interp <- r[[21:31]]
#  make  the rasters first and last of 
r_interp[[1]] <- wet_DLEM_exp1
r_interp[[11]] <- wet_DLEM_exp2

# interpolate between missing years
r_interp <- approxNA(r_interp)


interp_i = 1
for (i in seq(1:nlayers(r))){
  
  if (i < 21){
    r[[i]] <- wet_DLEM_exp1}
  
  if (i >= 21){
    r[[i]] <- r_interp[[interp_i]]
    interp_i <- interp_i+1}
}

names(r) <- year_seq

dlem_natwet_1700_stack <- r


# 
wet_Orchidee_exp1 <- read_wetchimp_raster(paste0(e1, 'amax_weta_1_Orchidee.nc'))
wet_Orchidee_exp2 <- read_wetchimp_raster(paste0(e2, 'amax_weta_2_Orchidee.nc'))


# amax_weta, amax_weta_all
wet_Orchidee_mid <- brick(paste0(d, 'ORCHIDEE_1932_1992/yearly_wetland_CH4_1932_1992_saisP07.nc'), varname="amax_weta")


# rename the yearly
year_seq <- seq(1932,1992,1)
for(i in seq(1, length(year_seq))){
  print(paste0('year',year_seq[i]))
  year_seq[i] <- paste0('year',year_seq[i])}
names(wet_Orchidee_mid) <- year_seq

wet_Orchidee_mid <- sel.by.pattern(wet_Orchidee_mid, "0")


# experiment 2: transient simulation for 1932-2009; has 12 monthly values
# starting in 1993 (probably averages for 1993-2004)
wet_SDGVM_exp1 <- read_wetchimp_raster(paste0(e1, 'amax_weta_1_SDGVM.nc'))
wet_SDGVM_exp2 <- read_wetchimp_raster(paste0(e2, 'amax_weta_2_SDGVM.nc'))


rm(d, e1, e2)


# READ POT WETLAND =============================================================
# potential map: ratio of mean annual maximum wetland extent averaged for 1980–2010
# and the long-term potential maximum wetland area (Fwet max). 
# see Zhang et al. 2016 - Figure 9
# pot <- '../../data/nat_wetland_map/zhang_2016/wetland_potential.nc'
# zhang_pot <- raster(pot, varname="Fw")


# READ FMAX ====================================================================
# Fmax derived from hybrid approach using SWAMPS and GLWD for present-day.
fmax <- './data/nat_wetland_map/zhang_2016/fmax.nc'
zhang_fmax <- raster(fmax, varname="parameter.fmax.without.openwater.fraction")


## Make a list of the rasters
rast_list <- list(wet_DLEM_exp1, wet_SDGVM_exp1, 
                  wet_DLEM_exp2, wet_SDGVM_exp2, 
                  zhang_fmax)
