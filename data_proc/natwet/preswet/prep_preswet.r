# SWAMPSGLWD / WAD2M
# GLWD3 (3-12)
# GIEMS2-CORR

# /----------------------------------------------------------------------------#
#/   WAD2M                                                         -------------
f <- "../data/natwet/wad2m/gcp-ch4_wetlands_2000-2018_025deg.nc"
wad2m_Fw <- brick(f, varname="Fw") # [[1:24]]


#  Summarize WAD2M                                          -------------
# Make group label list; labels rasters from same timestep together
groupn = function(n,m){rep(1:m,rep(n/m,m))}

nlay <- length(names(wad2m_Fw))

# Make list of group labels
timestep_grp = groupn(nlay, nlay/12 )

f = function(x){tapply(x, timestep_grp, max, na.rm=T)}

# Calculate annual maximum
wad2m_annualmax <- calc(wad2m_Fw, fun=f)

# Get average of annual maximums  (ie MAMax)
wad2m_mamax <- calc(wad2m_annualmax, fun=mean, na.rm=T)
plot(wad2m_mamax)

# Conver fraction to area (km2)
wad2m_Aw_mamax = wad2m_mamax * area(wad2m_mamax) 

# Save to file
writeRaster(wad2m_Aw_mamax, '../output/results/natwet/preswet/wad2m_Aw_mamax.tif', overwrite=T)

# Get global total
sum_raster(wad2m_Aw_mamax) / 10^6   #  8.41 Mkm2



# Calculate annual maximum
# wad2m_max <- calc(wad2m_Fw, fun=max, na.rm=T)
# wad2m_max_a = wad2m_max * area(wad2m_max) 
# sum_raster(wad2m_max_a) / 10^6   #  13.75 Mkm2


# /----------------------------------------------------------------------------#
#/      GLWD3                                                      -------------

# Coarsen GLWD3
source('../data_proc/natwet/preswet/agg_glwd3_preswet.r')

# Convert to area
glwd3_fw <- raster('../output/results/natwet/preswet/glwd3_wet_fw.tif')
crs(glwd3_fw) <- CRS("+init=epsg:4326")
glwd3_akmw <- glwd3_fw * area(glwd3_fw) 

sum_raster(glwd3_akmw) / 10^6   # 8.77 Mkm2


# /----------------------------------------------------------------------------#
#/     GIEMS2                                                      -------------

# Reproject GIEMS2 to WGS84
source('./data_proc/natwet/preswet/prep_giems2_preswet.r')
# Save reprojected GIEMS as NetCDF
source('./data_proc/natwet/preswet/save_giems2_ncdf.r')
# This reprojection was updated to fix area conservation issue 


# Get monthly stack of GIEMS2
giems2_stack <- stack('../output/results/natwet/preswet/giems2_aw_v3.tif')[[121:244]]


# Make group label list; labels rasters from same timestep together
groupn <- function(n,m) { rep(1:m,rep(n/m,m)) }
nlay = nlayers(giems2_stack)
timestep_grp = groupn(nlay, nlay/12 )
f = function(x) { tapply(x, timestep_grp, max, na.rm=T) }

# Calculate maximum
giems2_annmax <- calc(giems2_stack, fun=f) # annual maximum
giems2_mamax  <- calc(giems2_annmax, fun=mean, na.rm=T) # mean of annual maximum


writeRaster(giems2_mamax, '../output/results/natwet/preswet/giems2_aw_v3_mamax.tif')

giems2_ltmax  <- calc(giems2_stack, fun=max, na.rm=T)



# FIXED ON JAN2022
giems2_annmax <- stackApply(giems2_stack, timestep_grp, fun = max)
giems2_mamax  <- calc(giems2_annmax, fun=mean, na.rm=T) # mean of annual maximum

sum_raster(giems2_ltmax) / 10^6
sum_raster(giems2_mamax) / 10^6  


# /----------------------------------------------------------------------------#
#/     Read correction layers                                     --------------

# JRC static
JRC <- stack('../data/natwet/wad2m_corr_layers/Global_JRC_025deg_WGS84_fraction.nc')
JRC <- JRC * area(JRC)

# Mirca Rice 12 months
MIRCA <- stack('../data/natwet/wad2m_corr_layers/MIRCA_monthly_irrigated_rice_area_025deg_frac.nc')
MIRCA <- calc(MIRCA, fun=max, na.rm=T)
MIRCA <- MIRCA * area(MIRCA)

# Coastal mask (Static)
COAST <- stack('../data/natwet/wad2m_corr_layers/MODIS_coastal_mask_025deg.nc')
COAST <- COAST * area(COAST)


# CIFOR wetland map (Static)
CIFOR <- stack('../data/natwet/wad2m_corr_layers/cifor_wetlands_area_025deg_frac.nc')
CIFOR <- CIFOR * area(CIFOR)

# NCSCD peatland map (static)
NCSCD <- stack('../data/natwet/wad2m_corr_layers/NCSCD_fraction_025deg.nc')
NCSCD <- NCSCD * area(NCSCD)



# Subtract the open water, rice and ocean water
giems2_mamax_corr <- giems2_mamax - JRC - MIRCA - COAST

# Set negatives to 0
giems2_mamax_corr[giems2_mamax_corr < 0] <- 0


giems2_mamax_corr <- stack(giems2_mamax_corr, CIFOR, NCSCD)

giems2_mamax_corr <- max(giems2_mamax_corr, na.rm=T)

giems2_mamax_corr[is.na(JRC)] <- NA

plot(giems2_mamax_corr)

sum_raster(giems2_mamax_corr) / 10^6    # 7.73 Mkm2



# /----------------------------------------------------------------------------#
#/    MAKE PRESWET STACK                                         ---------------

# Make stack
preswet_stack <- stack(wad2m_Aw_mamax, glwd3_akmw, giems2_mamax_corr)
preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum)


# Rename rasters in stack
names(preswet_stack) <- c('wad2m_Aw_mamax', 'glwd3_akmw', 'giems2_mamax_corr')

# Save to file
writeRaster(preswet_stack, '../output/results/natwet/preswet/preswet_stack.tif', overwrite=T)
