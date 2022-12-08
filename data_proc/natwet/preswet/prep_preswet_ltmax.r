# SWAMPSGLWD / WAD2M
# GLWD3 (3-12)
# GIEMS2-CORR

# GEt correction layers; originally from Zhen
source('./data_proc/natwet/preswet/get_corr_layers.R') 


# /----------------------------------------------------------------------------#
#/   WAD2M                                                         -------------
f <- "../data/natwet/wad2m/gcp-ch4_wetlands_2000-2018_025deg.nc"
wad2m_Fw <- brick(f, varname="Fw") # [[1:24]]

# Get global total
# sum_raster(wad2m_Aw_mamax) / 10^6   #  8.41 Mkm2

#/   99th percentile
# wad2m_q99 <- calc(wad2m_Fw, fun=function(x) quantile(x, 0.99, na.rm=T))  # 11.40  Mkm2
wad2m_q99 <- calc(wad2m_Fw, fun=max, na.rm=T)
wad2m_q99_a = wad2m_q99 * area(wad2m_q99)
sum_raster(wad2m_q99_a) / 10^6   



# /----------------------------------------------------------------------------#
#/      GLWD3                                                      -------------

# Coarsen GLWD3
if(0) { source('../data_proc/natwet/preswet/agg_glwd3_preswet.r') }

# Convert to area
glwd3_fw <- raster('../output/results/natwet/preswet/glwd3_wet_fw.tif')
crs(glwd3_fw) <- CRS("+init=epsg:4326")
glwd3_akmw <- glwd3_fw * area(glwd3_fw) 

sum_raster(glwd3_akmw) / 10^6   # 8.77 Mkm2

# Stack chose 
glwd3_akmw_corr <- stack(glwd3_akmw, CIFOR, NCSCD)
glwd3_akmw_corr  <- calc(glwd3_akmw_corr, fun=mean, na.rm=T) # mean
plot(glwd3_akmw_corr)
# glwd3_akmw_corr  <- calc(glwd3_akmw_corr, fun=max, na.rm=T) # 13.57


sum_raster(glwd3_akmw_corr) / 10^6   # 10.88



# /----------------------------------------------------------------------------#
#/     GIEMS2                                                      -------------

if(0){
  # Reproject GIEMS2 to WGS84
  source('./data_proc/natwet/preswet/prep_giems2_preswet.r')
  # Save reprojected GIEMS as NetCDF
  source('./data_proc/natwet/preswet/save_giems2_ncdf.r')
  # This reprojection was updated to fix area conservation issue 
  }


giems2_stack <- stack('../output/results/natwet/preswet/giems2_aw_v3.tif')# [[1:12]]

# # replace 0s with NA
# f1 <- function(x){  x[x == 0] <- NA
#   return(x) }

outstack <- stack()

# Loop through rasters; 
for (i in 1:nlayers(giems2_stack)){
  
  print(i)
  giems2_raster <- giems2_stack[[i]]
  giems2_raster <- calc(giems2_raster, fun=function(x) x[x == 0] <- NA,  progress='text')
  outstack <- stack(outstack, giems2_raster)
  }


writeRaster(outstack, '../output/results/natwet/preswet/giems2_aw_v3_na.tif')



## 
giems2_stack_na <- outstack
beginCluster(8)
# Get 99th percentiles
# giems2_na_q99 <- calc(giems2_stack_na, fun=function(x) quantile(x, 0.99, na.rm=T), progress='text')

giems2_na_q99 <- calc(giems2_stack_na, fun=max, na.rm=T, progress='text')
sum_raster(giems2_na_q99) / 10^6    # 11.4  Mkm2
# sum_raster(giems2_na_max) / 10^6  # 12.15  Mkm2
endCluster()



# Subtract the open water, rice and ocean water
giems2_q99_corr <- giems2_na_q99 - JRC - MIRCA - COAST

# Set negatives to 0
giems2_q99_corr[giems2_q99_corr < 0] <- 0

corr_max <- mean(stack(CIFOR, NCSCD, GLWD), na.rm=T)

# Stack correction grids
giems2_q99_corr <- stack(giems2_q99_corr, corr_max)

# Get long term max of corrected MAMax
giems2_q99_corr <- mean(giems2_q99_corr, na.rm=T)
# giems2_q99_corr <- max(giems2_q99_corr, na.rm=T)


giems2_q99_corr[is.na(JRC)] <- NA

sum_raster(giems2_q99_corr) / 10^6    #   99th percentile = 13.13





# /----------------------------------------------------------------------------#
#/    MAKE PRESWET STACK                                         ---------------

# Make stack
preswet_stack <- stack(wad2m_q99_a, glwd3_akmw_corr, giems2_q99_corr)
preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum)


# Rename rasters in stack
names(preswet_stack) <- c('wad2m_max_Aw', 'glwd3_akmw', 'giems2_max_corr')

cellStats(preswet_stack, sum)/10^6


# Save to file
writeRaster(preswet_stack, '../output/results/natwet/preswet/preswet_stack_max.tif', overwrite=T)
