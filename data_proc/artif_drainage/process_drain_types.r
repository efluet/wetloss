# /----------------------------------------------------------------------------#
#/     Get fcn extracting drain types                                   --------- 
# the different drain types are run separately because they rely on different raster sources
# function:  process_drainage_stats
source('./scripts/data_proc/artif_drainage/fcn/fcn_calc_percent_artif_drainage_pertype_v2.r')

# to do list:
#   - test LUH  v2 forestry
#   - add in the subnational data
#   - grid the drainage onto natwetgrid
#   - separate spate irrig
#   - separate irrig vs rainfed 


# Get drainage fraction data
drained <- read.csv("./output/results/artif_drainage/drained_wetcult_ha.csv", stringsAsFactors = F)

# /----------------------------------------------------------------------------#
#/    Cropland                                                       -----------

hyde <- stack('./output/results/hyde_resampled/hyde32_0.5.nc', varname='cropland')

# keep only raster every 10yrs between 1700-2000 
hyde <- hyde[[1:31]]
names(hyde) <- seq(1700,2000,10)

# run the function 
df_c <- process_drainage_stats(drained, "cropland", hyde)


# /----------------------------------------------------------------------------#
#/     Forestry                                                         --------

# make grid of wood harvest
source('./scripts/data_proc/prep_data/wood_harvest/save_wood_luhv2.r')
forest_harv_stack <- stack("./output/results/forest_harv.nc")



crs(forest_harv_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
forest_harv_stack <- forest_harv_stack * area(forest_harv_stack[[1]])
# make sure this comes last, and names don't get overwritten
names(forest_harv_stack) <- seq(1700,2000,10) 

# run function 
df_f <- process_drainage_stats(drained, "forestry", forest_harv_stack)



#==============================================================================#
###    Peatland extraction                --------------------------------------
#==============================================================================#

# debug peatland
l <- './data/lpx_dytop_2014/s1_dyptop_output.nc'
lpx <- nc_open(l)
lpx_peat <-brick(l, varname="lu_area", level=2)   # peat doesn't change between month of the same year.
last_idx <- dim(lpx_peat)[3]

# get raster of gridcell area, accounting for projection
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")

# aggregate grid to resolution of wetland data
area <- aggregate(area, fact= c(2), fun=sum)


peat_stack <- lpx_peat[[last_idx]]  *  area

peat_stack <- stack( mget( rep( "peat_stack" , 31)))


names(peat_stack) <- seq(1700,2000,10)


# clean up environment
rm(area, l, lpx, lpx_peat, last_idx)



### THIS RUNS ON TOO MANY POLYGONS, DONT NEED YEARLY DATA, SUBSAMPLE TO DECADAL
df_p <- process_drainage_stats(drained, "peatland", peat_stack)



#==============================================================================#
# combine the extracted df                 -------------------------------------  
#==============================================================================#
drained_wfrac <- bind_rows(df_c, df_f, df_p)


# write output to file
write.csv(drained_wfrac, "./output/results/artif_drainage/drained_wfrac.csv")


rm(df_c, df_f, df_p)