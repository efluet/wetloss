# /----------------------------------------------------------------------------#
#/  Read hyde ncdf                                                         -----
#  RERUN THIS WHEN TESTING THE REDISTRIBUTION BC THE OVERLAY AREA GETS UPDATED AS DRAINAGE EXPANDS!!! 
h <- '../output/results/hyde_resampled/hyde32_allvars_05deg_to2020.nc'
hyde <- nc_open(h)




forestry <- stack(h, varname='forest_harv')
# 1Dec2020 - It appears that 
forestry <- forestry * raster::area(maxlncr)
forestry <- raster2df(forestry)
forestry <- left_join(maxlncr_df_xy, forestry, by=c('x','y'))[c(3:35)]  #[3:33]
#  For forestry, use the area of 2000, and use it for every timestep 
forestry <- forestry[,c(rep(33, 33))] #[,c(rep(31, 31))]

## START AT COLUMN == 3 BECAUSE X & Y COLUMNS ARE FIRST  
ir_rice    <- raster2df(stack(h, varname='ir_rice'))
ir_rice    <- left_join(maxlncr_df_xy, ir_rice, by=c('x','y'))[c(3:35)]  #[3:33]

urban      <- raster2df(stack(h, varname='uopp_'))
urban      <- left_join(maxlncr_df_xy, urban, by=c('x','y'))[c(3:35)]  #[3:33]

pasture    <- raster2df(stack(h, varname='pasture'))
pasture   <- left_join(maxlncr_df_xy, pasture, by=c('x','y'))[c(3:35)]  #[3:33]

cropland   <- raster2df(stack(h, varname='cropland'))
cropland   <- left_join(maxlncr_df_xy, cropland, by=c('x','y'))[c(3:35)]  #[3:33]

wetcultiv    <- cropland  # use cropland as LU proxy for wetcultiv too 


#-------------------------------------------------------------------------------

# ADDED MARCH 2021: REPEATED YEAR 2000, TO FILL IN 2010 AND 2020... Should use acutal LU for those years.
# peatland   <- raster2df(flip(stack(h, varname='peatland'),direction='y'))
# peatland   <- left_join(maxlncr_df_xy, peatland, by=c('x','y'))[c(3:35)]
# peatextr   <- peatland


## NOV2020 - replace with PEATMAP;  repeated at each step
peatmap <- raster('../data/natwet/xu2018_PEATMAP/PEATMAP_global_0.5deg_fraction.tif')
peatmap <- peatmap * raster::area(peatmap)
peatland <- raster2df(peatmap)
peatland   <- left_join(maxlncr_df_xy, peatland, by=c('x','y'))
peatland <- peatland[,c(rep(3, 35))]
# names(peatland) <- names(cropland)
peatextr   <- peatland



#-------------------------------------------------------------------------------
#  READ POPULUATION; used to to prioritize peat extraction 
# Read population count grid
pop <- raster('../data/lucc/hyde32_beta/popc_2000BC.asc')

# aggregate grid to resolution of wetland data
wet_res_x <- 0.5
wet_res_y <- 0.5
pop <- aggregate(pop, fact= c(wet_res_x/0.0833333, wet_res_y/0.0833333), fun=sum) 

pop[pop==0]<- 10^-10

pop_log <- log10(pop)

pop_log <- (pop_log - cellStats(pop_log, 'min'))
pop_log <- pop_log / cellStats(pop_log, 'max')

pop <- pop_log

pop <- raster2df(pop)
pop$x = round(pop$x, 2)
pop$y = round(pop$y, 2)
pop   <- left_join(maxlncr_df_xy, pop, by=c('x','y'))
names(pop) <- c('x','y','pop')

