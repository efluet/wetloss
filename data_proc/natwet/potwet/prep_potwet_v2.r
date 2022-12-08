

# /---------------------------------------------------
#/    Get simwet stack
simwet_stack <- stack('../output/results/natwet/simwet/simwet_stack.tif')
names(simwet_stack) <- c('orchidee2_km2', 'SDGVM2_km2', 'dlem2_km2', 'zhang_wpot')
simwet_stack <- raster::aggregate(simwet_stack, fact=2, fun=sum, na.rm=TRUE)


# /---------------------------------------------------
#/    Get preswet mamax stack
preswet_stack <- stack('../output/results/natwet/preswet/preswet_stack.tif')
names(preswet_stack) <- c('wad2m_Aw_mamax', 'glwd3_akmw', 'giems2_mamax_corr')
preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum, na.rm=TRUE)


# /---------------------------------------------------
#/   Present-day wetland - maximum area
preswet_max_stack <- stack( '../output/results/natwet/preswet/preswet_stack_max.tif')
names(preswet_max_stack) <- c('wad2m', 'glwd3', 'giems2')
# preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum, na.rm=TRUE)



# SIMWET: 
#   - ORCHIDEE EXP#2
#   - SDGVM  (35-45Mkm2)
#   - DLEM (wet)
#   - Zhen wPot (LPJ-wsl)

# PRESWET
#   - WAD2M
#   - GLWD
#   - GIEMSv2



### PLOT GRID OF POTWET  (PRESWET X SIMWET)
if(0){ source('plots/nat_wet/map_potwet_grid.r') }
