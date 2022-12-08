
# SIMWET: 
#   - ORCHIDEE EXP#2
#   - SDGVM  (35-45Mkm2)
#   - DLEM (wet)
#   - Zhen wPot

# PRESWET
#   - WAD2M
#   - GLWD
#   - GIEMSv2

# POTWET = SIMWET - PRESWET


# Get simwet stack
simwet_stack <- stack('../output/results/natwet/simwet/simwet_stack.tif')
names(simwet_stack) <- c('orchidee2_km2', 'SDGVM2_km2', 'dlem2_km2', 'zhang_wpot')
simwet_stack <- aggregate(simwet_stack, factor=2, fun=sum)
simwet_stack

# Get preswet stack
preswet_stack <- stack('../output/results/natwet/preswet/preswet_stack.tif')
names(preswet_stack) <- c('wad2m_Aw_mamax', 'glwd3_akmw', 'giems2_mamax_corr')
preswet_stack <- aggregate(preswet_stack, factor=2, fun=sum)
preswet_stack


# /----------------------------------------------------------------------------#
#/    CALC DIFF                           ----------------------

# Initialize output
potwet_stack <- stack()

# Loop through rasters, calculating the difference
for (s in c(1:nlayers(simwet_stack))){
  
  print(paste('s', s))
  simwet_temp <- simwet_stack[[s]]
  simwet_temp[is.na(simwet_temp)] <- 0
  
  for (p in c(1:nlayers(preswet_stack))){
    
    print(paste('  |- p', p))
    preswet_temp <- preswet_stack[[p]]  
    preswet_temp[is.na(preswet_temp)] <- 0
    
    # potwet <- simwet_temp - preswet_temp
    potwet<- overlay(simwet_temp,
                      preswet_temp,
                      fun=function(r1, r2){return(r1-r2)})
    
    names(potwet) <- paste0(names(simwet_temp),'_',names(preswet_temp))
    
    potwet[potwet<0] <- 0
    
    potwet_stack <- stack(potwet_stack, potwet)
    
  }
}

# Save potwet to file
writeRaster(potwet_stack, '../output/results/natwet/potwet_n12_stack.tif')

#sum the layers
datasum <- cellStats(potwet_stack, stat='sum', na.rm=TRUE)/10^6
