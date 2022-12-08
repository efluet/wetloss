
# get hyde years  ===============================================================
h <- './output/results/hyde32_0.5.nc'
hyde_yrs <- sort(nc_open(h)$dim$time$vals)



# Save the raster stacks as R objects
wetloss_Mk2_stack <- readRDS('./output/results/wetloss_Mk2_stack_wetchimp.rds')
remwet_Mkm2_stack <- readRDS('./output/results/remwet_Mkm2_stack_wetchimp.rds')

names(wetloss_Mk2_stack)
wetloss_Mk2_stack_dlem <- raster::subset(wetloss_Mk2_stack, grep('weta_1_DLEM', names(wetloss_Mk2_stack), value = T))
wetloss_Mk2_stack_fmax <- raster::subset(wetloss_Mk2_stack, grep('fmax', names(wetloss_Mk2_stack), value = T))
wetloss_Mk2_stack_SDGVM <- raster::subset(wetloss_Mk2_stack, grep('weta_1_SDGVM', names(wetloss_Mk2_stack), value = T))



source("./plots/fcn/global_map_ggplot.r")


saveGIF({

  # why are 1850 and 1950 disapearing? plot global timeline?
  
  # loop through hyde years ======================================================
  for (i in 1:length(hyde_yrs)){
    
    
    d <- wetloss_Mk2_stack_dlem[[i]]
    d <- global_map_ggplot(d, 'OrRd')
    
    s <- wetloss_Mk2_stack_SDGVM[[i]]
    s <- global_map_ggplot(s, 'YlGn')
    
    f <- wetloss_Mk2_stack_fmax[[i]]
    f <- global_map_ggplot(f, 'YlGn')
    
    
    grid.arrange(d, s, f, ncol=1)
    
  }
},  movie.name = "./output/figures/gif/natwet_remwet_wggplot.gif", ani.width=400, ani.height=600, interval = 0.5, clean=TRUE)
dev.off()


# ideas for plot ===============================================================
# peak year of wetland loss
# remaining % since 10kBC, since 0AD, since pre-industrial

