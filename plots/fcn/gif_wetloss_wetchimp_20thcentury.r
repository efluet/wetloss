# Save the raster stacks as R objects
wetloss_Mk2_stack <- readRDS('../../output/results/wetloss_Mk2_stack_wetchimp.rds')
remwet_Mkm2_stack <- readRDS('../../output/results/remwet_Mkm2_stack_wetchimp.rds')




# read raster of gridcell area, accounting for projection
area <- raster("../../data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")


wetloss_perc_stack <- wetloss_Mk2_stack / area * 100
# transfer names
names(wetloss_perc_stack) <- names(wetloss_Mk2_stack)


# subset the stack per model name
subset_stack <- function(greppattern, stack){
  i <- grep(greppattern, names(stack))
  t <- stack[[i]] }


wetloss_perc_stack_DLEM <- subset_stack("DLEM", wetloss_perc_stack)
wetloss_perc_stack_SDGVM <- subset_stack("SDGVM", wetloss_perc_stack)
wetloss_perc_stack_pot <- subset_stack("wetland_potential", wetloss_perc_stack)
wetloss_perc_stack_fmax <- subset_stack("fmax", wetloss_perc_stack)



#ani.options("convert")
saveGIF({
  
  for(i in seq(1, length(names(DLEM_wetloss_Mk2_stack)))){
  
    DLEM_temp <- wetloss_perc_stack_DLEM[[i]]
    DLEM_temp <- wetloss_perc_stack_SDGVM[[i]]
    DLEM_temp <- wetloss_perc_stack_pot[[i]]
    DLEM_temp <- wetloss_perc_stack_fmax[[i]]
    
    # print counter
    print(names(wet))
    
    # plot wetloss w/ levelplot
    source('./plots/fcn/levelplot_forgif_wetloss_only.r') 
    
  
  }
},  movie.name = "../../output/figures/gif/wetloss_since1700.gif", 
    ani.width=600, ani.height=300, interval = 0.5, clean=TRUE)

dev.off()