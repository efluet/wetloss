# get libraries for making animated GIFs
source('./plots/fcn/animated_gif_libraries.r')



# read Stocker and HYDE ncdf data ==============================================

# read hyde ncdf
h <- '../../data/lucc/hyde32_beta/zip/hyde32.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals) # get years


# read stocker ncdf
l <- '../../data/nat_wetland_map/trace21_129.cdf'
lpx <- nc_open(l)
lpx_yrs <- sort(lpx$dim$TIME$vals) # get years
lpx_yrs <- (lpx_yrs -5) # round(lpx_yrs, -1)# round to remove 5 as last digit


# get the index of hyde years in lpx data
idx_subset_yrs <- which((lpx_yrs %in% hyde_yrs) == TRUE)
subset_hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]

wetloss_stack <- stack()
remwet_stack <- stack()

#ani.options("convert")
saveGIF({

  
  # why are 1850 and 1950 disapearing? plot global timeline?
  
  # loop through hyde years ======================================================
  for (t in 1:length(hyde_yrs)){

    
    # if no match in
    if(!is.na(idx_subset_yrs[t])){
      yr <- hyde_yrs[t]  
      print(paste("Year from hyde: ", yr, sep=""))
      print(paste("idx of LPX: ", idx_subset_yrs[t], sep=""))
      print(paste("Year from LPX: ", lpx$dim$TIME$vals[idx_subset_yrs[t]], sep=""))

      a <- raster(l, varname="area")  # get grid area
      # get max inund fraction (over 12 months)  
      wet <- max(brick(l, varname="inund", 
                       level = idx_subset_yrs[t], lvar=4)) # tells it level is 4th dim
    
      
      
      g <- raster(l, varname="lu_area", 
                  band =  idx_subset_yrs[t], level=4)   # read a specific band
      
    
      c <- raster(h, varname="cropland", band = t, level=4)  # get cropland
      
      
      origin(c) <- origin(wet)   # set origin
      # set same grid
      ext <- extent(-180, 180, -90, 90)
      a <- extend(a, ext)
      g <- extend(a, ext)
      c <- extend(c, ext)
      wet <- extend(wet, ext)
      
    
      # calculate remaining wetland extent
      wetloss <- ((c/a)  * wet) / wet
      remwet <- (wet - wetloss) / wet
      
      wetloss_stack <- stack( wetloss_stack , wetloss )
      remwet_stack <-  stack( remwet_stack , remwet )
      
      # plot wetloss w/ levelplot
      # source('./plots/fcn/levelplot_forgif_wetloss.r')
      
      # plot 
      source('./scripts/r/plots/ggplot_wetlossmap.R') 

      
    }
    
  }
},  movie.name = "../../output/figures/gif/wetloss_wggplot.gif", ani.width=600, ani.height=1000, interval = 0.5, clean=TRUE)
dev.off()

# ideas for plot ===============================================================
# peak year of wetland loss
# remaining % since 10kBC, since 0AD, since pre-industrial

