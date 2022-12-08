# import packages
library(ncdf4)
library(raster)
library(rasterVis)
library(maptools)
library(maps)
library(animation)

setwd('C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss')
#setwd('C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/scripts/r')
#setwd('../../output/fig/gif/')

d <- './data/nat_wetland_map/trace21_129.cdf'
#d <- 'C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/data/nat_wetland_map/trace21_129.cdf'
nc <- nc_open(d)


yrs <-  c(unique(ncvar_get( nc, attributes(nc$dim)$names[4]))) # get year labels
yrs <- yrs[yrs > -10105]  # filter to only years since 10kBC
yrnb <- seq(1, length(yrs), 30) # subsample the timeseries


#ani.options("convert")
saveGIF({
  
  # # The "%02d" is a placeholder for a two character counter (01,02 etc.).
  # png(file="example%02d.png",   
  #     width=300, height=200, units="mm", res=300)
  
  # loop through the years =====================================================
  for (t in yrnb){
    
    for(m in seq(1, 12, 1)){
      
      print(paste0("now mapping:", t, " ", m))
      
      # read a specific band
      lu <- raster(d, varname="inund", band = t, level=m)
      
      # read a specific band
      glacier <- raster(d, varname="lu_area", 
                        band = t, level=4)
      
      # run the plot
      source('./plots/fcn/levelplot_forgif.R')
      
    }
  }
},  movie.name = "./output/figs/gif/inund_stocker_post10k_v6.gif", ani.width=600, ani.height=300, interval = 0.05, clean=TRUE)

