# get libraries for making animated GIFs
source('./scripts/r/plots/fcn/animated_gif_libraries.r')
source('./scripts/r/plots/themes/gif_map_theme.r')


# get country and bbox polygons
source('./scripts/r/plots/get_country_bbox_shp_for_ggplot_map.r')

# INPUTS FOR 6000BC-2000 =======================================================

# read hyde ncdf
#h <- './data/lucc/hyde32_beta/zip/hyde32.nc'
h_2.5 <- './output/results/hyde_resampled/hyde32_2.5.nc'
h_0.5 <- './output/results/hyde_resampled/hyde32_0.5.nc'
hyde <- nc_open(h_2.5)
hyde_yrs <- sort(hyde$dim$time$vals)
hyde_indx <- match(hyde_yrs, hyde$dim$time$vals)
# hyde_yrs<- hyde_yrs[15:30]


# read stocker ncdf for natural wetland extent 
l <- './data/nat_wetland_map/trace21_129.cdf'
lpx <- nc_open(l)
lpx_yrs <- sort(lpx$dim$TIME$vals)  -5



hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]
lpx_indx <- match(hyde_yrs, lpx_yrs)




# get the index of hyde years in lpx data
idx_subset_yrs <- which((lpx_yrs %in% hyde_yrs) == TRUE)
subset_hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]



# INPUTS FOR 1700-2000 LAYERS ==================================================

# get ensemble remwet at 0.5 deg
wetloss_mean_stack0.5 <- readRDS('./output/results/wetloss/grid/wetloss_Mkm2_stack_0.5deg_mean_year.rds')


# get grid area
a_2.5 <- raster(l, varname="area") / 10^6
# get raster of gridcell area, accounting for projection
a_0.5 <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif") 


t_post1700 = 1


#ani.options("convert")
saveGIF({

  # loop through hyde years ======================================================
  for (t in 1:length(hyde_yrs)){
    
    # if match in the subset years of HYDE
    if(!is.na(idx_subset_yrs[t])){
      
      # get year label
      yr <- hyde_yrs[t]  
      
      # # print ticker
      print(paste("Year from hyde: ", yr, sep=""))
      print(paste("idx of LPX: ", idx_subset_yrs[t], sep=""))
      print(paste("Year from LPX: ", lpx$dim$TIME$vals[idx_subset_yrs[t]], sep=""))

      
      # GET 2.5deg LAYERS ============================
      

      # get max inund fraction (over 12 months)  
      wet <- max(brick(l, varname="inund", level=lpx_indx[t], lvar=4))
      # glaciers
      g <- raster(l, varname="lu_area", band =  lpx_indx[t], level=4)
      # get cropland
      c <- raster(h_2.5, varname="cropland", band = hyde_indx[t], level=4)

      origin(c) <- origin(wet)   # set origin
      # set same grid
      ext <- extent(-180, 180, -90, 90)
      a_2.5 <- extend(a_2.5, ext)
      g <- extend(g, ext)
      c <- extend(c, ext)
      wet <- extend(wet, ext)
      
    
      c_f = c / a_2.5
      # calculate wetloss remaining wetland extent
      wetloss_f <- (c_f  * wet) / wet

      
      # ggplot maps of LPXDYTOP 
      source('./scripts/r/plots/ggplot_wetlossmap.R') 

      
      
      # empty 
      if (yr >= 1700){

        # wetloss in 1700 as natwet
        natwet_0.5 <- sel.by.pattern(wetloss_mean_stack0.5, "1700")
        
        # wetloss at certain period
        wetloss_0.5_mean_sel <- sel.by.pattern(wetloss_mean_stack0.5, yr)
        
        
        c <- raster(h_0.5, varname="cropland", band = t_post1700, level=4)[[1]]
        
        
        origin(c) <- origin(a)   # set origin
        ext <- extent(-180, 180, -90, 90)

        a <- extend(a, ext)
        c   <- extend(c, ext)
        
        # set origin of hyde grids
        origin(c) <- origin(natwet_0.5)
        # set same grid
        ext <- extent(-180, 180, -90, 90)
        c   <- extend(c, ext)
        
        
        natwet_f = natwet_0.5 * 10^6 / a_0.5
        c_f <- c / a_0.5
        wetloss_f = wetloss_0.5_mean_sel * 10^6 / a_0.5
        
        
        
        # ggplot maps of LPXDYTOP 
        source('./scripts/r/plots/ggplot_wetlossmap_0.5.R')
        
      } else{
        # if before  1700, make blank map
        source('./scripts/r/plots/ggplot_wetlossmap_0.5_blank.R')
      }

      source('./scripts/r/plots/timeline.R')
      
      comb <- arrangeGrob(pre_1700_maps, post_1700_maps, timeline, nrow=3) 
      grid.arrange(comb)
      
    }
    
  }
},  movie.name = "./output/figures/gif/wetloss_wggplot_v9.gif", ani.width=1000, ani.height=600, interval = 0.8, clean=TRUE)
dev.off()

