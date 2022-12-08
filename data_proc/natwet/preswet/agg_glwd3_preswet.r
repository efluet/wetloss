

## AGGREGATE GLWD WETLAND FRACTION
# read in GLWD3
if(0){
  g <- "../../Chap2_wetland_classification_GIEMS-D15C/data/glwd/glwd_3/glwd_3.adf"
  glwd3 <- raster(g, RAT=FALSE)
  
  rcl <- rbind(c(1,NA),
        c(2,NA),
        c(3,NA),
        c(4, 1),
        c(5, 1),
        c(6, 1),
        c(7, 1),
        c(8, 1),
        c(9, 1),
        c(10,.75),
        c(11,.375),
        c(12,.125))
  rcl <- data.frame(rcl)
  
  
  # Substitute values in raster
  glwd3 <- subs(glwd3, rcl, by='X1', which='X2')
  
  # aggregate to 0.25 grid (0.25/0.0083333 = 30)
  glwd3_agg <- aggregate(glwd3, fact=30, na.rm=TRUE, fun="sum")
  # extent(glwd3_agg) <- extent(-180,180,-90,90)
  
  glwd3_agg_frac <- glwd3_agg/900
  
  writeRaster(glwd3_agg_frac, '../output/results/preswet/glwd3_wet_fw.tif', overwrite=T)
  }


# /----------------------------------------------------------------------
#/   AGGREGATE GLWD OPEN WATER INTO: FRACTION OF 0.25DEG  
# read in GLWD3
if(0){
  g <- "../../Chap2_wetland_classification_GIEMS-D15C/data/glwd/glwd_3/glwd_3.adf"
  glwd3 <- raster(g, RAT=FALSE)
  
  rcl <- rbind(c(1,1),
               c(2,1),
               c(3,1),
               c(4,NA),
               c(5,NA),
               c(6,NA),
               c(7,NA),
               c(8,NA),
               c(9,NA),
               c(10,NA),
               c(11,NA),
               c(12,NA))
  rcl <- data.frame(rcl)
  
  glwd3 <- subs(glwd3, rcl, by='X1', which='X2')
  
  
  # aggregate to 0.25 grid (0.25/0.0083333 = 30)
  glwd3_agg <- aggregate(glwd3, fact=30, na.rm=TRUE, fun="sum")
  # extent(glwd3_agg) <- extent(-180,180,-90,90)
  
  glwd3_agg_frac <- glwd3_agg/900
  
  writeRaster(glwd3_agg_frac, '../output/results/preswet/glwd3_ow_fw.tif', overwrite=T)
  }


