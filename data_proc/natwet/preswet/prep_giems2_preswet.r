# /----------------------------------------------------------------------------#
#/ Read GIEMSv2 from original .DAT file
#  The data are gridded on an equal area grid of 0.25°x0.25° at the equator.
#  Each grid cell is 773km2 in surface. Monthly for 24 years (1992-2015). 

# The combination of EASE-projection and coordinates in lat/lon  has made us perplexed as to how to reproject GIEMSv2 without introducing bias along the latitudinal gradient.  It appears to us that the right way to convert projections would be to generate points, convert them to equal-area polygons, then intersect those with a WGS84 grid.  However, this procedure seems too involved to be the simplest answer...

# GIEMS2 is on the same grid as the original GIEMS1 you already used, for a friendly transition from GIEMS1 to GIEMS2. 
# It is on the same equal area grid of 0.25°x0.25° at the equator. 
# The same routine you used for GIEMS1 should be directly applicable to GIEMS2. 
# As a reminder, all the boxes have the same height in latitude (0.25°), and a varying width in longitude. 
# IT IS NOT ON THE NSIDC EASE-GRID as your message could suggest. 
# You can check that the latitude and longitude do not match the NSIDC EASE-grid.

library(fasterize)
library(rasterDT)
library(parallel)



# /----------------------------------------------------------------------------#
#/ GET INUND GIEMS2 DATA                                   ---------
#  Read in .dat file
f <- '../data/natwet/giems2/wetland_1992_2015_Global_monthly_v2.dat'
d <- read.delim(f, header = FALSE, stringsAsFactors = FALSE, sep = "", check.names=FALSE)

# Make template raster in WGS84;  0.25*0.25deg
template_raster = raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180*4, ncols=360*4, crs="+init=epsg:4326")

# Make time steps
month_ls <- rep(seq(1, 12), 12)
year_ls  <- rep(1992:2015, each=12)
tstep_ls <- paste0(year_ls, '_', month_ls)
# Column names
names(d) <- c('aw','lon','lat', tstep_ls)




# /----------------------------------------------------------------------------#
#/  Calculate the global sum per month
giems2_raw_sum <- d %>% 
                  pivot_longer(`1992_1`:`2015_12`, names_to='date', values_to='Aw') %>% 
                  group_by(date) %>% 
                  dplyr::summarise(Aw = sum(Aw, na.rm=T))
        

giems2_raw_sum <- giems2_raw_sum %>%
                  mutate(year  = substr(date, 1, 4),
                         month = substr(date, 6, 6)) %>% 
                  mutate(date2 = as.Date(with(., paste(year, month, '1', sep="-")), "%Y-%m-%d")) %>% 
                  ungroup()


ggplot(data=giems2_raw_sum)+
  geom_line(aes(x=date2, y=Aw/10^6, group='1'), color='black' ) +
  geom_line(aes(x=date2, y=reproj_area, group='1'), color='red' ) +
  line_plot_theme



# /----------------------------------------------------------------------------#
#/  Read GIEMS2 coordinates from .dat file
f <- '../data/natwet/giems2/cell_lat_lon_width.dat'
df <- read.delim(f, header = FALSE, stringsAsFactors = FALSE, sep = "", check.names=FALSE)
names(df) <- c('id','y','x','w')

yres=0.25
df$ymax <- df$y + yres/2
df$xmax <- df$x + df$w/2
df$ymin <- df$y - yres/2
df$xmin <- df$x - df$w/2

df$xmax[df$xmax >  180] <- 180
df$xmin[df$xmin < -180] <- -180

square=cbind(df$xmin, df$ymax,  # NW corner
             df$xmax, df$ymax,  # NE corner
             df$xmax, df$ymin,  # SE corner
             df$xmin, df$ymin,  # SW corner
             df$xmin, df$ymax)  # NW corner again - close ploygon

# Extract the plot ID information
ID = df$id


# /----------------------------------------------------------------------------#
#/  create spatial polygons from coordinates
#  Code from NEON: https://www.neonscience.org/field-data-polygons-centroids
polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))



# /----------------------------------------------------------------------------#
#/  CALCUALTE THE NB OF PIXELS IN EACH BOX, THEN DIVIDE AREA BY NB OF PIXELS

library(sf)
polys_st <- st_as_sf(polys)

wgs_pts <- SpatialPoints(template_raster)
crs(wgs_pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
wgs_pts_st <- st_as_sf(wgs_pts)

# Get the 
pt_count <- lengths(st_intersects(polys_st, wgs_pts_st))


##############

d$pt_count <- pt_count

# Example_Ratio <- example %>%
#   mutate(across(-Detrital_Divisor, ~ . / !! Detrital_Divisor)) %>%
#   select(all_of(Ratio_Elements))


#/  Attach data to polygons
polys_df <- SpatialPolygonsDataFrame(polys, d)

# make raster of point count
pt_count_r <- fasterizeDT(polys_df, template_raster, field = "pt_count", fun="first")



# /----------------------------------------------------------------------------#
#/   RATERIZE 

# initialize output stack
giems2_stack <- stack()

# initialize cluster
beginCluster(4, type='SOCK')


### Loop through timesteps
# for (i in c(121:132)){  
for (i in c(1:length(tstep_ls))){

  # Get time step of loop
  tstep <- tstep_ls[i]
  print(tstep)
  
  # Fasterize 
  r <- fasterizeDT(polys_df, template_raster, field = tstep, fun="first")
  
  # divide are by number of pixels in each polygon
  r <- r / pt_count_r
  
  # Add raster to output stack
  giems2_stack <- stack(giems2_stack, r) 
  }


# cellStats(r, 'sum')/10^6
# cellStats(r2, 'sum')/10^6


# Save to file
writeRaster(giems2_stack, '../output/results/natwet/preswet/giems2_aw_v3.tif', overwrite=T)

# Stop parrallelizing
endCluster()


giems2_raw_sum$reproj_area <- cellStats(giems2_stack, 'sum')/10^6


# /----------------------------------------------------------------------------#
#/ 

giems2_original_1 <- d %>% select(V1:V10)

giems2_original_1_lat <- giems2_original_1 %>% 
                         mutate(lat = round(V2, 0)) %>%  
                         group_by(lat) %>% 
                         summarize(area = sum(V10, na.rm=T)) %>%
                         mutate(version = 'Original')



# Get monthly stack of GIEMS2
giems2_reproj_1 <- stack('../output/results/natwet/preswet/giems2_aw_v3.tif')
giems2_reproj_1 <- giems2_reproj_1[[7]]
giems2_reproj_1_lat <- raster2df(giems2_reproj_1) %>% 
                       mutate(lat= round(y, 0)) %>% 
                       group_by(lat) %>% 
                       summarize(area = sum(giems2_aw_v3.7, na.rm=T)) %>% 
                       mutate(version = 'Reprojected')


giems2_lat <- bind_rows(giems2_original_1_lat, giems2_reproj_1_lat)

glimpse(giems2_lat)

# Make lat barplot plot
p <- 
  ggplot(data= giems2_lat) +
  geom_bar(aes(x=lat, y=area, fill=version, color=version), stat='identity') + 
  # geom_line(data= giems2_reproj_1_lat, aes(x=lat, y=reproj_area), color='red', size=0.4) +
  facet_wrap(~version) +
  theme_classic2() +
  ggtitle('July 1992') +
  xlab('Latitude') +
  ylab('GIEMS2 area\n(km2 per 1deg latitude)') +
  coord_flip() +
  theme(legend.position = 'none')
  
p


#/ Save 
ggsave('../output/figures/giems2_reproj_lat_profile.png', p,
       width=180, height=100, dpi=300, units="mm") #type = "cairo-png")
dev.off()



# Calc sums
giems2_reproj_1 <- stack('../output/results/natwet/preswet/giems2_aw_v3.tif')
reproj_sums <- cellStats(giems2_reproj_1, 'sum')
reproj_sums <- cellStats(giems2_reproj_1, 'sum')




# /----------------------------------------------------------------------------#
#/ WRONG OLD WAY OF CONVERTING                            ---------------
# if(0){
#   # Read in .dat file
#   f <- '../data/natwet/giems2/wetland_1992_2015_Global_monthly_v2.dat'
#   d <- read.delim(f, header = FALSE, stringsAsFactors = FALSE, sep = "", check.names=FALSE)
#   
#   # Make template raster in WGS84;  0.25*0.25deg
#   template_r = raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180*4, ncols=360*4, crs="+init=epsg:4326")
#   
#   # Create output
#   out_r <- stack()
#   
#   # EPSG:6933 WGS 84 / NSIDC EASE-Grid 2.0 Global
#   # EPSG:3410 NSIDC EASE-Grid Global
#   
#   # Loop through 
#   for(i in c(121:144)){
#     
#     print(i)
#     # Read image as points
#     p <- SpatialPointsDataFrame(d[c(3,2)], d[(3+i)])
#     # Conver to df  
#     pr_df <- as.data.frame(p)
#     # Assign column names
#     names(pr_df) <- c('aw','lon','lat')
#     
#     # Remove zeros (optional); these cells become NA in raster
#     pr_df <- pr_df %>% filter(aw!=0)
#     
#     # Rasterize the points over the WGS84 template
#     r <- rasterize(pr_df[, 2:3], template_r, pr_df[,1], fun=mean)
#     # Add image to stack
#     out_r <- stack(out_r, r)
#   }
#   
#   # Save regridded GIEMS2
#   writeRaster(out_r, '../output/results/giems2.tif', overwrite=T)
# }


# # /----------------------------------------------------------------------------#
# #/ read GIEMSv2 from original .DAT file
# # The data are gridded on an equal area grid of 0.25°x0.25° at the equator.
# # Each grid cell is 773km2 in surface. Monthly for 24 years (1992-2015). 
# 
# f <- '../data/nat_wetland_maps/wetland_1992_2015_Global_monthly_v2.dat'
# 
# d <- read.delim(f, header = FALSE, stringsAsFactors = FALSE, sep = "", check.names=FALSE)
# # d <- read.table(f, header = FALSE, stringsAsFactors = FALSE, quote = "", sep = "\t")
# 
# ### 0.25*0.25 degree resolution and extent -180, 180, -90, 90
# template_r = raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180*4,ncols=360*4,crs="+init=epsg:4326")
# 
# # Create output
# out_r <- stack()
# 
# # EPSG:6933 WGS 84 / NSIDC EASE-Grid 2.0 Global
# # EPSG:3410 NSIDC EASE-Grid Global
# # 4-288
# for(i in c(121:288)){
#   
#   print(i)
#   p <- SpatialPointsDataFrame(d[c(3,2)], d[(3+i)])
#   
#   pr_df <- as.data.frame(p)
#   names(pr_df) <- c('aw','lon','lat')
#   pr_df <- pr_df %>% filter(aw!=0)
#   
#   r <- rasterize(pr_df[, 2:3], template_r, pr_df[,1], fun=mean)
#   out_r <- stack(out_r, r)
# }
# 
# giems2_mean = calc(out_r, fun=mean, na.rm=T)  # mean is ~ 5.98 Mkm2
# giems2_max = calc(out_r, fun=max, na.rm=T)   # max is ~ 10.78 Mkm2
# cellStats(giems2_max, stat='sum', na.rm=T)/10^6
# 
# # save
# writeRaster(out_r, '../output/results/giems2.tif', overwrite=T)
# 
# 
# # /----------------------------------------------------------------------------#
# #/ Get GIEMSv2
# giems2 <- stack('../output/results/giems2.tif')
# # calculate time-series mean
# # giems2_mean <- calc(giems2, fun=mean, na.rm=T)
# giems2_max <- calc(giems2, fun=max, na.rm=T)
# 
# 
# 
# # Get OW fraction
# glwd3_ow <- raster('../output/results/preswet/glwd3_ow_fw.tif')
# glwd3_ow[is.na(glwd3_ow)] <- 0 
# # Get OW area
# glwd3_ow_a <- glwd3_ow * area(glwd3_ow)
# 
# # Subtract open waterbodies from GIEMSv2
# giems2_max_wet <- giems2_max - glwd3_ow_a
# # Turn 0s into NAs (when more OW than wetland)
# giems2_max_wet[giems2_max_wet < 0] <- NA
# 
# # Save to file
# writeRaster(giems2_max_wet, '../output/results/preswet/giems2_max_wet.tif', overwrite=T)
# 
# giems2_mean = calc(out_r, fun=mean, na.rm=T)  # mean is ~ 5.98 Mkm2
# giems2_max = calc(out_r, fun=max, na.rm=T)   # max is ~ 10.78 Mkm2
 
# cellStats(giems2_max, stat='sum', na.rm=T)/10^6

### FROM CATHERINE PRIGENT'S  INSTRUCTIONS TO READ DATA IN FORTRAN

# The data are gridded on an equal area grid of 0.25°x0.25° at the equator.
# To read the ascii file in fortran:
# c integer icell, imonth,surf(288)
# c real alat,alon
# read(30,*) icell,alat,alon,(surf(imonth),imonth=1,288)
# c icell = for internal use only
# c alat = latitude between -90 and 90 of the center of the pixel
# c alon = longitude between -180 and 180 of the center of the pixel
# c surf(imonth) = inundated surface in km2 for the 773km2 pixel.




# # /----------------------------------------------------------------------------#
# #/ Reread GIEMSv2;  Calculate mean
# giems2 <- stack('../data/nat_wetland_maps/giems2.tif')
# # calculate time-series mean
# giems2_mean <- calc(giems2, fun=mean, na.rm=T)
# # Subtract open waterbodies from GIEMSv2
# giems2_mean_wet <- giems2_mean - glwd3_agg_area
# giems2_mean_wet[giems2_mean_wet < 0] <- NA
# 
# writeRaster(glwd3_agg_frac, '../output/results/preswet/giems2_wet.tif', overwrite=T)