# /----------------------------------------------------------------------------#
#/      GET PACKAGES & PLOT THEMES                                    ----------
library(terra);
library(geosphere)
library(fasterize)
library(rnaturalearth)
library(rworldmap)
# import packages
source('./data_proc/fcn/import_libraries.r')

# load miscellaneous functions
source('./data_proc/fcn/fcn_misc.r')
source('./data_proc/fcn/fcn_ifrm.r')

# get ggplot & mapping theme
source('./plots/themes/map_theme.r')
source('./plots/themes/map_raster_theme.r')
source("./plots/themes/line_plot_theme.r")

# get country and bbox polygons for mapping
source('./plots/fcn/get_country_bbox_shp_for_ggplot_map.r')


options(row.names=FALSE, scipen=100, digits=6, stringsAsFactors = FALSE, row.names=FALSE)


# /----------------------------------------------------------------------------#
#/   Make template raster
template <- raster(nrows=180*2, ncols=360*2, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                   crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', vals=NULL)

# Make template at 0.25deg 
template_025deg <- raster(nrows=180*4, ncols=360*4, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                          crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', vals=NULL)




### FIX THE CORNER DUPLICATE RASTERS IN ROBINSON PROJ

# Function converting format & proj 
# Question: is the the one that prevents exceeded area in robinson proj?
WGSraster2dfROBIN <- function(r){
  library(terra)
  crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  r <- terra::rast(r)
  r_robin <- terra::project(r, '+proj=robin', method='near', mask=T)
  r_robin_df <- as.data.frame(r_robin, xy=TRUE, na.rm=TRUE) 
  return(r_robin_df) }




# # /----------------------------------------------------------------------------#
# #/   Function converting format & proj 
# WGSraster2dfROBIN <- function(r){
#   library(terra)
#   crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
#   r <- terra::rast(r)
#   r_robin <- terra::project(r, '+proj=robin', method='near', mask=T)
#   r_robin_df <- as.data.frame(r_robin, xy=TRUE, na.rm=TRUE) 
#   
#   # r_robin <- projectRaster(r, crs=CRS('+proj=robin'), method='ngb', over=TRUE)
#   # r_robin <- as(r_robin, 'SpatialPixelsDataFrame')
#   # r_robin_df <- as.data.frame(r_robin)
#   
#   return(r_robin_df)
# }
# 


# # /----------------------------------------------------------------------------#
# #/   Function converting format & proj 
# WGSraster2dfROBIN <- function(r){
#   
#   crs(r) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
#   r_robin <- projectRaster(r, crs=CRS('+proj=robin'), method='ngb', over=TRUE)
#   
#   r_robin <- as(r_robin, 'SpatialPixelsDataFrame')
#   r_robin_df <- as.data.frame(r_robin)
#   
#   return(r_robin_df)
# }
