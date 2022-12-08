
raster2df <- function(x){as.data.frame(as(x, 'SpatialPointsDataFrame'))}


#==============================================================================#
### make function that gets raster number of max wetloss -----------------------
#==============================================================================#

which.max.na <- function(x, ...) ifelse(length(x) == sum(is.na(x)), 0, which.max(x))

#==============================================================================#
### selects rasters by name ----------------------------------------------------
#==============================================================================#

sel.by.pattern <- function(x, y) x[[grep(names(x), pattern=y)]]

#==============================================================================#
### FUNCTION: overlay where not 0 ----------------------------------------------
#==============================================================================#

overlay.where.not0 <- function(r1,r2,val=0){
  r3 <- r1
  r3[r2!=val] <- r2[r2!=val]
  return(r3) }

#==============================================================================#
### make raster sum function (shorten) -----------------------------------------
#==============================================================================#

sum_raster <- function(raster){sum(cellStats(raster, stat="sum"))}


#==============================================================================#
### reproject and convert POLYGON SHP to df for ggplot mapping -----------------
#==============================================================================#

prep_poly_into_robin_map <- function(x){
  crs(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  y <- spTransform(x, CRS("+proj=robin"))  # reproject bounding box
  y <- fortify(y)
  y <- as.data.frame(y)
  return(y)}


#==============================================================================#
### reproject and convert RASTER to df for ggplot mapping ---------------------
#==============================================================================#

prep_raster_into_robin_map <- function(x){
  crs(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  y <- projectRaster(x, crs=CRS("+proj=robin"), method='ngb')
  y <- as(y, "SpatialPixelsDataFrame")
  y <- as.data.frame(y)
  return(y)}



#==============================================================================#
### reproject and convert POLYGON SHP WITH its own data to df for ggplot mapping ---------------------
#==============================================================================#

# x <-artdrain_nat
# table <- drained

# function that reprojects and converts rasters to df for plotting in ggplot
prep_poly_into_robin_map_wdata <- function(x){
  
  # set projection of incoming shp as WGS84
  crs(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # transform to robin
  x_robin <- spTransform(x, CRS("+proj=robin"))
  
  # add id column to transformed shapefile
  #x_robin@data <- x_robin@data[,c(1,6)]
  # x_robin@data <- merge(x_robin@data, table, by="rec_id")
  x_robin@data$id <- as.numeric(rownames(x_robin@data))
  
  # fortify the reproj shapefile
  x_df <- fortify(x_robin, region = "id")
  
  # 
  x_df <- merge(x_df, x_robin@data, by="id")
  
  return(x_df)}



#==============================================================================#
### reproject and convert POLYGON SHP WITH DATA TABLE to df for ggplot mapping ---------------------
#==============================================================================#

# x <-artdrain_nat
# table <- drained

# function that reprojects and converts rasters to df for plotting in ggplot
prep_poly_into_robin_map_mergedata <- function(x, table){
  
  # set projection of incoming shp as WGS84
  crs(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # transform to robin
  x_robin <- spTransform(x, CRS("+proj=robin"))

  # add id column to transformed shapefile
  x_robin@data <- x_robin@data[,c(1,6)]
  x_robin@data <- merge(x_robin@data, table, by="rec_id")
  x_robin@data$id <- as.numeric(rownames(x_robin@data))
  
  # fortify the reproj shapefile
  x_df <- fortify(x_robin, region = "id")
  
  # 
  x_df <- merge(x_df, x_robin@data, by="id")
  
  return(x_df)}
