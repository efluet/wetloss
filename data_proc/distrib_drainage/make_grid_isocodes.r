
# Needs this grid of maximum landarea to run the ISOGRID function
maxlncr <- raster('../data/lucc/hyde32_beta/general_files/maxln_cr.asc', 
                  crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
origin(maxlncr) <- c(0,0)
maxlncr <- extend(maxlncr, extent(-180, 180, -90, 90))
maxlncr <- aggregate(maxlncr, fact=6, fun='sum')



# /-----------------------------------------------------------------------------
#/   MAKE GRID OF COUNTRY ISO CODES 

library(rworldmap)
sPDF <- getMap()[getMap()$ADMIN!='Antarctica','ISO_A3']

# this has 242 observations (hence 242 indiv countries)
isolookup <- as.data.frame(sPDF$ISO_A3)
isolookup$val <- as.numeric(sPDF$ISO_A3)

# then we lose some small countries in the gridding
ciso <- rasterize(sPDF, maxlncr, "ISO_A3")
ciso <- ratify(ciso)


isolookup2 <- isolookup %>% filter(isolookup$val %in% c(levels(ciso)[[1]])$ID)
names(isolookup2) <- c("ISO_A3", "ID")
isolookup2 <- isolookup2[,c("ID", "ISO_A3")]
isolookup2$ISO_A3 <- as.character(isolookup2$ISO_A3) 
# levels(ciso) <- isolookup2


# CONVERT TO DF
ciso_df <- data.frame(as(ciso, 'SpatialPointsDataFrame'))  %>% 
           left_join(., isolookup2, by=c('layer'='ID')) %>% 
           dplyr::select(x,y,ISO_A3)


# ciso_df <- raster2df(ciso)
ciso_df$x = round(ciso_df$x, 2)
ciso_df$y = round(ciso_df$y, 2)
ciso_df <- left_join(maxlncr_df_xy, ciso_df, by=c('x','y'))
# ciso_df_adm3a <- ciso_df$layer
# names(ciso_df_adm3a) <- c('adm3a')