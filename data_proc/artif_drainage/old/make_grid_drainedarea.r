# /----------------------------------------------------------------------------#
#/    Get fcn extracting drain types                                   --------- 
#     Different drain types are run separately because they rely on different raster sources
#     function:  process_drainage_stats

# Inputs:
#   - Potential wetland grid (1 layer)
#   - Stack of wetloss area per driver (1700-2000), subsetted to random 

# /-----------------------------------------------------------------------------
#/   Get function calculating % drained
#source('./scripts/data_proc/artif_drainage/fcn/fcn_calc_percent_artif_drainage_pertype_v2.r')


# /-----------------------------------------------------------------------------
#/   Read interpolated drainage area table
drainage <- read.csv("./output/results/artif_drainage/drained_wetcult_ha_sigmoidpred.csv")


# read raster to GeoTiff
potwet <- raster("./output/results/potwet/potwet.tif")
potwet[potwet<0] <- 0

# /-----------------------------------------------------------------------------
#/   GET GRIDS OF COUNTRY ISO CODES 

library(rworldmap)
sPDF <- getMap()[getMap()$ADMIN!='Antarctica','ISO_A3']

# this has 242 observations
isolookup <- as.data.frame(sPDF$ISO_A3)
isolookup$val <- as.numeric(sPDF$ISO_A3)

# then we lose some small countries in the gridding
ciso <- rasterize(sPDF, potwet, "ISO_A3")
ciso <- ratify(ciso)


isolookup2 <- isolookup %>% filter(isolookup$val %in% c(levels(ciso)[[1]])$ID)
names(isolookup2) <- c("ISO_A3", "ID")
isolookup2 <- isolookup2[,c("ID", "ISO_A3")]
levels(ciso) <- isolookup2

# split into 0.5deg
#ciso05 <- disaggregate(ciso, fact=2)




# /-----------------------------------------------------------------------------
#/   Loop through years
for (y in 2000:2000){
  
  
  # subset drainage data per year
  
  # /----------------------------------------------------------------------------#
  #/    Cropland                                                           -------
  
  # losese the names when saved as TIFF; need to pass as list
  wetloss_Mk2_stack<- brick('./output/results/wetloss/grid/lu_wet_overlap_stack.tif')
  names(overlap_Mk2_stack) <- namelist
  
  # subset to each 3rd grid (rdm?)
  wetloss_Mk2_stack<- wetloss_Mk2_stack[[1]]
  
  # Sum LU-potwet overlapping area per country
  z <- as.data.frame(zonal(wetloss_Mk2_stack, ciso, 'sum'))
  
  # convert from wide to long table format
  z <- z %>% gather(key="time", value="value", -zone)
    
  # join the ISO codes
  z <- left_join(z, isolookup, by=c("zone"="val"))
  
  
  # /--------------------------------------------------------------------------#
  #/ Calculate each pixels % of national overlap (of wet-crop)             -----
  # by dividing the pixels by the national total of overlap.
  # Using Random scenario since it's middle of the road? Or Preference? (It shouldn't matter much in the distribution...)
  # Why are there negative values?  becaus of negative pot-wet?
  
  zt <- z %>% 
        #filter(time=="lu_wet_overlap_stack.18") %>%
        select(zone, value)
  
  #ciso05<-ratify(ciso05)
  
  # Subsitute values of raster based on country ISO code
  overlap_sum <- subs(ciso, zt, by=1, which=2, subsWithNA=TRUE)# , filename='')
  
  # calc percentage
  perc_overlap <- wetloss_Mk2_stack[[1]] / overlap_sum

  
  # /--------------------------------------------------------------------------#
  #/  MAKE GRID OF DRAINAGE STATS
  drainage_sub <- drainage %>% 
                  select(-X) %>%
                  filter(year == y ) %>%
                  filter(type == "cropland") %>%
                  # add country codes
                  mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F)) %>%
                  left_join(., isolookup2, by=c("iso_a3"="ISO_A3")) %>%
                  distinct() %>%
                  filter(!is.na(ID))
  
  # Subsitute values of raster based on country ISO code
  drainage_sum <- subs(ciso, drainage_sub, by=7, which=1, subsWithNA=TRUE) #filename='')
  
  
  # Then multiply the % of overlap with the national drained area.
  # That distributes the draineed area over the pixels.
  drainage_distributed <- perc_overlap * drainage_sum
  
  
  # Then exclude the negative percentage.
  drainage_distributed[drainage_distributed>0] <- 0
  
  # Does order of intersect matter?  The order should exhaust the available wetland area in the overlay too.
  # Rice > Cropland  > Pasture > Ag
  
  }



# Output design: Save as multivariable NCDF
# Time dim; 1700-2000
# Variables: 
#       - Potential wetland
#       - Cropland (documented area)
#             - Irrigated
#             - Rainfed
#             - Rice
#       - Pastureland(10% drained; Pavelis1987)
#       - Forestry (documented area)
#       – Peat extraction (documented area)
#       – Urban areas (map overlay)





# Pin drained area wherever we have data, then use 
# - Reconstructed drainage expansion based on national drainage stats
#     - Null model: drainage expanded at the same rate as the LU (e.g. cropland, forestry).
#     - Full range of overlap: avoidance, random, preference




# to do list:
#   - test LUH  v2 forestry
#   - add in the subnational data
#   - grid the drainage onto natwetgrid
#   - separate spate irrig
#   - separate irrig vs rainfed 



# /----------------------------------------------------------------------------#
#/     Forestry                                                         --------

# make grid of wood harvest
source('./scripts/data_proc/prep_data/wood_harvest/save_wood_luhv2.r')
forest_harv_stack <- stack("./output/results/forest_harv.nc")



crs(forest_harv_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
forest_harv_stack <- forest_harv_stack * area(forest_harv_stack[[1]])
# make sure this comes last, and names don't get overwritten
names(forest_harv_stack) <- seq(1700,2000,10) 

# run function 
df_f <- process_drainage_stats(drained, "forestry", forest_harv_stack)



#==============================================================================#
###    Peatland extraction                --------------------------------------
#==============================================================================#

# debug peatland
l <- './data/lpx_dytop_2014/s1_dyptop_output.nc'
lpx <- nc_open(l)
lpx_peat <-brick(l, varname="lu_area", level=2)   # peat doesn't change between month of the same year.
last_idx <- dim(lpx_peat)[3]

# get raster of gridcell area, accounting for projection
area <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")

# aggregate grid to resolution of wetland data
area <- aggregate(area, fact= c(2), fun=sum)


peat_stack <- lpx_peat[[last_idx]]  *  area

peat_stack <- stack( mget( rep( "peat_stack" , 31)))


names(peat_stack) <- seq(1700,2000,10)


# clean up environment
rm(area, l, lpx, lpx_peat, last_idx)



### THIS RUNS ON TOO MANY POLYGONS, DONT NEED YEARLY DATA, SUBSAMPLE TO DECADAL
df_p <- process_drainage_stats(drained, "peatland", peat_stack)



#==============================================================================#
# combine the extracted df                 -------------------------------------  
#==============================================================================#
drained_wfrac <- bind_rows(df_c, df_f, df_p)


# write output to file
write.csv(drained_wfrac, "./output/results/artif_drainage/drained_wfrac.csv")


rm(df_c, df_f, df_p)
