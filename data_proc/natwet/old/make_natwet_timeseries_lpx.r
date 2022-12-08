# description: downscale and harmonize LPX Holocene simulation
# and LPX simulation for 1901-2012

par(mar=c(1,1,1,1))

#==============================================================================#
### Read/select 20th century LPX natwet data  ----------------------------------
#==============================================================================#

# time dimension is monthly over 112 years (1901-2012)
# variable names are:  "inund", "lu_area", "wtpos"
l <- './data/lpx_dytop_2014/s1_dyptop_output.nc'
lpx <- nc_open(l)

# get years; and round to remove 5 as last digit
lpx_yrs <- sort(lpx$dim$TIME$vals)

# make empty list
yr_ls <- c()

# fill list with 12 month intervals of decade years (1910, 1920, 1930, ...)
for(y in 0:10){ yr_ls <- c(yr_ls, c(seq(97+120*y,108+120*y,1))) }

# subset the inundation =======================================================#
lpx_inund <-brick(l, varname="inund")
lpx_inund <- lpx_inund[[yr_ls]]


# subset lu area ==============================================================#

# level:  1= area? , 2= peat?  , 3= new peat?
# get gridcell area
lpx_upland <- brick(l, varname="lu_area", level=1)
lpx_upland <- lpx_upland[[yr_ls]]

# peat doesn't change between month of the same year.
lpx_peat <-brick(l, varname="lu_area", level=2)
lpx_peat <- lpx_peat[[yr_ls]]

# get gridcell area
lpx_new_peat <- brick(l, varname="lu_area", level=3)
lpx_new_peat <- lpx_new_peat[[yr_ls]]


# lpx_grid_fraction <- lpx_20th_peat + lpx_luarea1 + lpx_luarea3
# lpx_grid_fraction <- lpx_grid_area[[1]]


### 1x1 grid area  Mkm2 =======================================================#

# get raster of gridcell area, accounting for projection
area_05x05 <- raster("./data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")
# aggregate grid to resolution of wetland data
area_1x1 <- aggregate(area_05x05, fact= c(2), fun="sum") 
rm(area_05x05)


#==============================================================================#
### FUNCTION: MAKE AMAX STACKS FROM LPX DATA -----------------------------------
#==============================================================================#

make_annmax_stack <- function(lpx_stack, output_stack){
  
  for (i in seq(0,10)){
  
    # create index for 12 months
    sel_years <- seq(1,12) + 12*i
    print(sel_years)
    
    # subset months of a given year
    temp_monthly <- lpx_stack[[sel_years]]
    # calc max value in each pixel
    temp_amax <- max(temp_monthly) * area_1x1
    # get year of the current iteration
    temp_year <- substr(names(temp_monthly[[1]]),2,5)
    # add name to the max raster
    names(temp_amax) <- paste0("amax", temp_year)
    # add the max raster to the stack
    output_stack <- stack(output_stack, temp_amax)
  }
  # return stack with appended layer
  return(output_stack)
}


#==============================================================================#
### FUNCTION: SUM AREA FROM AMAX GRIDS -----------------------------------------
#==============================================================================#

sum_each_raster<- function(input_stack, output_df, label){
  
  for (i in seq(1:length(names(input_stack)))) {
    
    temp <- input_stack[[i]]
    temp_name <- names(temp)
    temp_year <- as.numeric(substr(temp_name,5,8))
    
    sum <- sum_raster(temp)/10^6
    
    # attach the sum_inund and year to output df
    output_df <- bind_rows(output_df, 
                           data.frame(temp_year, sum, label))
  } 
  return(output_df)
}



#==============================================================================#
### Read 20th century LPX as rasters --------------------------------------------
#==============================================================================#





# make empty stack of rasters
inund_annmax_stack <- stack()
inund_annmax_stack <- make_annmax_stack(lpx_inund, inund_annmax_stack)

# make empty stack of rasters
peat_annmax_stack <- stack()
peat_annmax_stack <- make_annmax_stack(lpx_peat, peat_annmax_stack)




# make empty output df
sum_area_df <- data.frame()

sum_area_df<- sum_each_raster(inund_annmax_stack, sum_area_df, "Inund LPX Stocker 2014")
sum_area_df<- sum_each_raster(peat_annmax_stack, sum_area_df, "Peat LPX Stocker 2014")
sum_area_df



for (i in seq(0,10)){
  
  # create index for 12 months
  sel_years <- seq(1,12) + 12*i
  print(sel_years)
  
  # subset months of a given year
  temp_inund_monthly <- lpx_inund[[sel_years]]
  # calc max value in each pixel
  temp_inund_amax <- max(temp_inund_monthly) * area_1x1
  # add name to the max raster
  temp_year <- substr(names(temp_inund_monthly[[1]]),2,5)
  names(temp_inund_amax) <- paste0("amax", temp_year)
  # sum_inund to Mkm^2
  sum_inund <- sum_raster(temp_inund_amax)/10^6
  
  
  
  # subset months of a given year
  temp_peat_monthly <- lpx_peat[[sel_years]]
  # calc max value in each pixel
  temp_peat_amax <- max(temp_peat_monthly) * area_1x1
  # add name to the max raster
  temp_year <- substr(names(temp_peat_monthly[[1]]),2,5)
  names(temp_peat_amax) <- paste0("amax", temp_year)
  # sum_inund to Mkm^2
  sum_peat <- sum_raster(temp_peat_amax)/10^6
  
  
  
  
  ### Make outputs
  
  # add the max raster to the stack
  natwet_timeline <- stack(natwet_timeline, temp_inund_amax)  

  # attach the sum_inund and year to output df
  sumarea_df <- bind_rows(sumarea_df, 
                          data.frame(as.numeric(temp_year), 
                                     as.numeric(sum_inund), 
                                     "LPX Stocker 2014"))

}
  
# rename the columns
names(sumarea_df) <- c("year", "natwet_type", "area_Mkm2", "model")
  



#==============================================================================#
### Plot 20th century wetland cover ----------------------------------------------
#==============================================================================#


ggplot(sumarea_df) + geom_point(aes(x=year, y=tot_natwet_Mkm2)) + geom_line(aes(x=year, y=tot_natwet_Mkm2))



# 
# # subset the hyde years to those also found in LPX (e.g removes years after 1980)
# hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]
# hyde_yrs <- hyde_yrs[which(hyde_yrs >= -6000)]
# 
# # get the index of hyde years in lpx data 
# lpx_indx <- match(hyde_yrs, lpx_yrs)
# 
# # get the index of hyde years in lpx data
# idx_subset_yrs <- which((lpx_yrs %in% hyde_yrs) == TRUE)
# 
# # get grid area - originally in m^2; convert to km2
# area <- raster(l, varname="area") / 10^6 / 10^6







#==============================================================================#
### Long term wetland data  (Stocker 2017)
###Get HYDE years & indexes from ncdf -------------------------------------------
#==============================================================================#

h <- './output/results/hyde_resampled/hyde32_2.5.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals) # get years
hyde_indx <- match(hyde_yrs, hyde$dim$time$vals)


#==============================================================================#
### Read stocker 2017 ncdf  -----------------------------------------------------------
#==============================================================================# 

# read the NetCDF 
l <- './data/nat_wetland_map/trace21_129.cdf'
lpx <- nc_open(l)

# get years; and round to remove 5 as last digit
lpx_yrs <- sort(lpx$dim$TIME$vals)  -5

# subset the hyde years to those also found in LPX (e.g removes years after 1980)
hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]
hyde_yrs <- hyde_yrs[which(hyde_yrs >= -6000)]

# get the index of hyde years in lpx data 
lpx_indx <- match(hyde_yrs, lpx_yrs)

# get the index of hyde years in lpx data
idx_subset_yrs <- which((lpx_yrs %in% hyde_yrs) == TRUE)

# get grid area - originally in m^2; convert to km2
area <- raster(l, varname="area") / 10^6 / 10^6


#==============================================================================#
### Sum_inund LPX nat wet area -------------------------------------------------------
#==============================================================================#

# create empty stack
lpx_natwet_Mkm2_stack <- stack()

# loop through hyde years
for (t in 1:length(hyde_yrs)){
  
  # print tickers
  yr <- hyde_yrs[t] 
  print(paste("Year from hyde: ", yr, sep=""))
  print(paste("Year from LPX: ", lpx$dim$TIME$vals[lpx_indx[t]], 
              "  at index of LPX: ", lpx_indx[t], sep=""))
  
  
  # read inund
  natwet <- max(brick(l, varname="inund", level=lpx_indx[t], lvar=4))
  
  # calc nat wet area from the fraction
  natwet <- natwet * area
  
  names(natwet) <- paste0("year",yr)
  
  lpx_natwet_Mkm2_stack <- stack(lpx_natwet_Mkm2_stack, natwet)
  
  
  # sum_inund global area to Mkm^2
  sum_inund <- sum_raster(natwet)
  
  # attach the sum_inund and year to output df
  sumarea_df <- bind_rows(sumarea_df, 
                          data.frame(year = hyde_yrs[t], 
                                     tot_natwet_Mkm2= sum_inund, 
                                     model="LPX-DYTOP wetland Stocker 2017"))
}

# rename the columns
names(sumarea_df) <- c("year", "tot_natwet_Mkm2", "model")



#==============================================================================#
### Get peatland grid from LPX  ==================================================
#==============================================================================#

# create empty stack
peat_area_df<- stack()

for (t in 1:length(hyde_yrs)){
  
  # print tickers
  yr <- hyde_yrs[t] 
  print(paste("Year from hyde: ", yr, sep=""))
  print(paste("Year from LPX: ", lpx$dim$TIME$vals[lpx_indx[t]], 
              "  at index of LPX: ", lpx_indx[t], sep=""))
  
  
  # read peatland area
  p <- raster(l, varname="lu_area", band=idx_subset_yrs[t], level=4)
  
  # calculate area
  p <- p * area
  # name grid
  names(p) <- paste0("year",yr)
  # add grid to stack
  peat_area_df <- stack(peat_area_df, p)
  
  # sum_inund to Mkm^2
  sum_inund <- sum_raster(p) # lpx_natwet_Mkm2_stack[[i]])
  
  # attach the sum_inund and year to output df
  sumarea_df <- bind_rows(sumarea_df, 
                          data.frame(as.numeric(yr), as.numeric(sum_inund), "LPX-DYTOP peatland"))
  
}

# rename the columns
names(sumarea_df) <- c("year", "tot_natwet_Mkm2", "model")





#==============================================================================#
### Downscale coarse LPX grid -------------------------------------
#==============================================================================#

# Because any estimation of wetland loss is dependent on
# the colocation of wetland and intersection of wetland and land use,
# we downscaled and corrected the LPX simulation to match the ORCHIDEE simulation 
# to produce a continuous 1x1deg grid


# calculate orchidee baseline
orchidee_b <- mean(orchidee_nat_wet_timeline)
sum_raster(orchidee_b)

# disaggregate orchidee to 0.25 res, dividing the area equally
orchidee_b_d <- disaggregate(orchidee_b, fact=4) / (4*4)
sum_raster(orchidee_b_d)

# reaggregate the grid to orchidee to 3.75 x 2.5, calculating the sum_inund or area
orchidee_b_dr <- aggregate(orchidee_b_d, fact=c(15,10), na.rm=TRUE, fun="sum_inund")
sum_raster(orchidee_b_dr)

# disaggregate the sums back to 0.25 res
orchidee_b_drd <- disaggregate(orchidee_b_dr, fact=c(15,10), na.rm=TRUE)
sum_raster(orchidee_b_drd)

# calculate percentage of grid
orchidee_b_drd_p = orchidee_b_d / orchidee_b_drd 
orchidee_f <- aggregate(orchidee_b_drd_p, fact=c(15,10), na.rm=TRUE, fun="sum_inund")


### Disaggregate the LPX ---------------------------------------------------------

# dissage LPX
lpx_natwet_Mkm2_stack_d <- disaggregate(lpx_natwet_Mkm2_stack, fact=c(15, 10), method="")



# set origin of hyde grids
origin(lpx_natwet_Mkm2_stack_d) <- origin(orchidee_b_drd_p)
ext <- extent(-180, 180, -90, 90)
lpx_natwet_Mkm2_stack_d <- extend(lpx_natwet_Mkm2_stack_d, ext)

# multiply by the subgrid %
lpx_natwet_Mkm2_stack_d <- lpx_natwet_Mkm2_stack_d[[1]] * orchidee_b_drd_p

# reaggregate to 1x1deg
lpx_natwet_Mkm2_stack_d1deg <- aggregate(lpx_natwet_Mkm2_stack_d, fact=4, na.rm=TRUE, fun="sum_inund")


#  WHY IS THERE A 20% AREA GAIN...?
sum_raster(lpx_natwet_Mkm2_stack_d1deg[[1]])
sum_raster(lpx_natwet_Mkm2_stack_d[[1]])
sum_raster(lpx_natwet_Mkm2_stack[[1]])








#===============================================================================
# 
# apply( lu_area, c(1,2,4), FUN = sum_inund ) * inund
# 
# lu_area <- ncvar_get(lpx, varid="lu_area")
# lu_area <- lu_area[,,,1:3]
# 
# inund <- ncvar_get(lpx, varid="inund")
# inund <- inund[,,1:3]
# 
# grid_area <- apply( lu_area, c(1,2,4), FUN = sum_inund ) * inund
# x <- getData(grid_area)
# lpx_gridarea <- stack(grid_area)
# 
# #x <- getData('worldclim',var='tmean',res=10)
# 
# lpx_gridarea <- brick(grid_area)
