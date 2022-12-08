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


#==============================================================================#
### Long term wetland data  (Stocker 2017)
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
area <- raster(l, varname="area") / 10^6



#==============================================================================#
### Make Stocker 2017 stacks -------------------------------------------------
#==============================================================================#

# create empty stack
inund_stack_lpx2017 <- stack()
peat_stack_lpx2017 <- stack()


# loop through hyde years
for (t in 1:length(hyde_yrs)){
  
  # print tickers
  year <- hyde_yrs[t] 
  print(paste("Year from hyde: ", year, sep=""))
  print(paste("Year from LPX: ", lpx$dim$TIME$vals[lpx_indx[t]], 
              "  at index of LPX: ", lpx_indx[t], sep=""))
  
  # read inund & calculate maximum
  inund <- max(brick(l, varname="inund", level=lpx_indx[t], lvar=4))
  inund <- inund * area
  names(inund) <- paste0("amax", year)
  
  
  # read peatland area
  peat <- raster(l, varname="lu_area", band=idx_subset_yrs[t], level=2)
  peat <- peat * area
  names(peat) <- paste0("amax", year)
  
  # add rasters to stack
  inund_stack_lpx2017 <- stack(inund_stack_lpx2017, inund)
  peat_stack_lpx2017 <- stack(peat_stack_lpx2017, peat)  
  
}


#==============================================================================#
### Calculate sums area ---------------------------------------------------
#==============================================================================#

# make empty output df
sum_area_df <- data.frame()

sum_area_df<- sum_each_raster(inund_annmax_stack, sum_area_df, "Inund LPX Stocker 2014")
sum_area_df<- sum_each_raster(peat_annmax_stack, sum_area_df, "Peat LPX Stocker 2014")

sum_area_df<- sum_each_raster(inund_stack_lpx2017, sum_area_df, "Inund LPX Stocker 2017")
sum_area_df<- sum_each_raster(peat_stack_lpx2017, sum_area_df, "Peat LPX Stocker 2017")

# rename the columns
names(sum_area_df) <- c("year", "area_Mkm2", "type")


#==============================================================================#
### Plot wetland area   ----------------------------------------------
#==============================================================================#

ggplot(sum_area_df) + 
  geom_point(aes(x=year, y=area_Mkm2, color=type)) + 
  geom_line(aes(x=year, y=area_Mkm2, color=type)) +
  scale_y_continuous(limits=c(0,15))


#==============================================================================#
### Downscale coarse LPX grid -------------------------------------
#==============================================================================#

# Because any estimation of wetland loss is dependent on
# the colocation of wetland and intersection of wetland and land use,
# we downscaled and corrected the LPX simulation to match the ORCHIDEE simulation 
# to produce a continuous 1x1deg grid


# calculate orchidee baseline
inund <- mean(inund_annmax_stack)
sum_raster(inund) / 10^6

# disaggregate orchidee to 0.25 res, dividing the area equally
inund_d <- disaggregate(inund, fact=4) / (4*4)
sum_raster(inund_d) /  10^6

# reaggregate the grid to 3.75 x 2.5
inund_b_dr <- aggregate(inund_d, fact=c(15,10), na.rm=TRUE, fun="sum")
sum_raster(inund_b_dr) / 10^6

# disaggregate the sums back to 0.25 res
inund_b_drd <- disaggregate(inund_b_dr, fact=c(15,10), na.rm=TRUE)
sum_raster(inund_b_drd) / 10^6  # this step creates a sum ares x150 times larger

# calculate percentage of grid
inund_f = inund_d / inund_b_drd
hist(inund_f)  # for example, an evene split would be 1/150=0.006666

#inund_f <- aggregate(inund_f, fact=4, na.rm=TRUE, fun="sum")
#inund_f <- aggregate(inund_f, fact=c(15, 10), na.rm=TRUE, fun="sum")
#inund_f[inund_f > 1] <- 1 




### Disaggregate the LPX ---------------------------------------------------------

inund_stack_lpx2017_backup <- inund_stack_lpx2017



inund_stack_lpx2017 <- inund_stack_lpx2017_backup



sum_raster(inund_stack_lpx2017[[45]]) / 10^6



#origin(inund_stack_lpx2017) <- c(-180, -90) # origin(inund_f)
inund_stack_lpx2017 <- extend(inund_stack_lpx2017, extent(-180, 180, -55, 85))
inund_stack_lpx2017@extent@xmin <- -180
inund_stack_lpx2017@extent@xmax <- 180
#res(inund_stack_lpx2017) <- c(0.25, 0.25)



# dissagregate the grid to downscale
inund_stack_lpx2017_d <- disaggregate(inund_stack_lpx2017, fact=c(15, 10))
sum_raster(inund_stack_lpx2017_d[[45]]) /10^6    # 1355.73/150 = 8.86

# multiply grid with 0.25deg fractions
inund_stack_lpx2017_d[is.na(inund_stack_lpx2017_d)]<-0
inund_f[is.na(inund_f)]<-0

inund_stack_lpx2017_ds <- inund_stack_lpx2017_d * inund_f
sum_raster(inund_stack_lpx2017_ds[[45]]) /10^6


# reaggregate the grid to 1x1
inund_stack_lpx2017_dsd <- aggregate(inund_stack_lpx2017_ds, fact=c(4,4), na.rm=FALSE, fun="sum")

# somehow when aggregating hte fractional grid, the total is higher than 1...
#inund_f_r <- aggregate(inund_f, fact=c(4,4), na.rm=FALSE, fun="sum")


inund_stack_lpx2017_dsd[inund_stack_lpx2017_dsd > 12260.39] <- 12260.39



#landmask<-inund_annmax_stack[[1]]


inund_stack_lpx2017_dsd <- extend(inund_stack_lpx2017_dsd, extent(-180, 180, -90, 90))
inund_stack_lpx2017_dsd@extent@xmin <- -180
inund_stack_lpx2017_dsd@extent@xmax <- 180
inund_stack_lpx2017_dsd@extent@ymin <- -90
inund_stack_lpx2017_dsd@extent@ymax <- 90


inund_stack_lpx2017_dsd <- mask(inund_stack_lpx2017_dsd, landmask)



### Save outputs

saveRDS(inund_stack_lpx2017_dsd, './output/results/natwet/grid/inund_stack_lpx2017_dsd.rds')



##### test stuff out -----------------------------------------------------------

plot(inund_annmax_stack[[1]])
plot(inund_stack_lpx2017[[45]])
plot(inund_stack_lpx2017_d[[45]])
plot(inund_stack_lpx2017_ds[[45]])
plot(inund_stack_lpx2017_dsd[[45]])



hist(inund_annmax_stack[[1]])
hist(inund_stack_lpx2017[[45]])
hist(inund_stack_lpx2017_d[[45]])
hist(inund_stack_lpx2017_ds[[45]])
hist(inund_stack_lpx2017_dsd[[45]])






# there is a loss of area because of mismatch in grids...

sum_raster(inund_stack_lpx2017[[45]])
sum_raster(inund_stack_lpx2017_d[[45]])
sum_raster(inund_stack_lpx2017_ds[[45]])
sum_raster(inund_stack_lpx2017_dsd[[45]])






# inund_stack_lpx2017_d[is.na(inund_stack_lpx2017_d)] <- 0
# inund_f[is.na(inund_f)] <- 0

# 
# mask(inund_stack_lpx2017_d[[1]], area)
# 
# inund_stack_lpx2017_d[inund_stack_lpx2017_d==0] <- NA 
# 
# plot()

# 
# 
# 
# # set origin of hyde grids
# origin(lpx_natwet_Mkm2_stack_d) <- origin(orchidee_b_drd_p)
# ext <- extent(-180, 180, -90, 90)
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
