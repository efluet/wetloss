# Description: reads in ORCHIDEE


# Read ORCHIDEE data =========================================================
# wetchimp data originally expressed in m^2


# function that reads in wetchimp data
# no data is expressed as  -9999
# area converted to Mkm2
read_wetchimp_raster <- function(input_dir, var_name){
  if(length(nc_open(input_dir)$dim$time$vals)>1){
    t=max(brick(input_dir, varname= var_name)) /10^6
  } else{ t     = raster(input_dir) /10^6 }
  t@file@name <- basename(input_dir)
  return(t)}

# create empty stack
orchidee_nat_wet_timeline <- stack()

# read orchidee 1922-1931 --------------------------------------------------------------

# Orchidee_alt*= model results for all WTDs from internal TOPMODEL approach for inter- and intra-annual wetland extents.
# Orchidee_altsat= model results for only saturated areas from internal TOPMODEL approach for inter- and intra-annual wetland extents.
# Orchidee= model results for all WTDs with prescribed Papa et al. 2010 inter- and intra-annual wetland extents.
# Orchidee_sat= model results for only saturated areas from prescribed Papa et al. 2010 inter- and intra-annual wetland extents.

# read ORCHIDEE 1901-1931 --------------------------------------------------------------------
# all have single layer
#m1 <- './data/nat_wetland_map/wetchimp/exp1/amax_weta/amax_weta_1_Orchidee.nc'       # 11.65 Mkm2
#m1 <- './data/nat_wetland_map/wetchimp/exp1/amax_weta/amax_weta_1_Orchidee_sat.nc'    # 7.41 Mkm2
m1 <- './data/nat_wetland_map/wetchimp/exp1/amax_weta/amax_weta_1_Orchidee_alt.nc'    # 9.19 Mkm2
#m1 <- './data/nat_wetland_map/wetchimp/exp1/amax_weta/amax_weta_1_Orchidee_altsat.nc' # 5.87 Mkm2


# read ORCHIDEE 1993-2004 --------------------------------------------------------------------
#m1 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1922_1931_saisTOP_1.nc'      # 5.77 Mkm2
#m1 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1922_1931_saisTOP_1_MEAN.nc' # 5.77 Mkm2
#m1 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1922_1931_saisP07_1.nc'      # 7.43 Mkm2
#m1 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1922_1931_saisP07_1_MEAN.nc' # 7.43 Mkm2

orchidee1 <-brick(m1, varname="amax_weta")
sum_raster(orchidee1[[1]])/10^6 / 10^6

names(orchidee1) <- "year1910"
orchidee_nat_wet_timeline <- stack(orchidee_nat_wet_timeline, orchidee1)

names(orchidee1) <- "year1920"
orchidee_nat_wet_timeline <- stack(orchidee_nat_wet_timeline, orchidee1)

names(orchidee1) <- "year1930"
orchidee_nat_wet_timeline <- stack(orchidee_nat_wet_timeline, orchidee1)



# monthly orchidee 1932-1992 --------------------------------------------------------------
# 732 layers   - these two are different
m2 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/monthly_wetland_CH4_1932_1992_saisTOP.nc'
#m2 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/monthly_wetland_CH4_1932_1992_saisP07.nc'

orchidee2 <-brick(m2, varname="mmax_weta_all")



# calculate yearly max for each hyde year in series
years<- c(1940, 1950, 1960, 1970, 1980, 1990)
year_dif <- c(0, 10, 20, 30, 40, 50)

for (i in seq(1, length(year_dif))){
  
  #start in 1940 
  sel_years <- (8 * 12) + year_dif[i] + seq(0,11)
 
  temp_orchidee2_monthly <- orchidee2[[sel_years]]
  temp_orchidee_amax <- max(temp_orchidee2_monthly)
  
  
  names(temp_orchidee_amax) <- paste0("year",years[i])
  orchidee_nat_wet_timeline <- stack(orchidee_nat_wet_timeline, temp_orchidee_amax)  
  }




# monthly orchidee 1993-2004 --------------------------------------------------------------
# 12 layers in total

### EXP 2 
#m3 <- './data/nat_wetland_map/wetchimp/exp2/amax_weta/amax_weta_2_Orchidee.nc'                     # n=12, sumofmax= 17.13 Mkm2
#m3 <- './data/nat_wetland_map/wetchimp/exp2/amax_weta/amax_weta_2_Orchidee_alt.nc'                # n=12, sumofmax= 12.87 Mkm2

### EXP 3 
#m3 <- './data/nat_wetland_map/wetchimp/exp3/amax_weta/amax_weta_3_Orchidee.nc'                    # n=12, sumofmax= 10.41 Mkm2
m3 <- './data/nat_wetland_map/wetchimp/exp3/amax_weta/amax_weta_3_Orchidee_alt.nc'                 # n=12, sumofmax= 10.41 Mkm2
#m3 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1993_2004_saisTOP_2.nc' # n=12, sumofmax=  8.57 Mkm2
#m3 <- './data/nat_wetland_map/wetchimp/orchidee_yearly/yearly_wetland_CH4_1993_2004_saisP07_2.nc' # n=12, sumofmax= 11.29 Mkm2

orchidee3 <-brick(m3, varname="amax_weta")
orchidee3 <- max(orchidee3)
sum_raster(orchidee3)/10^6 / 10^6

# calc yearly max  for 1993-2004
names(orchidee3) <- "year2000"
orchidee3<-raster::aggregate(orchidee3, fact=2, fun="sum")
orchidee_nat_wet_timeline <- stack(orchidee_nat_wet_timeline, orchidee3)


# orchidee_nat_wet_timeline <- orchidee_nat_wet_timeline /10^6 / 10^6



#==============================================================================#
# sum wetland area at each step ------------------------------------------------
#==============================================================================#

# make empty output df
sumarea_df<- NULL # c(year= numeric(), tot_natwet_Mkm2 = numeric())


# for each layer in the stack
for (i in seq(1, dim(orchidee_nat_wet_timeline)[3])){
  
  # year list
  years <- seq(1910, 2000, 10)
  
  # sum to Mkm^2
  sum <- sum_raster(orchidee_nat_wet_timeline[[i]])/10^6 / 10^6
  
  # attach the sum and year to output df
  sumarea_df <- bind_rows(sumarea_df, data.frame(years[i], sum, "ORCHIDEE"))
}

# rename the columns
names(sumarea_df) <- c("year", "tot_natwet_Mkm2", "model")





#==============================================================================#
# get HYDE years & indexes from ncdf -------------------------------------------
#==============================================================================#

h <- './output/results/hyde_resampled/hyde32_2.5.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals) # get years
hyde_indx <- match(hyde_yrs, hyde$dim$time$vals)


#==============================================================================#
# read stocker ncdf  -----------------------------------------------------------
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
### sum LPX nat wet area -------------------------------------------------------
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
  
  natwet <- max(brick(l, varname="inund", level=lpx_indx[t], lvar=4))
  natwet <- natwet * area
  #natwet <- raster(natwet)
  names(natwet) <- paste0("year",yr)

  lpx_natwet_Mkm2_stack <- stack(lpx_natwet_Mkm2_stack, natwet)
  
  
  # sum to Mkm^2
  sum <- sum_raster(natwet) # lpx_natwet_Mkm2_stack[[i]])
  
  # attach the sum and year to output df
  sumarea_df <- bind_rows(sumarea_df, 
                          data.frame(year = hyde_yrs[t], 
                                     tot_natwet_Mkm2= sum, 
                                     model="LPX-DYTOP wetland"))
  }

# rename the columns
names(sumarea_df) <- c("year", "tot_natwet_Mkm2", "model")



#==============================================================================#
# get peatland grid from LPX  ==================================================
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
  
  # sum to Mkm^2
  sum <- sum_raster(p) # lpx_natwet_Mkm2_stack[[i]])
  
  # attach the sum and year to output df
  sumarea_df <- bind_rows(sumarea_df, 
                          data.frame(year = hyde_yrs[t], 
                                     tot_natwet_Mkm2= sum, 
                                     model="LPX-DYTOP peatland"))

  }

# rename the columns
names(sumarea_df) <- c("year", "tot_natwet_Mkm2", "model")



saveRDS(sumarea_df, "./output/results/natwet/natwet_orchidee.rds")


#==============================================================================#
# plot potential wetland area timeline: ORCHIDEE & LPX-DYTOP -------------------
#==============================================================================#

ggplot(sumarea_df) +
  
  # nat wet  line
  geom_line(aes(x=year, y= tot_natwet_Mkm2, color=model), size=0.4) +
  #geom_point(aes(x=year, y= tot_natwet_Mkm2, color=model), size=0.9) +
  
  # axis lables
  xlab("Year") +  
  ylab("Potential wetland area (Mkm2)") +
  
  # # axis scales
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2000))+ #, breaks=seq(1900, 2000, 10)) +
  # scale_y_continuous(expand=c(0,0), limits=c(7, 11)) + #, breaks=c(20, 40, 60, 80, 100, 120)) +
  
  line_plot_theme +
  theme(legend.position = c(0.05, 0.3),
        plot.margin = margin(2,3,2,3,"mm"))



### save plot ------------------------------------------------------------------
ggsave("./output/figures/nat_wet_line_plot_orchidee_lpx.pdf",
       width=178, height=80, dpi=600, units='mm')

ggsave("./output/figures/nat_wet_line_plot_orchidee_lpx.png",
       width=178, height=80, dpi=600, units='mm', type = "cairo-png")

dev.off()





### split the ORCHIDEE GRID into 0.25 deg  -------------------------------------

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

# reaggregate the grid to orchidee to 3.75 x 2.5, calculating the sum or area
orchidee_b_dr <- aggregate(orchidee_b_d, fact=c(15,10), na.rm=TRUE, fun="sum")
sum_raster(orchidee_b_dr)

# disaggregate the sums back to 0.25 res
orchidee_b_drd <- disaggregate(orchidee_b_dr, fact=c(15,10), na.rm=TRUE)
sum_raster(orchidee_b_drd)

# calculate percentage of grid
orchidee_b_drd_p = orchidee_b_d / orchidee_b_drd 
orchidee_f <- aggregate(orchidee_b_drd_p, fact=c(15,10), na.rm=TRUE, fun="sum")


# disaggregate the LPX ---------------------------------------------------------

# dissage LPX
lpx_natwet_Mkm2_stack_d <- disaggregate(lpx_natwet_Mkm2_stack, fact=c(15, 10), method="")



# set origin of hyde grids
origin(lpx_natwet_Mkm2_stack_d) <- origin(orchidee_b_drd_p)
ext <- extent(-180, 180, -90, 90)
lpx_natwet_Mkm2_stack_d <- extend(lpx_natwet_Mkm2_stack_d, ext)

# multiply by the subgrid %
lpx_natwet_Mkm2_stack_d <- lpx_natwet_Mkm2_stack_d[[1]] * orchidee_b_drd_p

# reaggregate to 1x1deg
lpx_natwet_Mkm2_stack_d1deg <- aggregate(lpx_natwet_Mkm2_stack_d, fact=4, na.rm=TRUE, fun="sum")


#  WHY IS THERE A 20% AREA GAIN...?
sum_raster(lpx_natwet_Mkm2_stack_d1deg[[1]])
sum_raster(lpx_natwet_Mkm2_stack_d[[1]])
sum_raster(lpx_natwet_Mkm2_stack[[1]])

