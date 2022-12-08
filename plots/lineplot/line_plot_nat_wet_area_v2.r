
# read hyde ncdf ===============================================================
h <- './output/results/hyde_resampled/hyde32_2.5.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals) # get years

hyde_indx <- match(hyde_yrs, hyde$dim$time$vals)



# read stocker ncdf ============================================================
l <- './data/nat_wetland_map/trace21_129.cdf'
lpx <- nc_open(l)
# get years; and round to remove 5 as last digit
lpx_yrs <- sort(lpx$dim$TIME$vals)  -5
# subset the hyde years to those also found in LPX (e.g removes years after 1980)
hyde_yrs <- hyde_yrs[which((hyde_yrs  %in%  lpx_yrs) == TRUE)]
# get the index of hyde years in lpx data 
#lpx_indx <- which((lpx_yrs %in% hyde_yrs) == TRUE)
lpx_indx <- match(hyde_yrs, lpx_yrs)

# get grid area - originally in m^2; convert to km2
#area <- raster(l, varname="area") / 10^4


#===============================================================================

# open connection to netcdf file
t <- './data/nat_wetland_map/trace21_129.cdf'
nc <- nc_open(t)

# get time labels
year <-  c(unique(ncvar_get( nc, attributes(nc$dim)$names[4])))
year_sub <- seq(1, length(year), 1) # subsample the timeseries

# subset specific variables
nc_inund <- ncvar_get(nc, 'inund') #subset to inund var
gridarea <- ncvar_get(nc, 'area') #subset to area var
nc_luarea <- ncvar_get(nc, 'lu_area') #subset to area var


# create empty df for output
output_df <- data.frame(year=numeric(),
                        sum_inund_Mkm2=numeric(),
                        sum_peatland_Mkm2=numeric())


# loop through subset of years
for (y in hyde_yrs){
  
  # get peatland area
  peatland_f <- nc_luarea[ , ,2,y]  # 3rd dimension is lu type: k=2 peatlands
  peatland_a <- gridarea * peatland_f # get inundated area 
  sum_peatland_Mkm2 <- sum(peatland_a, na.rm = TRUE) * 10^-6 * 10^-6  # sum to Mkm2
  
  # get inund area
  inund_f <- apply(nc_inund[ , , , y], c(1,2), max) # get elementwise max value
  inund_a <- gridarea * inund_f # get inundated area 
  sum_inund_Mkm2 <- sum(inund_a, na.rm = TRUE)  * 10^-6 * 10^-6  # sum to Mkm2
  
  
  # append sums to df
  output_df <- rbind(output_df, 
                     data.frame(year = year[y],
                                sum_inund_Mkm2 = sum_inund_Mkm2,
                                sum_peatland_Mkm2= sum_peatland_Mkm2))
}

# convert output df to long format
output_df <- output_df %>%
              gather(type, value, sum_inund_Mkm2:sum_peatland_Mkm2) %>%
              mutate(type= ifelse(type=='sum_inund_Mkm2','Max monthly inund.', 'Peatland'))




# line plot wet area & peatland over 10k =======================================
ggplot(output_df) +
  geom_line(aes(x=year, y=value, color=type), size=0.3) +
  
  ylab('Global area (km2 10^6)') +
  scale_x_continuous(limits=c(-10000, 2000), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,12), expand=c(0,0)) +
  
  theme_bw() +    # declare theme elements
  line_plot_theme




# save figure to file
ggsave('../../output/figs/lpxdytop_global_inund_peatland_holocene.png',  
       width=87, height=70, dpi=600, units="mm", type = "cairo-png")
dev.off()


