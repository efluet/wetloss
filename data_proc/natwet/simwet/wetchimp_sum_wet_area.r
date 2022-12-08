# /------------------------------------------------------------------
#/    Potential wetland map
#     this is the climatically supportable wetland area.


# Ensemble of wetland area: 
# Orchidee (1.0deg)  Exp 2 - 1
# DLEM     (0.5deg)  Exp 2 - 1   
# SDGVM    (0.5deg)  Exp 2 - 1 
# Bern               Exp 2 - 1


# VAR is one of:
#   amax_weta = annual maximal wetland areal extent (m^2)
#   mmax_weta = monthly maximal wetland areal extent (m^2)
#   mch4_e = monthly emitted to atmosphere CH4 flux density (g CH4 / m^2 of gridcell)
#   ach4_e = annual emitted to atmosphere CH4 flux density (g CH4 / m^2 of gridcell)


# experiment 1: equilibrium run for 1901-1931
# experiment 2: transient simulation for 1932-2004
# data: 12 monthly values, are monthly averages for 1993-2004


# /----------------------------------------------------------------------------#
#/       Create empty df for output of wetland area                      -------
output_df <- data.frame(model      =character(),
                        experiment =numeric(),
                        extension  =character(),
                        wet_Mkm2   =numeric(),
                        year_start =numeric(),
                        year_end   =numeric(),
                        temp       =character())


# /----------------------------------------------------------------------------#
#/       Read grid of cell area at 0.5;  
#        This has the right global surface area ~510.066 Mkm2
#area_km2 <- raster("../../data/ease_area_grid/area_easewgs84_0p5deg_corrextent.tif")



# wetchimp common directory
d <- './data/nat_wetland_map/wetchimp/'
e1 <- 'exp1/amax_weta/'
e2 <- 'exp2/amax_weta/'
e3 <- 'exp3/amax_weta/'

# make list of  data
l <- c(paste0(d, e1, (list.files(path = paste0(d, e1), pattern = ".nc"))),
       paste0(d, e2, (list.files(path = paste0(d, e2), pattern = ".nc"))),
       paste0(d, e3, (list.files(path = paste0(d, e3), pattern = ".nc"))))

library("rlist")
l <- list.remove(l, c(8,27))

# /----------------------------------------------------------------------------#
#/     LOOP AND CALCULATE SUMs                                   ------------

for (i in l){
  
  name = tail(strsplit(i, "/")[[1]],n=1)
  name = substr(name, 1, nchar(name)-3)
  print(name)
  
  
  # reset 
  wasmonthly = ""
  extension = ""
  
  
  # read max raster
  wet <- raster(i, varname="amax_weta")
  NAvalue(wet) <- 0
  
  # sum global in million km2
  wet_Mkm2 <- sum_raster(wet / 10^6 / 10^6)
  # read as ncdf
  
  temp_nc <- nc_open(i)
  year_start <- 1901
  year_end <- 1932
  
  # if multiple month, get the maximum value across all months
  if (length(temp_nc$dim$time$vals) > 1){
    wet <- max(brick(i, varname="amax_weta"))
    wet_Mkm2 <- sum_raster(wet / 10^6 / 10^6)
    year_start <- 1993
    year_end <- 2004
    wasmonthly = "from monthly max"}
  
  output_df <- rbind(output_df, 
                     # get the filename at end of data directory
                     data.frame(model    = unlist(strsplit(name, "_"))[4],
                                experiment = substr(name, 11, 11),
                                extension  = unlist(strsplit(name, "_"))[5],
                                wet_Mkm2= wet_Mkm2,
                                year_start = year_start,
                                year_end = year_end,
                                temp = wasmonthly))
  
}

output_df <-  output_df %>%
              mutate(extension = as.character(extension)) %>%
              mutate(extension=ifelse(is.na(extension), "wet", extension))


# Save output
write.csv(output_df, "./output/results/natwet/sum_wetchimp_grids.csv")
