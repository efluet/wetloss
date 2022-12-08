library(raster)
library(ncdf4)

# Get reprojected GIEMS2
s_out <- stack('../output/results/natwet/preswet/giems2_aw_v3.tif')

# output filename
nc_filename <- paste0('../output/results/natwet/preswet/giems2_wgs84_v2.nc')

# s_out <- data.frame(SpatialPoints(giems2[[1]]))
# glimpse(s_out)

# Longitude and Latitude data
xvals <- unique(values(init(s_out, "x")))
yvals <- unique(values(init(s_out, "y")))
nx <- length(xvals)
ny <- length(yvals)
lon <- ncdim_def("longitude", "degrees_east", xvals)
lat <- ncdim_def("latitude", "degrees_north", yvals)

# Missing value to use
mv <- -999

# Time component
time <- ncdim_def(name = "Time", 
                  units = "Months since 01/01/1992", 
                  vals = 1:288, 
                  unlim = TRUE,
                  longname = "Months since 01/01/1992")

# Define flux variables
var_aw <- ncvar_def(name = "Wetland area",
                      units = "km^2",
                      dim = list(lon, lat, time),
                      longname = "Wetland area in each 0.25deg grid cell (km^2)",
                      missval = mv,
                      compression = 9)

# Add the variables to the file
ncout <- nc_create(nc_filename, list(var_aw))
print(paste("The file has", ncout$nvars,"variables"))
print(paste("The file has", ncout$ndim,"dimensions"))

# add some global attributes
ncatt_put(ncout, 0, "Title", "Global Inundation Estimate from Multiple Satellites - Version 2 (GIEMS-2 product)")
ncatt_put(ncout, 0, "Author", "Catherine Prignet, Carlos Jimenez, Philippe Bousquet")
ncatt_put(ncout, 0, "Comment", "Reprojected equal area grid of 0.25°x0.25° at the equator to WGS84 by Etienne Fluet-Chouinard (12/2020)")
ncatt_put(ncout, 0, "Version", "Version 2.0")
ncatt_put(ncout, 0, "Reference", "Satellite-derived global surface water extent and 2 dynamics over the last 25 years (GIEMS-2); Article DOI: 10.1029/2019JD030711")
ncatt_put(ncout, 0, "Created on", date())

# Place the precip and tmax values in the file 
#need to loop through the layers to get them 
# to match to correct time index
for (i in 1:nlayers(s_out)) { 
  print(i)
  #message("Processing layer ", i, " of ", nlayers(prec))
  ncvar_put(nc = ncout, 
            varid = var_aw, 
            vals = values(s_out[[i]]), 
            start = c(1, 1, i), 
            count = c(-1, -1, 1))  
  }

# Close the netcdf file when finished adding variables
nc_close(ncout)


# Reopen file to check
f1 <- '../output/results/natwet/preswet/giems2_wgs84.nc'
f2 <- '../output/results/natwet/preswet/giems2_wgs84_v2.nc'
# a1 <- nc_open(f)
a1 <- stack(f1)[[7]]
names(a1) <- 'first wrong WGS84 reproject'

a2 <- stack(f2)[[7]]
names(a2) <- 'updated correct WGS84 reproject'

# plot(a1)
# plot(a2)
a <- stack(a1, a2)

plot(a)

png('../output/figures/giems2_reproject_map.png')
plot(a, width = 800, height = 300)
dev.off()
