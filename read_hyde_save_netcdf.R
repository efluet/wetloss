### read HYDE database

library(utils) # package of unzip 
library(raster)
library(ncdf4)

# set working dir
setwd("C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/data/lucc/hyde32_beta/zip")


# list .zip files in dir
file_ls <- list.files(pattern='_lu.zip', full.names=TRUE)


# get list of HYDE years  ======================================================
year_ls <- sub(pattern=c('_lu.zip'), "", x=file_ls)
year_ls <- sub(pattern='./', "", x=year_ls)
bc_idx <- grep(pattern='BC', x=year_ls)
year_ls <- sub(pattern='AD|BC', "", x=year_ls)
year_ls <- as.numeric(year_ls)
year_ls[bc_idx] <- year_ls[bc_idx] * -1


# get list of lu var names =====================================================
f <- file_ls[1]
u <- unzip(f, exdir='./temp') # unzip
var_ls <- sub(pattern=c('./temp/'), "", x=u)
var_ls <- sub(pattern=c('AD.asc|BC.asc'), "", x=var_ls)
var_ls <- sub(pattern=c('[0-9]+'), "", x=var_ls)


# set dimensions ===============================================================
londim <- ncdim_def("lon","degrees_east", seq(-179, 180, 3.75)) 
latdim <- ncdim_def("lat","degrees_north", seq(-89, 90, 2.5)) 
timedim <- ncdim_def("time", 'year', year_ls)
# from stocker: -178.125, 181.875, -56.25, 83.75

nx = 360/ 3.75
ny = 180/ 2.5


# make function defining netcdf variable
define_ncdf_var <- function(var_name, var_unit){
  fillvalue <- -999
  # define variables
  temp <- ncvar_def(var_name, var_unit, list(londim,latdim,timedim), 
                    fillvalue, var_name, prec="single")
  #assign(var_name, t)
  return(temp)
}



# create the netcdf variables ==================================================
for (v in var_ls){  # loop through variables
  # give obj the name of the variable
  assign(v, define_ncdf_var(v, 'km^2'))
}

rm(v) # delete iterator


# create netCDF file and put arrays ============================================
ncfname <- "hyde32.nc"

# write all variable names in a list, cause I spent ALL FUCKING DAY trying 
# to convert these GODDAMN strings into a flat list of variable names, 
# but looking online, it seems like I'm the only FUCKING one that is interested in that
var_ls_symbols <- list(cropland, grazing, pasture, rangeland, conv_rangeland, 
                       rf_rice, ir_rice, rf_norice, ir_norice, tot_rice, tot_rainfed, tot_irri)


ncnew <- nc_create(ncfname, var_ls_symbols)

print(paste("The file has", ncnew$nvars,"variables"))
print(paste("The file has", ncnew$ndim,"dimensions"))



# loop through the zip files (ie years)
for (f in 1:length(file_ls[1:4])){  
  
  u <- unzip(file_ls[f], exdir='./temp') # unzip year zip
  
  for (v in 1:length(u[1:4])){  # loop through variables
    
    print(paste(f, file_ls[f], u[v], var_ls[v], sep="   ")) # print and counter
    
    r <- raster(u[v]) # conver to raster
    
    # aggregate grid to resolution of wetland data
    ra <- aggregate(r, fact= c(3.75/0.0833333, 2.5/0.0833333), fun=sum)   
    # flip and transpose the matrix, otherwise the ncdf raster is inverted
    ra <- flip(ra, 'y')
    ma <- t(as.matrix(ra))
    
    ncvar_put(ncnew, var_ls[v], ma, start=c(1,1,f), count=c(nx,ny,1))  
    
  }
  rm(u) # delete the unzip object
  unlink('./temp', recursive=T) # delete temp directory
}

# close the file, writing data to disk
nc_close(ncnew)





# read stocker ncdf ============================================================
h <- "hyde32.nc"
hyde <- nc_open(h)

#crs <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '
lu <- raster(h, varname="cropland", band = 3)#, crs=crs)#, level=1)
plot(lu)


# e <- list(cropland, grazing, pasture, rangeland, conv_rangeland, 
#           rf_rice, ir_rice, rf_norice, ir_norice, tot_rice,
#           tot_rainfed, tot_irri)
# 
# # create netCDF file and put arrays
# ncout <- nc_create(ncfname, e, force_v4=T)
#
# e<-list(cropland, pasture)
# 
# library(memisc)
# a<- as.symbols(var_ls)
# 
# library(rlang)
# y <- mapply(var_ls, FUN=as.symbol)