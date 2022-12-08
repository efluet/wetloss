# /----------------------------------------------------------------------------#
#/     HYDE database and aggregate it to the                            --------


# package of unzip 
library(utils)
library(raster)
library(ncdf4)


# Create function
# arguments:   X & Y resolution of wetland grid, start & end year 
save_resampled_hyde_data <- function(wet_res_x, wet_res_y, start_year, end_year){
  
  # some var values for testing
  wet_res_x=1
  wet_res_y=1
  start_year=-7000
  end_year=-2000
  
  
  # list .zip files in dir
  d <- "./data/lucc/hyde32_beta/zip"
  
  # get the full list of zip files; from all years
  file_ls <- list.files(path= d, pattern='_lu.zip', full.names=TRUE)
  
  
  # get list of HYDE years and convert to numeric ==============================
  year_ls <- sub(pattern=c('_lu.zip'), "", x=file_ls)
  
  # remove directory from path string 
  year_ls <- sub(pattern=d, "", x=year_ls)
  year_ls <- sub(pattern= "/", "", x=year_ls)
  bc_idx <- grep(pattern='BC', x=year_ls)   # get index of BC years
  year_ls <- as.numeric(sub(pattern='AD|BC', "", x=year_ls))
  year_ls[bc_idx] <- year_ls[bc_idx] * -1
  rm(bc_idx)
  
  
  # filter years within the range defined ===========================================
  sel_years <- (year_ls >= start_year & year_ls <= end_year)
  year_ls <- year_ls[sel_years]
  file_ls <- file_ls[sel_years]
  # remove objects
  rm(sel_years, start_year, end_year) 
  
  
  # use the first zip files to get list of lu var names ========================
  z <- file_ls[1]
  # u is a list of unzipped .asc files 
  u <- unzip(z, exdir='./output/temp') # unzip
  var_ls <- sub(pattern=c('./output/temp/'), "", x=u) # remove directory
  var_ls <- sub(pattern=c('AD.asc|BC.asc'), "", x=var_ls)
  var_ls <- sub(pattern=c('[0-9]+'), "", x=var_ls)
  var_ls <- c("cropland", "ir_rice")
  
  
  # set raster dimensions ======================================================
  londim <- ncdim_def("lon","degrees_east", seq(-180, 180, wet_res_x)) 
  latdim <- ncdim_def("lat","degrees_north", seq(-90,  90, wet_res_y)) 
  timedim <- ncdim_def("time", 'year', year_ls)
  nx = 360/ wet_res_x
  ny = 180/ wet_res_y
  
  
  
  # make function defining netcdf variable =====================================
  define_ncdf_var <- function(var_name, var_unit){
    fillvalue <- -999
    # define variables
    temp <- ncvar_def(var_name, var_unit, list(londim,latdim,timedim), 
                      fillvalue, var_name, prec="single")
    return(temp) }
  
  
  # create the netcdf variables ================================================
  # loop through HYDE variables
  for (v in var_ls){
    # give obj the name of the variable
    assign(v, define_ncdf_var(v, 'km^2'))}   
  # delete iterator
  rm(v) 
  
  
  # create netCDF file and put arrays ==========================================
  o <- "./output/results/hyde_resampled/"
  ncfname <- paste0(o, "hyde32_", wet_res_y,".nc")
  
  # write all variable names in a list, cause I spent ALL FUCKING DAY trying 
  # to convert these GODDAMN strings into a flat list of variable names, 
  
  var_ls_symbols <- list(cropland, ir_rice)
  # (cropland, grazing, pasture, rangeland, conv_rangeland,
  # rf_rice, ir_rice, rf_norice, ir_norice, tot_rice, tot_rainfed, tot_irri)
  
  # create NetCDF file
  ncnew <- nc_create(ncfname, var_ls_symbols)
  
  # print dimensions
  print(paste("The file has", ncnew$nvars,"variables and ", ncnew$ndim," dimensions"))
  
  
  
  # ADD THE DATA TO THE NETCDF =================================================
  # loop through the zip files (ie years)
  for (f in 1:length(file_ls)){  
    
    # unzip year zip
    #u <- unzip(file_ls[f], exdir='./temp')
    
    unzip_dir <- paste0('./output/results/temp/', strsplit(basename(file_ls[f]), ".zip")[[1]])
    u <- unzip(file_ls[f], exdir=unzip_dir)
    
    
    # loop through variables 
    for (v in 1:length(var_ls)){
      
      # print and counter
      print(paste(f, year_ls[f], var_ls[v], sep=" "))
      
      # read HYDE grid as raster
      r <- raster(grep(var_ls[v], u, value=TRUE))  # u[v]
      
      # aggregate grid to resolution of wetland data
      ra <- aggregate(r, fact= c(wet_res_x/0.0833333, wet_res_y/0.0833333), fun=sum) 
      
      # flip and transpose the matrix, otherwise the ncdf raster is inverted
      ra <- flip(ra, 'y')
      ma <- t(as.matrix(ra))
      
      # write out to ncdf 
      ncvar_put(ncnew, var_ls[v], ma, start=c(1,1,f), count=c(nx,ny,1))  
      
    }
    
    unlink(unzip_dir, recursive=T) # delete temp directory
    # delete the unzip object
    rm(u, r, ra, ma)
    
  }
  
  # close the file, writing data to disk
  nc_close(ncnew)
  
  rm(v, f, o, timedim, file_ls)
}



