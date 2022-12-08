
# Prepares a NCDF 
#   - LU from HYDE3.2
#   - POP from HYDE3.2
#   - Forestry from LUHv2

library(utils) # package of unzip 
library(raster)
library(ncdf4)


# create function
# arguments:   X & Y resolution of wetland grid, start & end year 
save_resampled_hyde_data <- function(ncfname, wet_res_x, wet_res_y, start_year, end_year){
  
  # some var values for testing
  # wet_res_x=1
  # wet_res_y=1
  # start_year=1700
  # end_year=1750
  
  
  # /--------------------------------------------------------------------------#  
  #/  GET YEARS                                              -------
  
  # list .zip files in dir
  d <- "./data/lucc/hyde32_beta/zip"
  
  # get the full list of zip files; from all years
  file_ls_lu <- list.files(path= d, pattern='_lu.zip', full.names=TRUE)
  
  # Convert list to numeric 
  year_ls <- sub(pattern=c('_lu.zip'), "", x=file_ls_lu) 
  
  # remove directory from path string 
  year_ls <- sub(pattern=d, "", x=year_ls)
  year_ls <- sub(pattern= "/", "", x=year_ls)
  
  bc_idx <- grep(pattern='BC', x=year_ls)   # get index of BC years
  year_ls <- as.numeric(sub(pattern='AD|BC', "", x=year_ls))
  year_ls[bc_idx] <- year_ls[bc_idx] * -1
  #rm(bc_idx)
  
  #  Filter years within the range defined
  sel_years <- (year_ls >= start_year & year_ls <= end_year)
  year_ls <- year_ls[sel_years]
  file_ls_lu <- file_ls_lu[sel_years]
  
  
  # /--------------------------------------------------------------------------#  
  #/  GET CROP + PASTURE LU                                              -------
  
  # Subset the list of variables listed from filenames 
  # var_ls_lu <- c("cropland", "grazing", "pasture", "rangeland", "conv_rangeland", 
  #             "rf_rice", "ir_rice", "rf_norice", "ir_norice", "tot_rice", "tot_rainfed", "tot_irri")
  var_ls_lu <- c("cropland", "grazing", "pasture", "ir_rice") #"rangeland", "rf_rice", "ir_rice", "tot_rainfed", "tot_irri")
  
  
  # /--------------------------------------------------------------------------#  
  #/     GET POP LU                                                    -------
  
    # get the full list of zip files; from all years
  file_ls_pop <- list.files(path= d, pattern='_pop.zip', full.names=TRUE)
  
  file_ls_pop <- file_ls_pop[sel_years]
  
  #  Get list of pop var names 
  # var_ls_pop <- c("popc_", "popd_", "urbc_", "rurc_", "uopp_")
  var_ls_pop <- c("uopp_")

  
  # /--------------------------------------------------------------------------#
  #/  Set output raster dimensions                                         -----
  londim  <- ncdim_def("Longitude","Degrees East",  seq(-179.5, 179.5, wet_res_x)) 
  latdim  <- ncdim_def("Latitude" ,"Degrees North", seq(-89.5,  89.5,  wet_res_y)) 
  timedim <- ncdim_def("Time", 'Year', year_ls)
  nx = 360/ wet_res_x
  ny = 180/ wet_res_y
  var_unit = "km^2"
  
  # /--------------------------------------------------------------------------#
  #/ make function defining netcdf variable  
  
  define_ncdf_var <- function(var_name, var_unit){
                              fillvalue <- -999
                              # define variables
                              temp <- ncvar_def(var_name, 
                                                var_unit, 
                                                list(londim, latdim, timedim), 
                                                fillvalue, 
                                                var_name, 
                                                prec="single")
                              return(temp) }
  
  
  # /--------------------------------------------------------------------------#
  #/    Create the netcdf variable for each HYDE vars                       ----
  
  var_ls_symbols <-  purrr::map(as.list(c(var_ls_lu, var_ls_pop)), ~define_ncdf_var(., var_unit))
  
  
  # /--------------------------------------------------------------------------#
  #/ create netCDF file and put arrays                                     -----

  o <- "./output/results/hyde_resampled/"
  #ncfname <- paste0("hyde32_", wet_res_y,"_allvars.nc")
  ncfname <- paste0(o, ncfname)
  outncdf <- nc_create(ncfname, var_ls_symbols)  # create NetCDF file
  
  # print dimensions
  print(paste("NCDF has", outncdf$nvars,"variables and ", outncdf$ndim," dimensions."))
  
  
  # /--------------------------------------------------------------------------#
  #/ ADD DATA TO NETCDF FILE                                              ------
  
  # loop through years
  for (f in 1:length(year_ls)){  
    
    
    # /------------------------------------------------------------------------#
    #/  LU VARIABLES                                                      ------
    
    # Unzip file for specific year
    tdir='./output/results/temp/'
    unzip_dir <- paste0(tdir, strsplit(basename(file_ls_lu[f]), ".zip")[[1]])
    lu_unzipped <- unzip(file_ls_lu[f], exdir=unzip_dir)
    
    
    # Loop through LU variables 
    for (v in 1:length(var_ls_lu)){
      
      # print and counter
      print(paste("Loop:", f, " - Year:", year_ls[f], "  Var:", var_ls_lu[v], sep=" "))
      
      # Read HYDE grid as raster
      # The [1] is to get first; resolve when rangeland returns two grids.
      r <- raster(grep(var_ls_lu[v], lu_unzipped, value=TRUE)[1],
                  xmn=-180, xmx=180, ymn=-90, ymx=90, 
                  crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      r <- extend(r, extent(-180, 180, -90, 90))
      origin(r) <- c(0, 0) #origin(wet)
      
      
      # aggregate grid to resolution of wetland data
      ra <- aggregate(r, fact= c(wet_res_x/0.0833333, wet_res_y/0.0833333), fun=sum) 
      
      # flip and transpose the matrix, otherwise the ncdf raster is inverted
      #ra <- flip(ra, 'y')
      #ma <- t(as.matrix(ra))
      ra <- values(flip(ra, 'y'))
      
      # write grid to ncdf 
      ncvar_put(outncdf, var_ls_lu[v], ra, start=c(1,1,f), count=c(nx,ny,1))  
      
    }
    unlink(unzip_dir, recursive=T) # delete temp directory
    
    
    # /------------------------------------------------------------------------#
    #/  POP VARIABLES                                                     ------
    
    # Unzip file for specific year
    tdir='./output/results/temp/'
    unzip_dir <- paste0(tdir, strsplit(basename(file_ls_pop[f]), ".zip")[[1]])
    pop_unzipped <- unzip(file_ls_pop[f], exdir=unzip_dir)
    
    
    # Loop through POP variables 
    for (v in 1:length(var_ls_pop)){
      
      # print and counter
      print(paste("Loop:", f, " - Year:", year_ls[f], "  Var:", var_ls_pop[v], sep=" "))
      
      # Read HYDE grid as raster
      r <- raster(grep(var_ls_pop[v], pop_unzipped, value=TRUE))
      
      # aggregate grid to resolution of wetland data
      ra <- aggregate(r, fact= c(wet_res_x/0.0833333, wet_res_y/0.0833333), fun=sum) 
      
      # flip and transpose the matrix, otherwise the ncdf raster is inverted
      #ra <- flip(ra, 'y')
      #ma <- t(as.matrix(ra))
      ra <- values(flip(ra, 'y'))
      
      # write variable to ncdf 
      ncvar_put(outncdf, var_ls_pop[v], ra, start=c(1,1,f), count=c(nx,ny,1))  
      
    }
    unlink(unzip_dir, recursive=T) # delete temp directory
    rm(pop_unzipped, lu_unzipped, r, ra, ma) # delete the unzip object
    
  }
  
  
  # Add some global attributes
  # ncatt_put(ncout, 0, "Title", "MultiDimesionsalNCDFTest")
  # ncatt_put(ncout, 0, "Source", "Some example data from the raster package")
  # ncatt_put(ncout, 0, "References", "See the raster package")
  # ncatt_put(ncout, 0, "Created on", date())
  nc_close(outncdf)   # close the file, writing data to disk
  
  
  # remove objects
  #rm(v, f, o, timedim, file_ls_lu, file_ls_pop)  # delete objects
  # rm(sel_years, start_year, end_year) 
  
}



