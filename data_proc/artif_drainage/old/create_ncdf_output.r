
# variable names as string
# Use same names as from overlay grids
var_ls_str_names =  c('ir_rice', 'wetcult', 'cropland', 'uopp_', 'pasture', 'peatland', 'forest_harv')

# variable names as sting
# var_ls_str_names = c('cropland_drain_distrib',
#                      'wetcult_drain_distrib',
#                      'forest_drain_distrib',
#                      'peatland_drain_distrib',
#                      'rice_rdm',
#                      'pasture_rdm',
#                      'urban_rdm',
#                      'tot_wetloss')  # added for sum

# not used yet
# var_ls_str_longnames = c('Cropland area drained (from stats)',
#                      'wetcult_drain_distrib',
#                      'forest_drain_distrib',
#                      'peatland_drain_distrib',
#                      'rice_rdm',
#                      'pasture_rdm',
#                      'urban_rdm',
#                      'tot_wetloss')  # added for sum
# 
# 
# ncdf_df = data.frame(varnames = var_ls_str_names,
#                      varlongnams = var_ls_str_longnames) # added for sum



# /----------------------------------------------------------------------------#
#/    Make function defining netcdf variable                              ------

wet_res_x = 0.5
wet_res_y = 0.5
londim  <- ncdim_def('Longitude','Degrees East',  seq(-180+wet_res_x/2, 180-wet_res_x/2, wet_res_x)) 
latdim  <- ncdim_def('Latitude' ,'Degrees North', seq(-90+wet_res_y/2,  90-wet_res_y/2,  wet_res_y)) 
timedim <- ncdim_def('Time', 'Year', years)
nx = 360/ wet_res_x
ny = 180/ wet_res_y
var_unit = 'km^2'
fillvalue <- -999

# /----------------------------------------------------------------------------#
#/  Set output ncdf dimensions    
define_ncdf_var <- function(var_name, var_unit){

  
  # define variables
  temp <- ncvar_def(var_name, 
                    var_unit, 
                    list(londim, latdim, timedim), 
                    fillvalue, 
                    var_name, 
                    prec='single')
  return(temp) }


# /----------------------------------------------------------------------------#
#/    Create the netcdf variable for each HYDE vars
#     converts them from string to symbol for the NetCDF
var_ls_wetloss_symbols <-  purrr::map(as.list(var_ls_str_names), ~define_ncdf_var(., var_unit))


o <- '../output/results/wetloss/grid/'
ncfname <- paste0(o, 'wetloss_drain_distrib_05deg.nc')
outncdf <- nc_create(ncfname, var_ls_wetloss_symbols)  # create NetCDF file

# print dimensions
print(paste('NCDF has', outncdf$nvars,'variables and ', outncdf$ndim,' dimensions.'))



ncatt_put(outncdf,0, 'title', 'Global wetland loss estimate')
ncatt_put(outncdf,0,'institution', 'Stanford University / University of Wisconsin-Madison')
ncatt_put(outncdf,0,'source', 'Etienne Fluet-Chouinard')
# ncatt_put(ncout,0,'references',references$value)
# history <- paste('P.J. Bartlein', date(), sep=', ')
# ncatt_put(ncout,0,'history',history)
# ncatt_put(ncout,0,'Conventions',Conventions$value)
