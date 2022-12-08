# /----------------------------------------------------------------------------#
#/   This is the script to copy in every R member 


#  SET WD
setwd(here())
source('./data_proc/prep_init.r') # Prep functions, themes, etc.
options(row.names=FALSE, scipen = 999)
# library(fasterize)
# library(raster)
# library(dplyr)
# library(countrycode)
# library(fasterize)
# library(sf)



#####################################
cs_peat=0   # Include peatland histcases in comparison or not

# Run config
test_theta = 0       # use test theta values (of 3, 0.5, 5)
null_theta = 0       # use null theta values (of 0,0,0)
test_potwet = 0      # use testing potwet (combination of preswet #1 and simwet #4) ?dbl check numbers
update_potwet = 0    # whether to subtract drained area from potwet; !!! turning this off reduces need for expanding allowable & filling overlap
save_all_output = 0  # Save all outputs for fig 2 & 3

# Drain_distrib config
fill_val = 10e-4    # Fill value for perc_overlap; influences balance between real LUoverlap and filled
scale_allowable = 1
expand_perc_overlap = 1  # Expand perc_overlap to every...?
nb_repeats = 4      # Number of redistribution loops

#######################################
# Args: s_i, p_i, niteration
s_i = 4
p_i = 1
niteration = 3
######################################



#  1
#   Initialize inputs for fitting (run once at the start)
source('./data_proc/overlay/initialize_prefitting.r')


#  2
#    Get function that takes in theta parameters, 
#    does the drain distrib, compares to histcases, then outputs residuals
#    This is the function that gets optimized
source('./data_proc/fit/fcn_runmodel_getresiduals_2021.r', local=T) 


#  3
#    Wrapper functions that sets the p_i & s_i  parameters
source('./data_proc/fit/fcn_run_mcmc.r', local=T)  
source('./data_proc/fit/fcn_run_modfit.r', local=T)  


#  4
#    Run the fitting
fit <- run_modfit(s_i, p_i, niteration)
# fit <- run_mcmc(s_i, p_i, niteration)

#  5
#   Save output
out_f <- paste0('../output/results/fit/modfit_s', s_i, '_p', p_i, '_i', niteration, '.rds')
saveRDS(fit, out_f)

