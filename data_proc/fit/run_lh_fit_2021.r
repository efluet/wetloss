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
# library(sf)




#####################################
cs_peat=0   # Include peatland histcases in comparison or not

# Run config
test_theta = 0       # use test theta values (of 3, 0.5, 5)
null_theta = 0       # use null theta values (of 0,0,0)
test_potwet = 0      # use testing potwet (combination of preswet #1 and simwet #4) ?dbl check numbers
preswet_max = 1      # Use larger preswet area instead of mamax preswet
update_potwet = 0    # whether to subtract drained area from potwet; !!! turning this off reduces need for expanding allowable & filling overlap
save_all_output = 0  # Save all outputs for fig 2 & 3

# Drain_distrib config
fill_val = 10e-4    # Fill value for perc_overlap; influences balance between real LUoverlap and filled
scale_allowable = 1   # Scale up allowable, for forestry and peat during redistribution loop
expand_perc_overlap = 1  # Expand perc_overlap to every gridcell after first distribution
nb_repeats = 20      # Number of redistribution loops  


#######################################
# Args: s_i, p_i, niteration
s_i = 4
p_i = 1

niteration = 3 #1000
par_range_min = 10^-5
par_range_max = 30
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
# Latin Hypercube sampling generates more efficient estimates of desired parameters than simple Monte Carlo sampling.
set.seed(1234)
pars <- data.frame(improvedLHS(niteration, 3, 3)  * par_range_max + par_range_min)
names(pars) <- c('theta_rice', 'theta_pasture', 'theta_urban')


#  4
#    Run the fitting
outdf <- data.frame()

for (p in 1:nrow(pars)){
  
  temp_pars <- c(pars[p,])
  temp_resid <- make_wetloss_df(temp_pars, s_i, p_i)
  # Calculate 2 sets of residuals
  sum_resid <- sum(temp_resid, na.rm=T)
  sum_sq_resid <- sum(temp_resid^2, na.rm=T)
  
  # Append
  outdf <- bind_rows(outdf, data.frame(temp_pars, sum_resid, sum_sq_resid))
  
  # Every 100 runs, save the output
  if(p == round(p, -1)) {
    out_filename <- paste0('../output/results/fit/lh_pars_resid_s', s_i, '_p', p_i, '_i', niteration, '.csv')
    write.csv(outdf, out_filename)  }
  }

out_filename <- paste0('../output/results/fit/lh_pars_resid_s', s_i, '_p', p_i, '_i', niteration, '.csv')
write.csv(outdf, out_filename)