# Description: This script uses output of MCMC, and produces final outputs
#              using fitted parameters.
#              It runs the same process as tht in the MCMC, but less optimized, and 
#              this version saves outputs to disc (not kept in memory). 

# /----------------------------------------------------------------------------#
#/    config
cs_peat=0   # Include peatland histcases in comparison or not

# Run config
test_theta = 0       # use test theta values (of 3, 0.5, 5)
null_theta = 0       # use null theta values (of 0,0,0)
test_potwet = 0      # use testing potwet (combination of preswet #1 and simwet #4) ?dbl check numbers
preswet_max = 1      # Use larger preswet area instead of mamax preswet
update_potwet = 0    # whether to subtract drained area from potwet; !!! turning this off reduces need for expanding allowable & filling overlap
save_all_output = 1  # Save all outputs for figures 2 & 3

# Drain_distrib config
fill_val = 10e-4    # Fill value for perc_overlap; influences balance between real LUoverlap and filled
scale_allowable = 1 # JULY-1ST; WAS 1;    # Scale up allowable, for forestry and peat during redistribution loop
expand_perc_overlap = 1  # Expand perc_overlap to every pixel after exhausted
nb_repeats = 25      # Number of redistribution loops  


# /----------------------------------------------------------------------------#
#/ CREATE EMPTY DF OUTPUTS                                               -------

# /----------------------------------------------------------------------------#
#/    GET LH PARAMETER RANGES                                          -------
# pars_all <- read.csv('../output/results/fit/mcmc/parameters/pars_modMCMC_2021.csv')  #v1
pars_all <- read.csv('../output/results/fit/lh/parameters/pars_range_lh_2021.csv')
glimpse(pars_all)

# INTIALIZE MODMCMC & MODFIT
source('data_proc/overlay/initialize_prefitting.r', local=FALSE)

# Create output df
corr_r2_df <- data.frame()

# /----------------------------------------------------------------------------#
#/   LOOP THROUGH FITTED PARAMETERS


for (pars in c('min','avg','max')) 1{
# for (pars in c('avg')) {

  pars_sub <- pars_all %>% filter(fun == pars)
  
  for (i in 1:nrow(pars_sub)) {
  
  
    # Set parameters
    theta_rice    = as.numeric(pars_sub[i, 'theta_rice'])
    theta_pasture = as.numeric(pars_sub[i, 'theta_pasture'])
    theta_urban   = as.numeric(pars_sub[i, 'theta_urban'])
    s_i           = as.numeric(pars_sub[i, 's_i'])
    p_i           = as.numeric(pars_sub[i, 'p_i'])
    
    # /--------------------------------------------------------------------------#
    #/.. Run the wetland loss mapping                                     --------
    source('data_proc/overlay/run_draindistrib_loop_jan2021.r', local=FALSE)
    
    # Save R2 metrics
    corr_r2_df <- bind_rows(corr_r2_df, corr_r2_row)
    
    }
  }


# Save R2 table to file
write.csv(corr_r2_df, paste0('../output/results/fit/lh/parameters/corr_r2_parameters_t', test_theta, '_v1.csv'))
