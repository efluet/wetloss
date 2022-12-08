# Metropolis-Hastings  (DRAM)
# Likelihood function of the model that we want to fit is the probability (density) 
# expected the observed data to occur conditional on the parameters of the model
#------------------------------------------------------------------------------#


# /----------------------------------------------------------------------------#
#/   Initialize MCMC run (only once)
source('./data_proc/overlay/initial_mcmc_fitting_df_v2.r', local=T)

# /----------------------------------------------------------------------------#
#/   Make function that takes in theta parameters, does the overlapping, compares to Davidson's sites 
source('./data_proc/mcmc_fit/fcn_make_wetloss_df.r', local=T)  


# /----------------------------------------------------------------------------#
#/ Wrapper function that sets MCMC parameters                          ---------
source('./data_proc/mcmc_fit/fcn_run_mcmc.r', local=T)  


# /----------------------------------------------------------------------------#
#/  Function that gets 0.025, 0.50, 0.974 percentile of  theta parameters   ------
source('./data_proc/mcmc_fit/fcn_get_pars_range.r', local=T)





# Save for each run:
# - figure of parameter convergence across runs
# - figure of posterior parameter value
# - best params

# library(ggmcmc)
# 
# parDRAM <- ggmcmc::ggs(MCMC_1_1) #multDRAM) ## to convert objet for using by all ggs_* graphical functions
# ggmcmc::ggs_traceplot(parDRAM)

# /------------------------------------------
#/  COMBINE  - dec2020 move to later
# pars_all <- bind_rows(pars_giems2, pars_wad2m, pars_glwd)
# write.csv(pars_all, '../output/results/mcmc_pars/mcmc_pars_all_dec2020.csv')
