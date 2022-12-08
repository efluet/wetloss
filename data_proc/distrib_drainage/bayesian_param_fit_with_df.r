# Metropolis-Hastings  (DRAM)
# Likelihood function of the model that we want to fit is the probability (density) 
# expected the observed data to occur conditional on the parameters of the model
#------------------------------------------------------------------------------#

# Make function that takes in theta parameters, does the overlapping, compares to Davidson's sites 
source('./data_proc/mcmc_fit/fcn_make_wetloss_df.r', local=T)  

# /----------------------------------------------------------------------------#
#/ Wrapper function that sets MCMC parameters                   ----------------
source('./data_proc/mcmc_fit/fcn_run_mcmc.r', local=T)  


# /----------------------------------------------------------------------------#
#/   Initialize MCMC run (only once)
source('./data_proc/overlay/initial_mcmc_fitting_df_v2.r', local=T)


# /----------------------------------------------------------------------------#
#/  Function that gets 0.025, 0.50, 0.974 percentile of  theta parameters   ------
source('./data_proc/mcmc_fit/fcn_get_pars_range.r', local=T)



# Run the MCMC, changing the preswet
# Args: s_i, p_i, niteration
# MCMC_1_1  <- run_my_mcmc(1, 1, 2)
# MCMC_giems2 <- run_my_mcmc(giems2_wet_df)
# MCMC_glwd   <- run_my_mcmc(glwd3_wet_a_df)




# /------------------------------------------------------------------
#/  Run MCMC parameters
# pars_wad2m  <- get_pars_range(MCMC_wad2m, 'wad2m')
# pars_giems2 <- get_pars_range(MCMC_giems2,'giems2')
# pars_glwd   <- get_pars_range(MCMC_glwd,  'glwd')

niter = 4
# 
pars_1_1  <- get_pars_range(run_my_mcmc(s_i=1, p_i=1, niteration=niter))
pars_1_2  <- get_pars_range(run_my_mcmc(s_i=1, p_i=2, niteration=niter))
pars_1_3  <- get_pars_range(run_my_mcmc(s_i=1, p_i=3, niteration=niter))

pars_2_1  <- get_pars_range(run_my_mcmc(s_i=2, p_i=1, niteration=niter))
pars_2_2  <- get_pars_range(run_my_mcmc(s_i=2, p_i=2, niteration=niter))
pars_2_3  <- get_pars_range(run_my_mcmc(s_i=2, p_i=3, niteration=niter))

pars_3_1  <- get_pars_range(run_my_mcmc(s_i=3, p_i=1, niteration=niter))
pars_3_2  <- get_pars_range(run_my_mcmc(s_i=3, p_i=2, niteration=niter))
pars_3_3  <- get_pars_range(run_my_mcmc(s_i=3, p_i=3, niteration=niter))

pars_4_1  <- get_pars_range(run_my_mcmc(s_i=4, p_i=1, niteration=niter))
pars_4_2  <- get_pars_range(run_my_mcmc(s_i=4, p_i=2, niteration=niter))
pars_4_3  <- get_pars_range(run_my_mcmc(s_i=4, p_i=3, niteration=niter))



# combine 
pars_all <- bind_rows(pars_giems2, pars_wad2m, pars_glwd)

write.csv(pars_all, '../output/results/mcmc_pars/mcmc_pars_all_dec2020.csv')


# plot(MCMC_glwd, Full = TRUE, col = 'red')
# hist(MCMC, Full = TRUE, col = 'darkblue')
# 
# saveRDS(MCMC, '../output/results/wetloss/wetloss_theta_mcmc_parameters_fromdf_wad2m_v2.rds')
# MCMC <- readRDS('../output/results/wetloss/wetloss_theta_mcmc_parameters_fromdf.rds')



# These variances can be obtained from the mean squares of fitted residuals.
# When var0 is not NULL, then f is assumed to return the model residuals OR an 
# instance of class modCost.
# When var0=NULL, then f should return either -2*log(probability of the model), 
# or an instance of class modCost.
# par(mfrow = c(2,2))
# hist(MCMC$pars, xlab='x', freq = FALSE, main = 'unconstrained', xlim = c(6, 14))
# hist(MCMC$pars, xlab='x', freq = FALSE, main = 'x>9', xlim = c(6, 14))
# hist(MCMC$pars, xlab='x', freq = FALSE)#, main = 'pri(x)~N(8,1)', xlim = c(6, 14))
# plot(MCMC, mfrow = NULL, main = 'AM')
# mtext(outer = TRUE, line = -1.5, 'N(10,1)', cex = 1.25)

# /----------------------------------------------------------------------------#
#/   Least square fit model to data
# 
# params <- c(theta_rice = 1,  #0.5
#             theta_pasture = 1,   #0.5
#             theta_urban = 1)  # 0.8
# 
# fit <- modFit(f=make_wetloss,
#               p=params,
#               lower=c(0.001, 0.001, 0.001),
#               upper=c(2,2,2),
#               method='Pseudo',  # 'Marq'
#               control=list(numiter=2) )  # , varleft=10^-4

#------------------------------------------------------------------------------#
# library(BayesianTools)
# citation('BayesianTools')
# 
# set.seed(123)
# 
# ll <- generateTestDensityMultiNormal(sigma = 'no correlation')
# bayesianSetup = createBayesianSetup(likelihood = ll, 
#                                     lower = rep(-10, 3), 
#                                     upper = rep(10, 3))

#===============================================================================
# Running MCMC and SMC functions
##  Make distributions of priors
# theta_rice_d = runif(0, 100, 1000)
# theta_pasture_d = runif(0, 100, 1000)
# theta_urban_d = runif(0, 100, 1000)
# run a calibration. runMCMC function is the main wrapper for all other implemented MCMC/SMC functions. 
# A bayesianSetup (alternatively, the log target function)
# The sampler name
# A list with settings
# As an example, choosing the sampler name “Metropolis” calls a versatile Metropolis-type MCMC with options for covariance adaptation, delayed rejection, tempering and Metropolis-within-Gibbs sampling. For details, see the the later reference on MCMC samplers. This is how we would call this sampler with default settings

# iter = 1000
# settings = list(iterations = iter, message = FALSE)
# out <- runMCMC(bayesianSetup = bayesianSetup, sampler = 'Metropolis', settings = settings)
# 
# summary(out)
# plot(out)


#  Least Squate
# (cs_joined$map_wetloss_prc_mean - cs_joined$perc_change_numeric)^2/ length(cs_joined)

# RMSE
#mean((cs_joined$map_wetloss_prc_mean - cs_joined$perc_change_numeric)^2)^(1/2)

# proc.time() - ptm

# - formcmc_faster with subsDT  =  295 sec for 31 years
# - formcmc_faster with subsDT & zonalDT  =  259 sec for 31 years  ????
# - the remwet & extract add an additional 103 sec


# Get remwet
# Estimate RemWet from drained area.
# source('./data_proc/make_tot_remwet_grid_additive.r', local=TRUE)
# 
# # Extract the loss at Davidson locations
# #.. DATA PROC - extract raster data with poly histcases  and join them to the histcase loss%
# source('./data_proc/hist_cases/extract_raster_wetloss_with_histcases_v5_fromncdf.r', local=TRUE)
#
# # Calculate least square differences to Davidson loss
# cs_joined <- readRDS('../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod_wmaploss_v2_serial.rds')
# cs_joined <- cs_joined@data

# cs_joined <- histcases_wmaploss_data

# Get residuals
# Residuals  <- function(p) (cs_joined$perc_change_numeric - make_wetloss(p))