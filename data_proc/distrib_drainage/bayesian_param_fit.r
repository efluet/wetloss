# Metropolis-Hastings  (DRAM)

# Likelihood function of the model that we want to fit is the probability (density) 
# expected the observed data to occur conditional on the parameters of the model
#------------------------------------------------------------------------------#

# Make function that takes in parameters, does the overlapping, compares to Davidson's sites 
make_wetloss <- function(p){
  
  theta_rice = p[1]
  theta_pasture = p[2] 
  theta_urban = p[3]
  
  # ptm <- proc.time()
  # Run the wetland loss mapping 
  # source('./data_proc/overlay/full_serial_formcmc.r', local=TRUE)
  # source('./data_proc/overlay/full_serial_formcmc_faster.r', local=TRUE)
  source('./data_proc/overlay/full_serial_formcmc_faster_v2.r', local=TRUE)
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
  
  cs_joined <- histcases_wmaploss_data
  
  # residuals
  resid <- cs_joined$map_wetloss_prc_mean - cs_joined$perc_change_numeric

  # return(cs_joined$map_wetloss_prc_mean) # return residuals
  return(resid)
  }


# Get residuals
# Residuals  <- function(p) (cs_joined$perc_change_numeric - make_wetloss(p))

# /----------------------------------------------------------------------------#
#/
library(FME)

# /----------------------------------------------------------------------------#
#/   Least square fit model to data

params <- c(theta_rice = 1,  #0.5
           theta_pasture = 1,   #0.5
           theta_urban = 1)  # 0.8

fit <- modFit(f=make_wetloss,
             p=params,
             lower=c(0.001, 0.001, 0.001),
             upper=c(2,2,2),
             method='Pseudo',  # 'Marq'
             control=list(numiter=2)  # , varleft=10^-4
             )

# /----------------------------------------------------------------------------#
#/  MCMC
#  Make distributions of priors
# theta_rice_d = runif(0, 100, 1000)
# theta_pasture_d = runif(0, 100, 1000)
# theta_urban_d = runif(0, 100, 1000)

source('./data_proc/overlay/initial_mcmc_fitting.r')

MCMC <-  modMCMC(f=make_wetloss, #Residuals, #,
                 p=params,
                 lower=c(0.001, 0.001, 0.001),
                 upper=c(3,3,3),
                 niter=200,
                 var0=0.4,  # 0.4  # prior mean for σ2 
                 wvar0=1,   # prior accuracy; =1, equal weight given to prior and current value; 0 then the prior is ignored.
                 # outputlength=niter
                 updatecov=2
                 )



plot(MCMC, Full = TRUE, col = 'red')
hist(MCMC, Full = TRUE, col = 'darkblue')


saveRDS(MCMC, '../output/results/wetloss/wetloss_theta_mcmc_parameters.rds')

MCMC <- readRDS('../output/results/wetloss/wetloss_theta_mcmc_parameters.rds')



# These variances can be obtained from the mean squares of fitted residuals.
# When var0 is not NULL, then f is assumed to return the model residuals OR an 
# instance of class modCost.
# When var0=NULL, then f should return either -2*log(probability of the model), 
# or an instance of class modCost.


par(mfrow = c(2,2))
hist(MCMC$pars, xlab='x', freq = FALSE, main = 'unconstrained', xlim = c(6, 14))
hist(MCMC$pars, xlab='x', freq = FALSE, main = 'x>9', xlim = c(6, 14))
hist(MCMC$pars, xlab='x', freq = FALSE)#, main = 'pri(x)~N(8,1)', xlim = c(6, 14))
plot(MCMC, mfrow = NULL, main = 'AM')
mtext(outer = TRUE, line = -1.5, 'N(10,1)', cex = 1.25)


#------------------------------------------------------------------------------#

library(BayesianTools)
citation('BayesianTools')

set.seed(123)

ll <- generateTestDensityMultiNormal(sigma = 'no correlation')
bayesianSetup = createBayesianSetup(likelihood = ll, 
                                    lower = rep(-10, 3), 
                                    upper = rep(10, 3))

# Running MCMC and SMC functions
# 
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

