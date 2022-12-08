# /----------------------------------------------------------------------------#
#/        SET WD                                                    ----------
library(here); here() # setwd to the location of the project
source('./data_proc/prep_init.r') # Prep functions, themes, etc.
library(ggpubr)  #ggarrange
library(fasterize)
options(row.names=FALSE, scipen = 999)


# /-----------------------------------------------------------------------------
#/ Run the MCMC
source('./data_proc/distrib_drainage/bayesian_param_fit_with_df_v2.r')



# /------------------------------------------------------------------
#/   Prep MCMC, changing the preswet
# Args: s_i, p_i, niteration
s_i = 3
p_i = 1
niteration= 1000

# /------------------------------------------------------------------
#/   Run MCMC & save
# ptm <- proc.time()
myMCMC  <- run_my_mcmc(s_i, p_i, niteration)
# proc.time() - ptm

out_f <- paste0('../output/results/mcmc/mcmc_obj/s', s_i, '_', 'p', p_i, 'i', niteration, '.rds')
saveRDS(myMCMC, out_f)


# /------------------------------------------------------------------
#/  Get and save parameters
pars  <- get_pars_range(myMCMC)
out_f <- paste0('../output/results/mcmc/parameters/s', s_i, '_', 'p', p_i, 'i', niteration, '.rds')
write.csv(pars, out_f)

