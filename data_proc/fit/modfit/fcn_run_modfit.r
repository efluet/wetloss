#/ Wrapper function that sets MCMC parameters                   ----------------
# Argument: 
#    s_i = simwet index
#    p_i = preswet index
#    niteration= number of mcmc iterations

library(FME)

# /-----------------------------------------------------------------------------
#/ Run modfit
run_modfit <- function(s_i, p_i, niteration) {
  
  # set initial params
  params <- c(theta_rice = 2,  theta_pasture = 1, theta_urban = 2)

  
  MCMC <-  modMCMC(f=make_wetloss_df,    # function to be evaluated
                   p=params,             # initial values for the parameters to be optimized over
                   s_i,                  # additional arguments passed to function f
                   p_i, 
                   lower=c(0.0001, 0.0001, 0.0001),
                   upper=c(10, 10, 10),
                   niter=niteration,   # 4,
                   var0=0.4,       # 0.4  # prior mean for σ2 
                   wvar0=0.0,     # prior accuracy; =1, equal weight given to prior and current value; 0 then the prior is ignored.
                   # outputlength=niter
                   updatecov=2 )
  
  return(MCMC)
}




