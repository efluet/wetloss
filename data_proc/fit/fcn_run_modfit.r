#/ Wrapper function that sets MCMC parameters                   ----------------
# Argument: 
#    s_i = simwet index
#    p_i = preswet index
#    niteration= number of mcmc iterations

library(FME)

# /-----------------------------------------------------------------------------
#/  Run modfit

run_modfit <- function(s_i, p_i, niteration) {
  
  # set initial params
  startingparams <- c(theta_rice = 2,  theta_pasture = 1, theta_urban = 2)


  fit <- modFit(f = make_wetloss_df, 
                p = startingparams,  #c(2, 0.5, 3),
                s_i=s_i, 
                p_i=p_i,               # additional arguments passed to function f
                lower=c(0.0001, 0.0001, 0.0001), 
                upper=c(100, 100, 100), 
                method='Pseudo',    # Pseudorandom fit
                control = c(numiter = niteration, verbose = TRUE))
  
  
  return(fit)
}



