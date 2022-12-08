s# /----------------------------------------------------------------------------#
#/  Function that gets 0.025, 0.50, 0.974 percentile of  theta parameters             ------
get_pars_range <- function(MCMCout) {
  
  # Get parameters in df
  MCMCpars <- as.data.frame(MCMCout$pars)
  
  # Remove first 50% of iterations as burn in
  # MCMCpars <- MCMCpars[seq(nrow(MCMCpars)*0.5, nrow(MCMCpars), by=1),]
  
  # Get 5% and 95% parameters
  # Each of the three parameter value is taken separately; low parameters -> low loss, high params -> high loss
  pars_sel <- data.frame(lapply(MCMCpars[1:3], quantile, prob = c(0.025, 0.975), names = TRUE)) 
  pars_sel$type <- row.names(pars_sel)
  
  
  
  # The bestpar are selected based on the ones giving the the highest probability (funp).
  # the parameter set that gave the highest probability.
  # Not sure why SS or residuals are not used to determine bestpar
  bestpar <- as.data.frame(MCMCout$bestpar) %>% t()
  names(bestpar) <- c('theta_rice', 'theta_pasture', 'theta_urban') 
  
  
  pars <- as.data.frame(MCMCout$pars) %>%  # [1001:2000,]
    mutate(SS =   MCMCout$SS) %>% 
    mutate(sig =   MCMCout$sig[,1]) %>%
    distinct() %>% 
    arrange(SS) 
  
  pars <- pars[1:10, ]
  
  # filter(SS == min(SS))
  # filter(sig == min(sig))
  
  
  glimpse(pars)
  
  ### GET BEST PARS BASED ON FUNP
  pars <- as.data.frame(MCMCout$pars) %>%
    mutate(SS =   MCMCout$SS) %>% 
    mutate(sig =   MCMCout$sig[,1]) %>%
    dplyr::filter(theta_rice == bestpar[1,'theta_rice'] &
                  theta_pasture == bestpar[1,'theta_pasture'] &
                  theta_urban == bestpar[1,'theta_urban'])
  glimpse(pars)
  

  # find the iteration no that had best par
  # use the iteration to get the SS
  
  
  
  # Get best parameter (diff var in mcmc object)
  bestpar <-  as.data.frame(t(MCMCout$bestpar)) %>% mutate(type='bestvar', var_model=max(MCMCout$sig))
  
  pars_sel <- bind_rows(pars_sel, bestpar)
  pars_sel$type <- c('low','high','best')
  # Write preswet name in column
  pars_sel$preswet <- names(preswet_stack)[p_i] #preswet_name
  pars_sel$niter <- nrow(MCMCpars)
  
  return(pars_sel)    
  }