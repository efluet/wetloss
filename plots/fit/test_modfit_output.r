


s_i=1
p_i=1
niteration=1000

# f11 <- paste0('../output/results/mcmc/mcmc_obj/s1_p1_i1000.rds')
s1p1i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s1_p1i1000.rds'))
s1p2i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s1_p2i1000.rds'))
s1p3i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s1_p3i1000.rds'))

s2p1i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p1i1000.rds'))
s2p2i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p2i1000.rds'))
s2p3i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p3i1000.rds'))

s3p1i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p1i1000.rds'))
s3p2i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p2i1000.rds'))
s3p3i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p3i1000.rds'))

s4p1i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p1i1000.rds'))
s4p2i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p2i1000.rds'))
s4p3i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s2_p3i1000.rds'))


plot(s1p1i1000, Full=TRUE)
plot(s1p2i1000, Full=TRUE)
plot(s1p3i1000, Full=TRUE)

plot(s2p1i1000, Full=TRUE)
plot(s2p2i1000, Full=TRUE)
plot(s2p3i1000, Full=TRUE)

plot(s3p1i1000, Full=TRUE)
plot(s3p2i1000, Full=TRUE)
plot(s3p3i1000, Full=TRUE)

plot(s2p1i1000, Full=TRUE)

summary(s2p3i1000)

# saveRDS(myMCMC, out_f)
# plot(mcmc)
# summary(s1p1i1000)
# s1p1i1000$naccepted

#-------------------------------------------------------------------------------


s1p1i1500 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s1_p1_i1500.rds'))
s1p2i1500 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/s1_p2_i1500.rds'))


plot(s1p1i1500, Full=TRUE, trace=TRUE)
plot(s1p2i1500, Full=TRUE)




hist(s1p1i2000, Full = TRUE, col = "darkblue")


MC <- as.mcmc(s1p2i1500$pars)
raftery.diag(MC)
cumuplot(MC)  # Line plot of parameter fit


##---------------------------------------------------------------------------
# Get parameters in df
MCMCpars <- as.data.frame(s1p1i2000$pars)
MCMCpars$SS <- s1p1i2000$SS
MCMCpars$sig <- s1p1i2000$sig

# Remove first 50% of iterations as burn in
MCMCpars <- MCMCpars[100:2000,]

MCMCpars <- MCMCpars %>% distinct %>% arrange(desc(SS))  # arrange(sig)

MCMCpars <- MCMCpars[1:100,]


hist(MCMCpars$theta_rice)
hist(MCMCpars$theta_pasture)
hist(MCMCpars$theta_urban)

pairs(s1p2i1500, nsample = 1000)

glimpse(MCMCpars)




summary(as.mcmc(s1p1i2000$pars))

  # Get 5% and 95% parameters
  # Each of the three parameter value is taken separately; low parameters -> low loss, high params -> high loss
  pars_sel <- data.frame(lapply(MCMCpars[1:3], quantile, prob = c(0.025, 0.975), names = TRUE)) 
  pars_sel$type <- row.names(pars_sel)
  
  # Get best parameter (diff var in mcmc object)
  bestpar <-  as.data.frame(t(MCMCout$bestpar)) %>% mutate(type='bestvar', var_model=max(MCMCout$sig))
  
  pars_sel <- bind_rows(pars_sel, bestpar)
  pars_sel$type <- c('low','high','best')
  # Write preswet name in column
  pars_sel$preswet <- names(preswet_stack)[p_i] #preswet_name
  pars_sel$niter <- nrow(MCMCpars)
  
  return(pars_sel)    
}


# PARAMS DENSITY PLOTS
m <- as.mcmc(s1p2i1500$pars[1000:1500]) #will make an instance of class mcmc, usable by coda.
densplot(m)  # coda density plot


gelRub <- coda::gelman.diag(multDRAM, autoburnin = TRUE)$mpsrf


# Parameter range
sR<-sensRange(parInput=s1p2i1500$pars, 
              func=make_wetloss_df(s_i, p_i)) # x=1:1500)


R> sR <- sensRange(func = HIV, parms = pars, parInput = MCMC$par)
R> plot(summary(sR), xlab = "time")

# The distribution is plotted and the data added to the plot:
plot(summary(sR), quant = TRUE)
points(Obs)




a <- FME::modFit(f = make_wetloss_df, 
            p = c(2, 0.5, 2),
            s_i=s_i, 
            p_i=p_i,               # additional arguments passed to function f
            lower=c(0.001, 0.001, 0.001), 
            upper=c(10, 10, 10), 
            method='Pseudo',  
            control = c(numiter = 200, verbose = TRUE))

plot(a)


# /----------------------------------------------------------------------------#
#/
s1p1i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p1_i1000.rds'))
s1p2i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p2_i1000.rds'))
s1p3i1000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p3_i1000.rds'))


s1p1i1000$par
s1p2i1000$par
s1p3i1000$par


plot(s1p1i1000$rsstrace[,2] ~ s1p1i1000$rsstrace[,1])
summary(s1p1i1000)


s1p1i1000$par
s1p1i1000$poppar   # all parameter vectors remaining in the population



a$residuals
a$par # he best set of parameters found.
a$ssr
a$var_ms # NULL
a$var_ms_unscaled
a$var_ms_unweighted
a$ssr




s1_p1_i2000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p1_i2000.rds'))
# plot(s1_p1_i2000$rsstrace[,2] ~ s1_p1_i2000$rsstrace[,1])
plot(s1_p1_i2000)


s1_p2_i2000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p2_i2000.rds'))
# plot(s1_p2_i2000$rsstrace[,2] ~ s1_p2_i2000$rsstrace[,1])
plot(s1_p2_i2000)


s1_p3_i2000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s1_p3_i2000.rds'))
# plot(s1_p3_i2000$rsstrace[,2] ~ s1_p3_i2000$rsstrace[,1])
plot(s1_p3_i2000)


s2_p1_i2000 <- readRDS(paste0('../output/results/mcmc/mcmc_obj/modfit_s2_p1_i2000.rds'))
# plot(s2_p1_i2000$rsstrace[,2] ~ s2_p1_i2000$rsstrace[,1])
plot(s2_p1_i2000)




# ## =======================================================================
# ## Type 1 input: name, time, value
# ## =======================================================================
# ## Create new data: two observed variables, "a", "b"
# Data <- data.frame(name = c(rep("a", 4), rep("b", 4)),
#                    time = c(1:4, 2:5), val = c(runif(4), 1:4))
# 
# ## "a nonsense model"
# Mod <- function (t, y, par) {
#   da <- 0
#   db <- 1
#   return(list(c(da, db)))
# }
# 
# out <- deSolve::ode(y = c(a = 0.5, b = 0.5), times = 0:6, func = Mod, parms = NULL)
# 
# Data # Show;  Observed 
# out  # model output
# 
# ## The cost function
# m <- FME::modCost(model = out, obs = Data, y = "val")
# 
# # ## The cost function with a data error added
# # Dat2 <- cbind(Data, Err = Data$val*0.1) # error = 10% of value
# # 
# # FME::modCost(model = out, obs = Dat2, y = "val", err = "Err")
