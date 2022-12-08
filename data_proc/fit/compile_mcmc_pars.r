
# Set dir where modFit object are located
p <- '../output/results/fit/mcmc/mcmc_obj/i3000'

# List of modFit configs
fls <- list.files(path=p, full.names = F)

# Create empty df
par_df <- data.frame()  

# /----------------------------------------------------------------------------#
#/  Loop through files
for (f in fls) {
  
  print(f)
  
  # Open modFit object
  MCMCout <- readRDS(paste0(p, '/', f))
    
  # Extract individual parameters 
  # row <- data.frame(f, t$bestpar[1], t$bestpar[2], t$bestpar[3]) #, t$SS)
  
  pars <- as.data.frame(MCMCout$pars) %>%
    mutate(SS =   MCMCout$SS) %>% 
    mutate(sig =   MCMCout$sig[,1]) %>%
    distinct() %>% 
    arrange(SS) %>% 
    mutate(run_id = f) %>% 
    mutate(s = as.numeric(substr(run_id, 10, 10)),
           p = as.numeric(substr(run_id, 13, 13)))
  
  # Get the best parameters based on SS
  pars_row <- pars[1, ]  # pars[1:10, ]

  # append row to df
  par_df <- bind_rows(par_df, pars_row)
  
  }


# Rename columns
names(par_df) <- c('run_id','theta_rice','theta_pasture','theta_urban','ssr','run_id','s_i','p_i')


glimpse(par_df)


# /----------------------------------------------------------------------------#
#/  Write to csv
write.csv(par_df, '../output/results/fit/mcmc/parameters/pars_modMCMC_2021.csv')




# f= fls[12]
# t <- readRDS(paste0(p, '/', f))
# summary(t, cov=TRUE)
# print(t)
# FME::print.summary(t)
# 
# plot(t$rsstrace, type='line')
# 
# plot(t, full=T)

# Largest residuals:  
# Yangtze 14 has lasrgest resids
# Montana: n53 has reds\\sid 40 
# Wyoming: n77 has resid of 22