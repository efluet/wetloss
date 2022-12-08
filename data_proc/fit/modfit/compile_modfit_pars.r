
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
  t <- readRDS(paste0(p, '/', f))  
  #  plot(t, Full=T)
    
  # Extract individual parameters 
  # row <- data.frame(f, t$par[1], t$par[2], t$par[3], t$ssr)
  row <- data.frame(f, t$bestpar[1], t$bestpar[2], t$bestpar[3]) #, t$SS)
    
  # append row to df
  par_df <- bind_rows(par_df, row)
  
  }


# Rename columns
names(par_df) <- c('run_id','theta_rice','theta_pasture','theta_urban','ssr')

# Convert columns of run IDs
par_df <- par_df %>% 
          mutate(s = as.numeric(substr(run_id, 9, 9)),
                 p = as.numeric(substr(run_id, 12, 12)))


# /----------------------------------------------------------------------------#
#/  Write to csv
write.csv(par_df, '../output/results/mcmc/parameters/pars_modFit_v2.csv')




f= fls[12]
t <- readRDS(paste0(p, '/', f))
summary(t, cov=TRUE)
print(t)
FME::print.summary(t)

plot(t$rsstrace, type='line')

plot(t, full=T)

# Largest residuals:  
# Yangtze 14 has lasrgest resids
# Montana: n53 has reds\\sid 40 
# Wyoming: n77 has resid of 22