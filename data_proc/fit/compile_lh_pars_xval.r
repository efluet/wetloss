# /----------------------------------------------------------------------------#
#/ Combine xval runs of fitting ran on SHERLOCK          --------

# Get list of files
fl <- list.files(path='../output/results/fit/xval', full.names = T, pattern='.csv', 
                 recursive=TRUE)

# Empty data frame
xval_df <- data.frame()


# /----------------------------------------------------------------------------#
#/  Loop through fold files and append to single df
for (f in fl){

  print(f)  
  # Append to output df
  xval_df <- bind_rows(xval_df, 
                       read.csv(f) %>% mutate(f=basename(f)) )
  }


# /----------------------------------------------------------------------------#
#/ Post-process the xval runs 

xval_df <- xval_df %>%
  # Split filename into components
  separate(f, c('a1','a2','a3','a4','a5','a6','a7','a8')) %>% 
  dplyr::select(-c(a1,a2,a3,a7)) %>% 
  rename(s_i=a4, p_i=a5, nbiter=a6, fold=a8) %>% 
  distinct() %>% 
  arrange(sum_sq_resid_train) %>%
  mutate(s_i = as.numeric(substr(s_i, 2, 2)),
         p_i = as.numeric(substr(p_i, 2, 2)))


glimpse(xval_df)


# /----------------------------------------------------------------------------#
#/  Filter to N best runs

nbestruns = 100

xval_df <- 
  xval_df %>% 
  group_by(s_i, p_i, fold) %>% 
  slice_max(order_by = sum_sq_resid_train, n = nbestruns)

glimpse(xval_df)



# /----------------------------------------------------------------------------#
#/  Get  cross val metrics

# Mean and range training R^2
median(xval_df$r.squared_train, na.rm=T)
range(xval_df$r.squared_train, na.rm=T)

# Mean and range validation R^2
median( xval_df$r.squared_val, na.rm=T)
range(xval_df$r.squared_val, na.rm=T)

#---------------------------------
# Mean and range training RMSE
median(xval_df$rmse_train, na.rm=T)
range(xval_df$rmse_train, na.rm=T)

# Mean and range validation R^2
median( xval_df$rmse_val, na.rm=T)
range(xval_df$rmse_val, na.rm=T)




# # /----------------------------------------------------------------------------#
# #/  Loop through members and subset the top 50 or 100  pars             -------
# for (f in fls) {
#   
#   print(f)
#   
#   # Open modFit object
#   pars <- read.csv(paste0(p, '/', f)) %>% dplyr::select(-X, -X.1)
#   
#   pars <- pars %>% 
#     distinct() %>% 
#     arrange(sum_sq_resid) %>%   # sum_sq_resid
#     mutate(run_id = f) %>% 
#     mutate(run_id = substr(run_id, 20, 24),
#            s_i = as.numeric(substr(run_id, 2, 2)),
#            p_i = as.numeric(substr(run_id, 5, 5)))
#   
#   # Get the best parameters based on SS
#   best_pars <- pars[1:nbestruns, ]
# 
#   # append row to df
#   best_pars_df <- bind_rows(best_pars_df, best_pars) %>% as_tibble()
#   }
# 
# # Rename columns
# names(best_pars_df) <- c('theta_rice','theta_pasture','theta_urban','sum_resid','sum_sq_resid','run_id','s_i','p_i')
# 
# 
# # Write to csv
# # write.csv(best_pars_df, '../output/results/fit/lh/parameters/best_pars_lh_2021.csv')
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/  Get mean best models                                  --------
# 
# pars_df_range <-  
#   best_pars_df %>% 
#   dplyr::select(theta_rice, theta_pasture, theta_urban, run_id, s_i, p_i) %>%  #, sum_resid, sum_sq_resid) %>% 
#   # Group by wetland runs
#   group_by(run_id, s_i, p_i) %>% 
#   # Get summary stats
#   summarize_all(list(min=min, avg=median, max=max), na.rm=T) %>% 
#   ungroup() %>% 
#   pivot_longer(cols=theta_rice_min:theta_urban_max, names_to='theta', values_to='vals') %>% 
#   mutate(fun=str_sub(theta, -3, -1)) %>% 
#   mutate(theta=str_sub(theta, 1, 10)) %>% 
#   mutate(theta=ifelse(theta=='theta_past','theta_pasture',theta),
#          theta=ifelse(theta=='theta_urba','theta_urban',theta)) %>% 
#   pivot_wider(id_cols=c(run_id,s_i,p_i,fun), names_from=theta, values_from=vals)
# 
# glimpse(pars_df_range)

# Write to csv
# write.csv(pars_df_range, '../output/results/fit/lh/parameters/pars_range_lh_2021.csv', row.names=F)




# /----------------------------------------------------------------------------#
#/   Get average residual per wetland run
# pars_df_meanresid <-  
#   best_pars_df %>% 
#   dplyr::select(run_id, s_i, p_i, sum_resid, sum_sq_resid) %>%
#   # Group by wetland runs
#   group_by(run_id, s_i, p_i) %>%
#   summarize_all(list(min=min, avg=mean, max=max), na.rm=T)
# 



# Largest residuals:  
# Yangtze 14 has lasrgest resids
# Montana: n53 has reds\\sid 40 
# Wyoming: n77 has resid of 22



# mutate(run_id = f) %>% 
# run_id = substr(run_id, 20, 24),
# write.csv(bind_rows(r_1, r_2), paste0('../output/results/fit/lh/lh_pars_resid_comb_', r, '.csv'))

# /----------------------------------------------------------------------------#
#/  Read combined pars

# Set dir where modFit object are located
# p <- '../output/results/fit/lh'

# List of modFit configs
# fls <- list.files(path=p, full.names = F, pattern='comb')

# Create empty df
# best_pars_df <- data.frame()