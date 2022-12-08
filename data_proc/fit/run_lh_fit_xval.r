# /----------------------------------------------------------------------------#
#/   This is the script to copy in every R member 

#  SET WD
setwd(here())
source('./data_proc/prep_init.r') # Prep functions, themes, etc.
options(row.names=FALSE, scipen = 999)
# library(fasterize)
# library(raster)
# library(dplyr)
# library(countrycode)
# library(sf)
library(lhs)
library(Metrics)


#####################################
cs_peat=0   # Include peatland histcases in comparison or not

# Run config
test_theta = 0       # use test theta values (of 3, 0.5, 5)
null_theta = 0       # use null theta values (of 0,0,0)
test_potwet = 0      # use testing potwet (combination of preswet #1 and simwet #4) ?dbl check numbers
preswet_max = 1      # Use larger preswet area instead of mamax preswet
update_potwet = 0    # whether to subtract drained area from potwet; !!! turning this off reduces need for expanding allowable & filling overlap
save_all_output = 0  # Save all outputs for fig 2 & 3

# Drain_distrib config
fill_val = 10e-4    # Fill value for perc_overlap; influences balance between real LUoverlap and filled
scale_allowable = 1   # Scale up allowable, for forestry and peat during redistribution loop
expand_perc_overlap = 1  # Expand perc_overlap to every gridcell after first distribution
nb_repeats = 20      # Number of redistribution loops  


#######################################
# Args: s_i, p_i, niteration
s_i = 4
p_i = 1

niteration = 3 #1000
foldid = 3  # Select data from fold

par_range_min = 10^-5
par_range_max = 30
######################################



#  1
#   Initialize inputs for fitting (run once at the start)
source('./data_proc/overlay/initialize_prefitting.r')

## This is the part in prefitting reading the histcases
histcases_df <- read.csv("../output/results/histcases/histcases_wdata_2021_rasterdf.csv")
histcases_df_recid <- unique(histcases_df$rec_id)
histcases_df_recid <- histcases_df_recid[!is.na(histcases_df_recid)]


# 1.5
# Break into k folds
library(caret)
n_folds = 10  # Set number of folds

# Create folds
folds <- createFolds(histcases_df_recid, k=n_folds, list = FALSE)

# Filter rec_ids in fold
fold_recid_val <- histcases_df_recid[folds==foldid]
fold_recid_train <- histcases_df_recid[folds!=foldid]


#  2
#    Get function that takes in theta parameters, 
#    does the drain distrib, compares to histcases, then outputs residuals
#    This is the function that gets optimized
source('./data_proc/fit/fcn_runmodel_getresiduals_2021_bootstrap.r', local=T) 
# source('./data_proc/fit/fcn_runmodel_getresiduals_2021.r', local=T) 


#  3
# Latin Hypercube sampling generates more efficient estimates of desired parameters than simple Monte Carlo sampling.
set.seed(1234)
pars <- data.frame(improvedLHS(niteration, 3, 3)  * par_range_max + par_range_min)
names(pars) <- c('theta_rice', 'theta_pasture', 'theta_urban')


#  4
#    Run the fitting
outdf <- data.frame()

for (p in 1:nrow(pars)){
  
  temp_pars <- c(pars[p,])
  cs_joined_resid <- make_wetloss_df(temp_pars, s_i, p_i) # This returns a df of both train and val residuals
  
  # Change 0s to small values; for log to work
  cs_joined_resid[cs_joined_resid$remwet_end==0,'remwet_end'] <- 10^-3
  
  # split into train/val
  cs_joined_val   <- cs_joined_resid[cs_joined_resid$rec_id %in% fold_recid_val,]
  cs_joined_train <- cs_joined_resid[!cs_joined_resid$rec_id %in% fold_recid_val,]
  
  # /----------------------------------------------------------------------------#
  #/ Calculate agreement metrics

  # RMSE
  rmse_train <- rmse(cs_joined_train$map_perc_lost, cs_joined_train$perc_change_numeric)
  rmse_val   <- rmse(cs_joined_val$map_perc_lost, cs_joined_val$perc_change_numeric)
  
  # r2
  r.squared_train <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost, data=cs_joined_train))$r.squared
  r.squared_val   <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost, data=cs_joined_val))$r.squared
  
  # adj r2
  adj.r.squared_train <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost, data=cs_joined_train))$adj.r.squared
  adj.r.squared_val   <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost, data=cs_joined_val))$adj.r.squared
  
  # weighted r2
  w.r.squared_train <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost,
                                weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2))), data=cs_joined_train))$r.squared
  w.r.squared_val <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost, 
                                weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2))), data=cs_joined_val))$r.squared

  # weighted adj.r2
  w.adj.r.squared_train <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost,
                                      weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2))), data=cs_joined_train))$adj.r.squared
  w.adj.r.squared_val <- summary(lm(perc_change_numeric ~ 0 + map_perc_lost,
                                    weights = log(areapoly_mkm2) + abs(min(log(areapoly_mkm2))), data=cs_joined_val))$adj.r.squared
  
  # bias
  bias_train <- mean(cs_joined_train$map_perc_lost, na.rm=T) - abs(mean(cs_joined_train$perc_change_numeric, na.rm=T))
  bias_val <- mean(cs_joined_val$map_perc_lost, na.rm=T) - abs(mean(cs_joined_val$perc_change_numeric, na.rm=T))
  

  # Append the parameters and residuals to output df
  outdf <- bind_rows(outdf, 
                     data.frame(temp_pars,
                                'sum_resid_train'    = sum(cs_joined_train$resid, na.rm=T), 
                                'sum_sq_resid_train' = sum(cs_joined_train$resid^2, na.rm=T), 
                                'sum_resid_val'    = sum(cs_joined_val$resid, na.rm=T), 
                                'sum_sq_resid_val' = sum(cs_joined_val$resid^2, na.rm=T),
                                'rmse_train' = rmse_train,
                                'rmse_val'   = rmse_val,
                                'r.squared_train'  = r.squared_train,
                                'r.squared_val'    = r.squared_val,
                                'adj.r.squared_train' = adj.r.squared_train,
                                'adj.r.squared_val' = adj.r.squared_val,
                                'bias_train' = bias_train,
                                'bias_val'   = bias_val))
  
  # Every 100 runs, save the output
  if(p == round(p, -1)) {
    out_filename <- paste0('../output/results/fit/lh_pars_resid_s', s_i, '_p', p_i, '_i', niteration, '_xval_k', foldid, '.csv')
    write.csv(outdf, out_filename)  }
}

# Export output again once completed 
out_filename <- paste0('../output/results/fit/lh_pars_resid_s', s_i, '_p', p_i, '_i', niteration, '_xval_k', foldid, '.csv')
write.csv(outdf, out_filename)





# # Append the parameters and residuals to output df
# outdf <- bind_rows(outdf, 
#                    data.frame(temp_pars, #'temp_pars' = temp_pars, 
#                               'sum_resid_train'    = sum(cs_joined_train$resid, na.rm=T), 
#                               'sum_sq_resid_train' = sum(cs_joined_train$resid^2, na.rm=T), 
#                               'sum_resid_val'    = sum(cs_joined_val$resid, na.rm=T), 
#                               'sum_sq_resid_val' = sum(cs_joined_val$resid^2, na.rm=T),
#                               'rmse_train' = median(cs_joined_train$RMSE),
#                               'rmse_val'   = median(cs_joined_val$RMSE),
#                               'r.squared_train'     = median(cs_joined_train$r.squared),
#                               'adj.r.squared_train' = median(cs_joined_train$adj.r.squared),
#                               'r.squared_val'     = median(cs_joined_val$r.squared),
#                               'adj.r.squared_val' = median(cs_joined_val$adj.r.squared),
#                               'bias_train' = median(cs_joined_train$bias),
#                               'bias_val'   = median(cs_joined_val$bias)))


# # VALID - Calculate 2 sets of residuals
# sum_resid_val <- sum(cs_joined_val, na.rm=T)
# sum_sq_resid_val <- sum(cs_joined_val^2, na.rm=T)
# 
# 
# # Calculate 2 sets of residuals
# sum_resid_train <- sum(cs_joined_train, na.rm=T)
# sum_sq_resid_train <- sum(cs_joined_train^2, na.rm=T)

# mutate(perc_change_numeric=abs(perc_change_numeric))
