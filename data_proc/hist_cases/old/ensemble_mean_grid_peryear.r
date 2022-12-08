# Description: Averages wetland area grids from 


# get
source("./scripts/r/plots/fcn/average_grids.r")


# create ouput directory string
odir <- './output/results/wetloss/grid/'

# create function that tests if object exists
exist <- function(x) { return(exists(deparse(substitute(x))))}


# get hyde years  =====================================================
hyde_yrs_all <- readRDS('./output/results/hyde_yrs/hyde_yrs_all.rds')
hyde_yrs_all <- hyde_yrs_all[hyde_yrs_all >= -6000]
hyde_yrs_all <- abs(hyde_yrs_all)

hyde_yrs_since1700 <- readRDS('./output/results/hyde_yrs/hyde_yrs_since1700.rds')




### ENSEMBLE MEAN OF  ============================

# get remwet & wetloss grids
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_2.5deg.rds')
wetloss_Mkm2_stack <- readRDS('./output/results/wetloss/grid/wetloss_Mkm2_stack_2.5deg.rds')

# subset of models
toMatch <- c("lpxdytop")

# averate  remwet since 1700
mean_stack <- average_grids(hyde_yrs_all, remwet_Mkm2_stack, toMatch)
saveRDS(mean_stack, paste0(odir, 'remwet_Mkm2_stack_2.5deg_mean_year.rds'))

# averate wetloss since 1700
mean_stack <- average_grids(hyde_yrs_all, wetloss_Mkm2_stack, toMatch)
saveRDS(mean_stack, paste0(odir, 'wetloss_Mkm2_stack_2.5deg_mean_year.rds'))




################################################################################

# get remwet
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg.rds')
wetloss_Mkm2_stack <- readRDS('./output/results/wetloss/grid/wetloss_Mkm2_stack_0.5deg.rds')


# list of models to include; excluding SDGVM because of exceedingly high wetalnd area
toMatch <- c("DLEM","fmax")


# averate  remwet since 6000BC
mean_stack <- average_grids(hyde_yrs_all, remwet_Mkm2_stack)
saveRDS(mean_stack, paste0(odir, 'remwet_Mkm2_stack_0.5deg_mean_year.rds'))

# averate wetloss since 6000BC
mean_stack <- average_grids(hyde_yrs_all, wetloss_Mkm2_stack)
saveRDS(mean_stack, paste0(odir, 'wetloss_Mkm2_stack_0.5deg_mean_year.rds'))


