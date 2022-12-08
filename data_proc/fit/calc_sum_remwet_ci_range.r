
# Get range of estimates from 
runs <- c('s1_p1', 's1_p2', 's1_p3', 's2_p1', 's2_p2', 's2_p3', 
          's3_p1', 's3_p2', 's3_p3', 's4_p1', 's4_p2', 's4_p3')

# /----------------------------------------------------------------------------#
#/   GET RANGE OF MEAN PARS                                      ---------

remwet_sum_mean_range <- data.frame()
pars = 'avg'

# Loop to append all mean par runs in a single df
for(r in runs){
  
  f <- paste0('../output/results/wetloss/sum/sum_remwet_', r,'_t', test_theta, '_', pars, '_v1.csv')
  remwet_sum <- read.csv(f) %>% mutate(run = r)# read file
  remwet_sum_mean_range <- bind_rows(remwet_sum_mean_range, remwet_sum)
  }

# Get min & max of mean par runs
remwet_sum_mean_range <- 
          remwet_sum_mean_range %>% 
          dplyr::select(-X) %>% 
          group_by(year) %>% 
          summarise_all(list(min=min, max=max, median=median)) #.funs=c(min,max))


# /----------------------------------------------------------------------------#
#/   GET RANGE OF MIN/MAX PARS                                      ---------

remwet_sum_minmax_range <- data.frame()

# Loop through min and max runs
for(r in runs){
  
  # print(r)
  # Read in files
  pars='min'
  f_min <- paste0('../output/results/wetloss/sum/sum_remwet_', r,'_t', test_theta,'_', pars, '_v1.csv')
  pars='max'
  f_max <- paste0('../output/results/wetloss/sum/sum_remwet_', r,'_t', test_theta,'_', pars, '_v1.csv')
  
  remwet_sum_min <- read.csv(f_min)
  remwet_sum_max <- read.csv(f_max)
  
  remwet_sum_minmax_range <- bind_rows(remwet_sum_minmax_range, remwet_sum_min, remwet_sum_max)
  }

# Calculate range
remwet_sum_minmax_range <- 
      remwet_sum_minmax_range %>% 
      dplyr::select(-X) %>% 
      group_by(year) %>% 
      summarise_all(list(min = min, max=max))


# reset the pars variable
pars <- 'avg'

