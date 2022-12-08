# process ensemble



# read overlay output of diff resolutions
wetloss_all_0.5 <- read.csv('./output/results/global_sum_wetloss_wetchimp_0.5_3subgrid.csv')
wetloss_all_1.0 <- read.csv('./output/results/global_sum_wetloss_wetchimp_3subgrid.csv')

wetloss_all_0.5$res <- 0.5
wetloss_all_1.0$res <- 1.0

# combine into one df
wetloss_all <- bind_rows(wetloss_all_0.5, wetloss_all_1.0)

# remove temps
rm(wetloss_all_0.5, wetloss_all_1.0)



# prepare remwer data =========================================================

# get remaining wetland area in 1700, to calc remwet percent from then
wetarea_in1700 <- wetloss_all %>%

  # keep years after 1700
  filter(year == 1700) %>%
  # select columns
  dplyr::select(name, overlap, tot_remwet_Mkm2) %>%
  # rename column to reflect its only in 1700
  rename(tot_remwet_Mkm2_in1700 = tot_remwet_Mkm2)


# prep data
wetloss_all<- wetloss_all %>%
  
  # remove years not ending in 10
  filter(year %% 10 ==0) %>%
  
  # join the remwet in 1700 and calc percent remaining area since then
  left_join(., wetarea_in1700, by=c("name", "overlap")) %>%
  
  # calculate the % from the 1700 wetcover 
  mutate(remwet_prc_since1700 = tot_remwet_Mkm2 / tot_remwet_Mkm2_in1700 * 100)




# calculate the ensemble min, mean, max
wetloss_ensemble_prc <- wetloss_all %>%
  
      # remove non-numeric columns from the summarize
      dplyr::select(-one_of(c("X","name", "overlap","res"))) %>%
      # group by year
      mutate(year = as.character(year)) %>%
      group_by(year) %>%
      # calculate the summary values
      summarise_all(.funs=c('mean', 'min', 'max')) %>%
      # return year to a numeric
      mutate(year= as.numeric(year))



# write outputs 
write.csv(wetloss_all,          "./output/results/wetloss/wetloss_all")
write.csv(wetloss_ensemble_prc, "./output/results/wetloss/wetloss_ensemble_prc")



# delete objects
rm(wetloss_all, wetarea_in1700, wetloss_ensemble_prc)

