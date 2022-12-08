library(countrycode) # package for country iso code
library(measurements) # package for lat/long conversion


# read hitorical cases data
# f <- './data/hist_records/wetland_loss_cases_combined_v2.csv'
f <- './data/hist_records/wetland_loss_cases_combined_v2_manmod.csv'

histcases <- 
  read.csv(f, stringsAsFactors = F, na.strings=c("", " ","NA")) %>%
  mutate(nb_yrs = as.numeric(nb_yrs),
         perc_change_numeric = as.numeric(perc_change_numeric))

# convert from decimal minutes to decimal degrees
histcases$lat_pt2 = measurements::conv_unit(histcases$lat_pt, from = 'deg_dec_min', to = 'dec_deg')
histcases$long_pt2 = measurements::conv_unit(histcases$long_pt, from = 'deg_dec_min', to = 'dec_deg')



histcases <- histcases %>%
  # make country code column for joining with polygon 
  mutate(country_code = countrycode(histcases$country, 'country.name','iso3c',warn=T)) %>%
  dplyr::select(-one_of("long_pt","lat_pt", "X", "X.1")) %>%
  
  filter(yr_start>1500) %>%
  mutate(yr_start = ifelse(yr_start<1700, 1700, yr_start)) %>%
  
  filter(nb_yrs > 10) %>%
  filter(perc_change_numeric < 100) %>%
  filter(!is.na(yr_start), !is.na(yr_end))


# write formatted histcase output 
write.csv(histcases, './output/results/histcases_loss_v2_manmod_p.csv')


# delete objects
rm(histcases, f)






# convert columns to numeric
# histcases$nb_yrs <- as.numeric(histcases$nb_yrs)
# histcases$perc_change_numeric <- as.numeric(histcases$perc_change_numeric)


# filter to remove outlier records
# i.e. with large increase (for plotting purpose; but also could be faulty record)
# PROBLEM: REMOVES VAN ASSELEN DATA  DURING FILE
#histcases <- histcases[histcases$perc_change_numeric < 200,]

