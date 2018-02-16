library(countrycode)



f <- '../../data/hist_records/wetland_loss_cases_combined_v2.csv'
histcases <- read.csv(f, stringsAsFactors = F, na.strings=c("","NA"))
rm(f)

# select certaint columns
histcases <- histcases[,4:20]

# make country code column 
histcases <- histcases %>%
              mutate(country_code = countrycode(histcases$country,
                                                'country.name','iso3c',warn=F)) %>%
              dplyr::select(-one_of("long","lat"))



# plot wetland loss duration
histcases$nb_yrs <- as.numeric(histcases$nb_yrs)
histcases$perc_change_numeric <- as.numeric(histcases$perc_change_numeric)



ggplot(histcases) +
    geom_hline(yintercept = 0) +
    geom_point(aes(x=nb_yrs, y=perc_change_numeric,  color=wet_categ), 
               size=3, alpha=0.3) +
  xlab('Record Length (years)')



