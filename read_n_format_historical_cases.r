# get package
library(countrycode)
library(dplyr)
library(ggplot2)



# read historical record data
histrec <- read.csv("../../data/hist_records/wetland_loss_cases_combined_v2.csv", stringsAsFactors = F)

# histrec <- as.data.frame(apply(histrec, 2, function(y) gsub(" ", NA, y, fixe=TRUE)))
# histrec <- gsub("", NA, histrec, fixed=TRUE)

# what single metric to use?

# filter to have only t
histrec <- histrec %>%
           filter(!is.na(yr_start), !is.na(yr_end)) %>%
           filter(perc_change <= 0) %>%
           # get continent from country
           mutate(continent = countrycode(country,'country.name','continent', warn=F)) %>%
           mutate(perc_change = as.numeric(perc_change),
                  yr_start = as.numeric(yr_start))






ggplot(histrec) + 
  geom_point(aes(x=yr_start, y=perc_change, color=continent))
