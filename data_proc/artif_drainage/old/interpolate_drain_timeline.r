# Description:  reads and interpolate time series of drainage

library(Hmisc)

# read artif drainage
# d <- read.csv("./output/results/artif_drainage/drained_wfrac.csv", stringsAsFactors = F)
d <- read.csv("./output/results/artif_drainage/drained_wetcult_ha.csv", stringsAsFactors = F)


# list of years
hyde_yrs <- seq(1700, 2000, 10)

# /----------------------------------------------------------------------------#
#/     Select countries with multiple data points                   ------------

# select the countries with multiple data points
d <-  d %>%
  dplyr::select(country_name, type, year, drained_area_tot) %>%
  # round year to closest decade 
  # mutate(year = round(year, -1)) %>%
  # average multiple values per year
  group_by(country_name, type, year) %>%
  dplyr::summarise(drained_area_tot = mean(drained_area_tot)) %>%
  ungroup() %>%
  # group by country
  group_by(country_name) %>%
  # count number of data points
  add_tally() %>%
  # filter to countries with at least 1 data points
  filter(n >= 1) %>%
  # remove count column
  ungroup()


# add empty rows for each missing year, where the interpolation will be inserted
d <- d %>%
      distinct() %>%
      
      ## fill with all the years with data and the decadal intervals from hyde
      # complete(., nesting(country_name, type), year=unique(hyde_yrs)) %>%
      complete(., nesting(country_name, type), year=unique(c(hyde_yrs, unique(d$year)))) %>%
      # sort by country and year
      arrange(country_name, type, year)



# /----------------------------------------------------------------------------#
#/      Interpolate the NA betwen years with data                       -------

# import package that has some ts interpolation functions
library(zoo)

di <- d %>%

  #  ungroup %>%
  group_by(country_name, type) %>%
  
  mutate(ts_drained_area_tot = na.approx(drained_area_tot,  method="linear", na.rm=FALSE)) 
  # rule 2 means "last observation carry forward", and first backward
  # rule=2, maxgap = Inf)) %>% #xout=f_drained,
  ungroup()


# make temporary df of min-max years with data for each country
# each country is a row
minmaxdatayear <- di %>%

  filter(!is.na(drained_area_tot)) %>%
  group_by(country_name, type) %>%
  dplyr::summarize(mindatayear = min(year), maxdatayear = max(year))


# join the interpolated data to the min-max years
di <- left_join(di, minmaxdatayear, by=c("country_name", "type"))


di <- di %>%
  # create a label of whether a value is interpolated or actuala data 
  mutate(valtype = ifelse(!is.na(drained_area_tot),  "data", "interp")) %>%
  # keep only decadal years of data
  #filter(year %in% hyde_yrs) %>%
  group_by(country_name, type) %>%
  
  # label the years beyond data as "extrapolated"
  mutate(valtype = ifelse(year < mindatayear, "extrapol", valtype),
         valtype = ifelse(year > maxdatayear, "extrapol", valtype)) %>%
  
  # add label of country groups to use
  mutate(continent = countrycode(country_name, "country.name", "region")) %>%
  mutate(continent = ifelse(continent %in% c("Central America","South America", "Caribbean"),"Central & South America",continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Europe","Southern Europe", "Northern Europe"), "Western Europe", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Northern America"), "North America", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Africa", "Southern Africa","Eastern Africa","Middle Africa"), "Africa", continent))
  


rm(minmaxdatayear)

#==============================================================================#
###   Select the longest time-series for each continent x type      ------------
#==============================================================================#

# ISSUES:  - MULTIPLE ROWS OF SAME YEAR WHEN ROUNDING YEAR VALUE (PREVENTING LM PREDICTIONS)
#          - AMERICAS TREATED AS ONE CONTINENT (SO VENEZUELA IS MISSING)

##  IN THE FUTURE: FIT A CURVE TO THE MOST DATA DENSE TIME SERIES PER BLOCKS OF 
##  CONTINENT & DRAINAGE TYPE

# For cropland: 
#  - Europe:  Italy (since 1850) / Germany (since 1970) / Estonia (since 1940)
#  - North America:  USA
#  - South America: Venezuela
#  - Asia:  China (since 1950)
#  - Northern Africa: Egypt (since 1950)
#  - Former ussr: Russia (since 1930)
#  - Sub-saharan Africa: Kenya (since 1970)


# Summarize time series lenght for each continent x type 
# use regional example of trajectory per region

country_ts_length <- di %>%
  
  # count the number of "data" points per group 
  mutate(n_obs = sum(valtype == "data")) %>%
  # filter to only the last year of data (just cause it needs to be boiled down to a single row)
  filter(year == maxdatayear) %>%
  # calculate the length 
  mutate(ts_length = maxdatayear - mindatayear) %>%
  # group by continent
  group_by(continent, type) %>%
  # keep only rows of countries that have the longest ts for each continent x type combination
  filter(ts_length == max(ts_length)) %>%
  select(country_name, continent, type, ts_length, mindatayear, maxdatayear, n_obs) %>%
  filter(n_obs > 3)



# subseting the countries with longest time series per strata (to then use them as templates)
country_ts_template <- semi_join(di, country_ts_length, by=c("country_name", "type")) %>%
                       mutate(drain_frac = ts_drained_area_tot / max(ts_drained_area_tot)) 


hyde_yrs <- seq(1700, 2020, 10)

source("./scripts/data_proc/fcn/fcn_ts_linear_extrapolation.r")

# run the function
extrapol <- linear_extrapol(country_ts_length, country_ts_template, 2)



template_extrapol <- extrapol %>%
            mutate(ts_drained_area_tot=ifelse(valtype=="extrapol", drained_area, ts_drained_area_tot)) %>%
            #filter(year %in% hyde_yrs) %>%
            select(-one_of(c("drained_area",  "drain_frac")))




###  APPLY  TEMPLATE TO OTHER COUNTRIES               --------------------------

template_extrapol_f <- template_extrapol %>%
            group_by(country_name, type) %>%
            mutate(drain_frommax = ts_drained_area_tot / max(ts_drained_area_tot, na.rm=T)) 
  

# stands for drainage - inteporlated - extrapolated
die <- left_join(di, template_extrapol_f, by=c("type", "continent", "year"))
names(die) <- c("country_name", "type", "year", "ts_drained_area_tot", "valtype", "continent",
                "template_country_name", "template_ts_drained_area_tot", "template_valtype", "template_frac_frommax" )

die <- die %>% 
  group_by(country_name, type) %>%
  #mutate(maxd = max(ts_drained_area_tot))
  mutate(ts_drained_area_tot_templateapplied = ifelse(valtype=="extrapol" & country_name != template_country_name,
                                      max(ts_drained_area_tot, na.rm=T) * template_frac_frommax,
                                      ts_drained_area_tot)) %>%
  
  mutate(ts_drained_area_tot_templateapplied = ifelse(country_name == template_country_name,
                                                      template_ts_drained_area_tot,
                                                      ts_drained_area_tot_templateapplied)) %>%
  ungroup %>%
  filter(year %in% hyde_yrs)




###   PROBLEM is that extrapolation of FORESTRY has it GROWING back in time, because of FInalnd template...


#==============================================================================#
###  PLOT the extrapolation      -----------------------------------------------
#==============================================================================#

source("./scripts/plots/artif_drainage/lineplot_drainage_inter_n_extrapol_1000ha.r")

### old plot
# source("./scripts/plots/artif_drainage/lineplot_drainage_interpolated_1000ha.r")

####   
source("./scripts/plots/artif_drainage/barplot_totaldrainage.r")


# # Use Italy as a model timeline, to be applied to other countries
# it <- di %>% 
#   filter(country_name=="Italy", type=="cropland") %>%
#   mutate(f = ts_drained_area_tot / max(ts_drained_area_tot))
# v <- it$f 
# v <- v[c(1:22,23:32)]
# length(v)
# 
# di <- di %>%
#       group_by(country_name, type) %>%
#       mutate(ts_drained_area_tot = ifelse(is.na(ts_drained_area_tot), 
#                                           max(ts_drained_area_tot, na.rm=T), ts_drained_area_tot)) %>%
#       mutate(ts_drained_area_tot = ts_drained_area_tot * v)
#       # add_tally() %>%
#       # filter(n >= 1) %>%
#       # dplyr::select(-n) %>%
#       # ungroup()



# # loop through HYDE year
# for (y in hyde_yrs){
#   
#   # for debugging
#   y <- hyde_yrs[1]
#   
#   # select the raster of that year
#   t <- sel.by.pattern(raster_stack, y)
#   
#   # subset the national statistics to that year
#   artdrain_type_nat_temp <- subset(artdrain_type_nat, decade==y)
#   
#   # Grid the fraction drained
#   r <- raster(t, values=FALSE)
#   x <- rasterize(artdrain_type_nat,t,getCover=TRUE,progress="text")
#   # multiply drainage fraction with the area raster 
#   # add to stack
# }

# df %>%
#   group_by(Individuals) %>%
#   mutate(ValueInterp = na.approx(Value, na.rm=FALSE)
# 

### an attempt to fit sigmoidal curve to the data
# artdrain_type_nat_data_p =  artdrain_type_nat_data %>% 
#                             group_by(country_name) %>%
#                             do(
#                               fit = lm(f_drained ~ poly(year, 3, raw=TRUE), 
#                                          data=artdrain_type_nat_data),
#                               
#                               predict(fit, newdata = data.frame(x = seq(1500, 2000, length.out = 50))))
# do(fit = nls(f_drained ~ SSlogis(input=year, Asym=1), 
#              data=artdrain_type_nat_data))