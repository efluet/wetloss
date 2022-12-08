library(nls.multstart)
library(nls2)
library(broom)
library(Hmisc)
library(nls2)
library(rcompanion)
library(soilphysics)
library(rsq)
#library(iterators)
#library(purrr)



# /----------------------------------------------------------------------------#
#/     Select countries with multiple data points                           ----

# Read and interpolate time series of drainage
# d <-  read.csv('../data/artif_drained/all_drain_stat_comb_v6.csv')  %>%
d <-  read.csv('../data/artif_drained/all_drain_stat_comb_v7.csv')  %>%
      filter(type %in% c('Cropland','Forestry')) %>% 
      filter(!is.na(drained_area_tot)) %>%
      filter(exclude!='exclude') %>%
      # select some columns
      dplyr::select(country_name, region, type, year, drained_area_tot, continent, peatland_only) %>% 
      # convert area from 1000 ha to km^2  ( / 100 * 1000)
      # TODO: RENAME COLNAME WITH KM2 
      mutate(drained_area_tot = drained_area_tot / 100 * 1000)  %>%
      filter(!is.na(year))


### NEED TO DECIDE IF PEATLAND DATA FROM JOOSTEN IS USEFUL
# USE JOOSTEN DATA IN COUNTRIES WHERE PEATLAND CROPLAND IS GREATER THAN NON-PEATLAND SOURCES, OR ONLY ONES AVAILABLE
# d <- d %>% filter(peatland_only == '') 


### PEAT EXTRACTION AREA SEPARATELY PROCESSED
# Run script on peatextraction first
names(drained_peatex_int) <- c('country_name','type','decade','drained_weight','drained_weight_cumsum','drained_area_tot')
d <- bind_rows(d, drained_peatex_int)

# Because peat extraction already underwent processing, use the decade col as year col
d <- d %>% 
  mutate(year=ifelse(is.na(year), decade, year)) %>% 
  mutate(region=ifelse(is.na(region), '', region)) %>% 
  # add label of country groups to use
  # NOX2020- ADDED THIS BECAUSE CONTINENT IS LOST IN PEAT
  mutate(continent = countrycode(country_name, "country.name", "region")) %>%
  mutate(continent = ifelse(continent %in% c("Central America","South America", "Caribbean"),"Central & South America",continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Europe","Southern Europe", "Northern Europe"), "Western Europe", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Asia","Central Asia"), "Western & Central Asia", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Northern America"), "North America", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Africa", "Southern Africa","Eastern Africa","Middle Africa"), "Africa", continent)) %>%
  # lump australia & NZ to same as Europe
  mutate(continent = ifelse(country_name %in% c('Australia','New Zealand'), "Europe & Central Asia", continent))



d <-  d %>% 
      # average multiple values per year
      group_by(country_name, region, type, year, continent) %>%
      dplyr::summarise(drained_area_tot = mean(drained_area_tot)) %>%
      ungroup() %>%
      # group by country
      group_by(country_name, region, type, continent) %>%
      # count number of data points
      add_tally() %>%
      # filter to countries with at least 1 data points; effectively removing nodata pts
      filter(n >= 1) %>%
      # remove count column
      ungroup()


# /----------------------------------------------------------------------------#
#/      Get longest time-series for each continent x type                   ----
templates <-  d %>%
              filter(n >= 4) %>%
              # Exclude regional data (NOV2021)
              filter(region == '') %>%
              filter(!is.na(year)) %>%
              group_by(country_name, region, type, continent) %>%
              # Data needs to cover minimum of 20 years 
              filter(max(year) - min(year) > 20) %>%
              filter(year == max(year)) %>% 
              # NOV2020  - BELARUS & POLAND DO NOT CONVERGE, EXCLUDE FROM TEMPLATES
              filter(!(type == 'Cropland' & country_name %in% c('Belarus', 'Poland') ))



# /----------------------------------------------------------------------------#
#/      Loop through template countries, fitting sigmoid                  -----

# delete fit all if it exists
ifrm(fitall)
ifrm(predtemplates)


# Loop through templates
for (i in 1:nrow(templates)){
  
  c = as.character(templates[i,'country_name'])
  t = as.character(templates[i,'type'])
  r = as.character(templates[i,'region'])
  o = as.character(templates[i,'continent'])
  
  # subset data to one country, on type, and non-NA area
  di = d %>% filter(d$country_name == c & d$type == t, d$region==r,
                    !is.na(drained_area_tot))
  
  if (nrow(di) > 0) {
    
    
    # /------------------------------------------------------------------------#
    #/     Fit Logistic curve                                               ----
    
    # Asym: numeric parameter representing the asymptote.
    # xmid: x value at the inflection point of the curve. 
    # The value of SSlogis will be Asym/2 at xmid.
    # scal: numeric scale parameter on the input axis.
    fit= nls_multstart(drained_area_tot ~ Asym/(1+exp((xmid-year)/scal)),  # SSlogis(year, Asym, xmid, scal),
                       data=di,
                       
                       start_lower=c(min(di$drained_area_tot)/20, # 0.1,
                                     min(di$year)/2,  # /2 is new
                                     min(di$drained_area_tot)),
                       
                       start_upper=c(max(di$drained_area_tot)*100,
                                     max(di$year)*1000,
                                     max(di$drained_area_tot)*1000),
                       
                       # lower = c(0.1,
                       #           1700,
                       #           min(di$drained_area_tot)),
                       # 
                       # upper = c(max(di$drained_area_tot)*10000,
                       #           max(di$year)*100,
                       #           max(di$drained_area_tot)*10000),
                       
                       supp_errors='Y',
                       convergence_count=FALSE, #300,
                       iter=1200)
    
    # print ticker
    print(paste(c, t, nrow(di), ' PseudoR^2=', round(Rsq(fit)$pseudo.R.squared, 3)))
    # accuracy(fit)
    
    # convert fit to a table
    tfit <- tidy(fit) %>%
            # add describing columns
            mutate(country_name= c,
                   continent= as.character(templates[i,'continent']),
                   type = t)

    
    # Use model to predict drainage across time series 
    pred = predict(fit, newdata = data.frame(year = seq(1700, 2020, 10)))
    
    pred[pred > max(di$drained_area_tot)] <- max(di$drained_area_tot)
    
    pred <- data.frame(pred_drained = round(pred,2), 
                       year = seq(1700, 2020, 10),
                       country_name = c,
                       region=r,
                       type = t, 
                       continent = o,
                       pseudo.R.squared = Rsq(fit)$pseudo.R.squared,
                       adj.R.squared = Rsq(fit)$adj.R.squared)
    
    # Bind rows of fit parameters to df
    if (!exist(fitall)){fitall <- tfit} else{ fitall <- bind_rows(fitall, tfit)}
    
    # Bind rows of predicted drainage
    if (!exist(predtemplates)){predtemplates <- pred} else{ predtemplates <- bind_rows(predtemplates, pred)}
    
  }
}


# /----------------------------------------------------------------------------#
#/     Write to file the predicted templates

# Save predicted templates
# NOV2020- I THINK THESE ARE REFIT ANYWAY DURING THE PREDALL STEP
write.csv(predtemplates, '../output/results/artif_drainage/drained_sig_predtemplates_v4_regions.csv')

# Save fit parameters
fitall <- fitall %>% filter(term != 'xmid' | (term=='xmid' & estimate <= 2040))
write.csv(fitall, '../output/results/artif_drainage/drained_sig_fitparameters_v4_regions.csv')


# Save processed drainage data- for use as data points in Figure 1
write.csv(d, '../output/results/artif_drainage/drained_data_fullproc_forfig1.csv')


# /----------------------------------------------------------------------------#
#/   Save facet line plot of templates
source('./plots/artif_drainage/sigmoid_template_facet.r')


