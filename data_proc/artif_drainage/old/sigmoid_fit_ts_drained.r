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
d <- read.csv('../data/artif_drained/all_drain_stat_comb_v6.csv')  %>%
  filter(type %in% c('Cropland','Forestry')) %>% 
  filter(!is.na(drained_area_tot)) %>%
  # select some columns
  dplyr::select(country_name, region, type, year, drained_area_tot) %>%
  # average multiple values per year
  group_by(country_name, region, type, year) %>%
  dplyr::summarise(drained_area_tot = mean(drained_area_tot)) %>%
  ungroup() %>%
  # group by country
  group_by(country_name, region, type) %>%
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
              group_by(country_name, region, type) %>%
              filter(max(year) - min(year) > 20) %>%
              filter(year == max(year))


# /----------------------------------------------------------------------------#
#/      Loop through template ecountries, fitting sigmoid                  -----

# delete fit all if it exists
ifrm(fitall)
ifrm(predtemplates)


for (i in 1:nrow(templates)){
  
  c = as.character(templates[i,'country_name'])
  t = as.character(templates[i,'type'])
  o = as.character(templates[i,'continent'])
  
  # subset data to one country, on type, and non-NA area
  di = d %>% filter(d$country_name == c & d$type == t,
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
                       
                       start_lower=c(min(di$drained_area_tot)/10, # 0.1,
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
    print(paste(c, t, nrow(di), " PseudoR^2=", Rsq(fit)$pseudo.R.squared))
    accuracy(fit)
    
    
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
#/     Write to file the predited templates
write.csv(predtemplates, "../output/results/artif_drainage/drained_sig_predtemplates_v3.csv")

write.csv(fitall, "../output/results/artif_drainage/drained_sig_fitparameters_v3.csv")




# /----------------------------------------------------------------------------#
#/     Plot template countries                                             -----

d_forplot <- semi_join(d, predtemplates, by=c("country_name"="country_name", "type"="type"))

m <-  ggplot() +
  geom_point(data=d_forplot,
             aes(x= year, y= drained_area_tot, color=type)) +
  
  geom_line(data=predtemplates, aes(x= year, y= pred_drained, color=type)) +
  
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free", ncol=3) +
  
  line_plot_theme +
  theme(legend.position = c(0.9, 0.03)) +
  ylab("Area drained (km^2)") + xlab("")


### save plot ------------------------------------------------------------------
ggsave(plot=m, "./output/figures/artif_drainage/sigmoid/template/drain_sigmoidtemplates.png",
       width=6.5, height=8.5, dpi=400, units='in' , type = "cairo-png")

dev.off()
