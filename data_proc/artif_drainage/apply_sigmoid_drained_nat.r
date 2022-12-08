# /----------------------------------------------------------------------------#
#/     Apply the fit to countries with 3 or fewer pts                 ----------

# Get function that applies sigmoid
source('./data_proc/artif_drainage/fcn/fcn_apply_sigmoid_drainage.r')

# make list of years
y = seq(1700, 2020, 10) 

ifrm(predall)

# get NATIONAL unique cases, with at least 1 data points, to which the sigmoid can be applied
# to then loop through these unique cases
d_nat <- d %>% filter(region == '' & !is.na(year) & !is.na(country_name))
ucases <- unique(d_nat[,c('country_name','continent', 'type')])


# /----------------------------------------------------------------------------#
#/ loop unique cases (country x type combinations) to apply sigmoid
for (i in 1:nrow(ucases)){
  
  c = as.character(ucases[i,'country_name'])
  t = as.character(ucases[i,'type'])
  o = as.character(ucases[i,'continent'])
  
  # If NA country
  # if(is.na(c)){ next }
  
  # subset data to unique case
  di = d_nat %>% filter(country_name == c & type == t)
  
  # Apply sigmoid function fitting  
  pred <-  applysigmoid(di, fitall)
  
  # Cap the drained area to the maximum data point; i.e. apply a ceiling value
  pred[pred$pred_drained > max(di$drained_area_tot),'pred_drained'] <- max(di$drained_area_tot)
  
  # Append to output
  if (!exist(predall)){predall <- pred} else{ predall <- bind_rows(predall, pred)}
}



# /----------------------------------------------------------------------------#
#/     Save predicted drained area
write.csv(predall, "../output/results/artif_drainage/drained_wetcult_km2_sigmoidpred_march2021.csv", row.names=F)



# /----------------------------------------------------------------------------#
#/      Facet plot
source('./plots/artif_drainage/lineplot/facet_sigmoid_all_national.r')
