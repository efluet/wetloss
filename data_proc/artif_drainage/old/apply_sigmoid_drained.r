# /----------------------------------------------------------------------------#
#/     Apply the fit to countries with 3+ pts                               ----

# Get function that applies sigmoid
source('./data_proc/artif_drainage/fcn/fcn_apply_sigmoid_drainage.r')

# make list of years
y = seq(1700, 2020, 10) 

ifrm(predall)

# get unique cases, to then loop through
ucases <- unique(d[,c('country_name','continent', 'type')])

# loop unique cases
for (i in 1:nrow(ucases)){
  
  c = as.character(ucases[i,'country_name'])
  t = as.character(ucases[i,'type'])
  o = as.character(ucases[i,'continent'])
  
  # subset data to unique case
  di = d %>% filter(d$country_name == c & d$type == t)
  
  # apply the sigmoid fit 
  pred <-  applysigmoid(di, fitall)
  
  # Cap the drained area to the maximum data point
  pred[pred$pred_drained > max(di$drained_area_tot),'pred_drained'] <- max(di$drained_area_tot)
  
  # append to output
  if (!exist(predall)){predall <- pred} else{ predall <- bind_rows(predall, pred)}
}


# /----------------------------------------------------------------------------#
#/     Save predicted drained area
write.csv(predall, "../output/results/artif_drainage/drained_wetcult_ha_sigmoidpred_v2.csv")




#   FACEt plot
source('./plot/artif_drain/lineplot/facet_sigmoid_all.r')