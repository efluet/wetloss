

applysigmoid <- function(di, fitall){
 
    # /--------------------------------------------------------------------------#
  #/     Get prefitted parameters
  #      Take median values from templates countries, bc multiple countries can be in a single group
  
  if (t == "Cropland"){
  
    # Get the pre-fitted Xmid;  imposing time stamp from more data rich country
    prefitxmid <- fitall %>% filter(continent == o, type == t, term == 'xmid') %>% pull(estimate) %>% median()
    # Get the fitted scale;  imposing time stamp from more data rich country    
    prefitscal <- fitall %>% filter(continent == o, type == t, term == 'scal') %>% pull(estimate) %>% median()

  } else {
      
    # Get the pre-fitted Xmid;  imposing time stamp from more data rich country
    prefitxmid <- fitall %>% filter(type == t, term == 'xmid') %>% pull(estimate) %>% median()
    # Get the fitted scale;  imposing time stamp from more data rich country    
    prefitscal <- fitall %>% filter(type == t, term == 'scal') %>% pull(estimate) %>% median()
    
    }  
  
  print(paste(i, '-', c, '-', t, '-', o, ' - n:', nrow(di), ' - prefitxmid:', round(prefitxmid, 2)))
  

  

  # /--------------------------------------------------------------------------#
  #/     Refit countries with 4+ points used as templates             ----------
  
  if (nrow(di) >= 4 & (max(di$year) - min(di$year) >= 20)) {

    # if there is a prefit xmid, continue
    # to skip  australia  ;  NOV2020 - DONT SKIP AUSTRALIA
    if (!is.na(prefitxmid)) {
    
      
      fit= nls_multstart(drained_area_tot ~ Asym/(1+exp((prefitxmid-year)/scal)),  # SSlogis(year, Asym, xmid, scal),
                         data=di,
                         
                         start_lower=c(min(di$drained_area_tot)/20, # 0.1,
                                       min(di$year)/2,  # /2 is new
                                       min(di$drained_area_tot)),
                         
                         start_upper=c(max(di$drained_area_tot)*100,
                                       max(di$year)*1000,
                                       max(di$drained_area_tot)*1000),
                         
                         # start_lower=c(0.1, 
                         #               prefitxmid-30, 
                         #               min(di$drained_area_tot)),
                         # start_upper=c(max(di$drained_area_tot)*50, 
                         #               prefitxmid+30, 
                         #               max(di$drained_area_tot)*100),

                         convergence_count= FALSE, #300,
                         supp_errors='Y',
                         iter=1200)

      # predict 
      pred = predict(fit, newdata = data.frame(year = y))
      
      pred <- data.frame(pred_drained = round(pred,2), 
                         year = y,
                         country_name = c,
                         type = t, 
                         continent = o)
      }
  }
  
  
  # /--------------------------------------------------------------------------#
  #/     For countries 1 or 2 points                                      -----
  
  if ((nrow(di) <= 3) ){   # |  (max(di$year) - min(di$year) < 20)

    # if there is not pre fit xmid, skip this one (e.g. in australia...)
    if (!is.na(prefitxmid)) { 
      
      # Predict with the median fitted sigmoidal parameters (varies by continent for cropland)
      pred =  max(di$drained_area_tot) / (1+exp((prefitxmid-y)/prefitscal))
      
      pred <- data.frame(pred_drained = round(pred,2), 
                         year = y,
                         country_name = c,
                         type = t, 
                         continent = o)
    }
  }
  
  # /--------------------------------------------------------------------------#
  #/     after both if statements, return the pred                        ------
  if (exist(pred))  { return(pred) }
}
