# Get correlation metrics
corr_r2_df <- read.csv(paste0('../output/results/fit/lh/parameters/corr_r2_parameters_t', test_theta, '_v1.csv')) %>% 
              mutate(run=paste0('s', s_i, '_p', p_i)) %>% 
              dplyr::select(-X) %>% 
              as_tibble()


# p_names=c('GIEMSv2','GLWD3','WAD2M')
p_names=c('WAD2M','GLWD3','GIEMSv2')
s_names=c('DLEM','ORCHIDEE','SDGVM','LPJ-wsl')




# /----------------------------------------------------------------------------#
#/ Compile data from runs
remwet_sum_runs <- data.frame()

# Loop to append all mean par runs in a single df
for(s_i in seq(1,4)){
  
  for(p_i in seq(1,3)){
    
    for(pars in c('avg','min','max')){
      
      
      # GET % REMWET DATA
      f <- paste0('../output/results/wetloss/sum/sum_remwet_s', s_i, '_p', p_i, '_t', test_theta, '_', pars, '_v1.csv')
      remwet_sum <- read.csv(f) %>% 
                    mutate( pars=pars, s_name=s_names[s_i], p_name=p_names[p_i], run=paste0('s',s_i,'_p',p_i)) %>% 
                    filter(year==2020) %>% dplyr::select(-X)
                  
      
      # GET DRAIN AREA KM2
      f_out <- paste0('../output/results/wetloss/sum/sum_drain_perlu_s', s_i, '_p', p_i, '_t', test_theta, '_', pars,'_v1.csv')
      total_drain_perlu_peryear <- 
        read.csv(f_out) %>%  
        as_tibble() %>% filter(year==2020) %>% 
        summarise(cumul_drain_Mkm2 = sum(cumul_drain_km2/10^6, na.rm=T)) %>% 
        mutate( pars=pars, s_name=s_names[s_i], p_name=p_names[p_i], run=paste0('s',s_i,'_p',p_i))
      
      # COMBINE
      remwet_sum <- left_join(remwet_sum, total_drain_perlu_peryear, by=c('s_name','p_name','run','pars'))
      
      # Bind rows to df
      remwet_sum_runs <- bind_rows(remwet_sum_runs, remwet_sum)
    }  
  }
}




# /----------------------------------------------------------------------------#
#/ combine into one table

remwet_sum_runs <- left_join(remwet_sum_runs, corr_r2_df, by=c('run', 'pars')) %>% as_tibble()

write.csv(remwet_sum_runs, '../output/results/fit/lh/parameters/runs_summary_table_forSI_v1.csv', row.names=F)



# /----------------------------------------------------------------------------#
#/  Format table into range columns (avg, min, max)
remwet_sum_runs2 <- 
  remwet_sum_runs %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  dplyr::select(-year, -s_i, -p_i, -remwet_km2, -remwet_perc, -f.r.squared, -f.adj.r.squared, -fw.r.squared) %>% 
  # Pivot longer to put all metrics in same column
  pivot_longer(cols=c(percloss, cumul_drain_Mkm2, RMSE:fw.adj.r.squared), names_to='metric', values_to='vals') %>% 
  mutate(metric=paste0(metric, '_', pars)) %>% 
  dplyr::select(-pars) %>% 
  # Then pivot wider to make columns for each metric x min,avg,max
  pivot_wider(id_cols=c(run, s_name, p_name), names_from=metric, values_from=vals) %>% 
  mutate(percloss_rng = paste0(percloss_avg, ' (', percloss_min,' - ', percloss_max,')'),
         RMSE_rng = paste0(RMSE_avg,' (', RMSE_min,' - ', RMSE_max,')'),
         bias_rng = paste0(bias_avg,' (', bias_min,' - ', bias_max,')'),
         cumul_drain_Mkm2_rng = paste0(cumul_drain_Mkm2_avg,' (', cumul_drain_Mkm2_min,' - ', cumul_drain_Mkm2_max,')'),
         fw.adj.r.squared_rng = paste0(fw.adj.r.squared_avg,' (', fw.adj.r.squared_min,' - ', fw.adj.r.squared_max,')')) %>% 
  dplyr::select(-percloss_avg, -percloss_min, -percloss_max, -RMSE_avg, -RMSE_min, -RMSE_max,
                -bias_avg, -bias_min, -bias_max, -cumul_drain_Mkm2_avg, -cumul_drain_Mkm2_min, -cumul_drain_Mkm2_max,
                -fw.adj.r.squared_avg, -fw.adj.r.squared_min, -fw.adj.r.squared_max)


remwet_sum_runs2

write.csv(remwet_sum_runs2, '../output/results/fit/lh/parameters/runs_summary_table_forSI_formatted_v1.csv', row.names=F)
