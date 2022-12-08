# Fig 2 C - Drain LU type stacked-area plot

# /----------------------------------------------------------------------------#
#/ Barplot of value types in the time-series           ------

#  Read in data
# die <- read.csv('../output/results/artif_drainage/drained_wetcult_gridded_sum_fullserial_dec2019.csv') %>%
# die <- read.csv("../output/results/artif_drainage/drained_wetcult_gridded_sum.csv") %>% 
#die <- read.csv("./output/results/artif_drainage/drained_wetcult_gridded_sum_serial.csv") %>% 
# dlu <- read.csv('../output/results/wetloss/sum/sum_lu_drain_serialmcmc.csv') %>% 
  
  
dlu <- read.csv('../output/results/wetloss/sum/sum_lu_drain_serialmcmc_wad2m.csv') %>% 
  mutate(lu_type= stri_trans_totitle(lu_type)) %>% 
  mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
         lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
         lu_type= ifelse(lu_type=='Gorestry' , 'Forestry', lu_type),
         lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type)       ) %>% 
  filter(type=='best' & preswet=='wad2m')
  # mutate(cumul_pred_drained = ifelse(is.na(cumul_pred_drained),0,cumul_pred_drained))



dlu$lu_type_fac <- factor(dlu$lu_type, levels=c('Cropland',
                                                'Forestry',
                                                'Pasture',
                                                'Peatland',
                                                'Urban',
                                                'Rice',
                                                'Wetland Cultiv.'))

# # Calculate loss rate
# die_loss_rate<- die %>% 
#   group_by(year) %>% 
#   summarise(cumul_drain_km2 = sum(cumul_drain_km2)) %>% 
#   ungroup() %>% 
#   mutate(lossrate = (cumul_drain_km2 - lag(cumul_drain_km2)) / 10)
# 


# /----------------------------------------------------------------------------#
#/     Bar plot of LU drain or conversion over time                       ------
fig2c_barplot_lutype <-
  
  ggplot(dlu) +
  
  geom_area(aes(x=year, y=cumul_drain_km2/10^6, fill=dlu$lu_type_fac), 
            color=NA, size=0.03, alpha=0.9, position='stack') +
  
  scale_y_continuous(expand=c(0,0),
                     limits= c(0, 2.5), 
                     labels = seq(0,  2, 0.5), 
                     breaks = seq(0,  2, 0.5)) +
  
  scale_x_continuous(expand=c(0,0), 
                     labels = seq(1700,2000,100), 
                     breaks= seq(1700,2000,100)) +
  
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
        'Wetland Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
        'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
        'Peatland' = '#ff7f00',#'brown',    # Peatland
        'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
        'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
        'Urban'    = '#a65628'),  #e0dd6e'), # Urban
    name="Driver of\nwetland loss") +
  
  ylab(expression(paste("Drained or converted area (10"^{6},' km'^{2}," )" ))) +
  xlab("") +
  line_plot_theme +
  theme(legend.position = c(0.02, 0.65)) 


fig2c_barplot_lutype
