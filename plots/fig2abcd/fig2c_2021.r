# /----------------------------------------------------------------------------#
#/ Barplot of value types in the time-series           ------
# Fig 2 C - Drain LU type stacked-area plot


#  Read in data
f <- paste0('../output/results/wetloss/sum/sum_drain_perlu_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '_v1.csv')

dlu <- read.csv(f) %>% 
       dplyr::select(-X)  %>% 
      distinct() %>% 
  mutate(lu_type= stri_trans_totitle(lu_type)) %>% 
  mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
         lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
         lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
         lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
         lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type))  


# dlu$lu_type_fac <- factor(dlu$lu_type, levels=c('Cropland',
#                                                 'Forestry',
#                                                 'Pasture',
#                                                 'Peat Extraction',
#                                                 'Urban',
#                                                 'Rice',
#                                                 'Wetland Cultiv.'))

dlu$lu_type_fac <- factor(dlu$lu_type, levels=c('Cropland',
                                                'Forestry',
                                                'Peat Extraction',
                                                'Wetland Cultiv.',
                                                'Pasture',
                                                'Urban',
                                                'Rice'
                                                ))

# Reordering the data
dlu <- arrange(dlu, lu_type_fac, year)


# /----------------------------------------------------------------------------#
#/     Bar plot of LU drain or conversion over time                       ------
fig2c <-
  
  ggplot(dlu) +
  
  geom_area(aes(x=year, y=cumul_drain_km2/10^6, fill=lu_type_fac), 
            color=NA, size=0.03, alpha=0.9, position='stack') +
  
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 4),
                     labels = seq(0,  4, 0.5),
                     breaks = seq(0,  4, 0.5)) +
  
  scale_x_continuous(expand=c(0,0), 
                     labels = seq(1700,2020,100), 
                     breaks= seq(1700,2020,100)) +
  
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#CC3311',
        'Wetland Cultiv.' = '#33BBEE',
        'Forestry'   = '#228833',
        'Peat Extraction' = '#EE7733',
        'Rice'     = '#AA4499',
        'Pasture'  = '#DDCC77',
        'Urban'    = '#332288'),
    name="Driver of\nwetland loss") +
  
  ylab(expression(paste("Drained or converted area (10"^{6},' km'^{2}," )" ))) +
  xlab("") +
  line_plot_theme +
  theme(legend.position = c(0.02, 0.65)) 


fig2c



# c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
#   'Wetland Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
#   'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
#   'Peat Extraction' = '#ff7f00',#'brown',    # Peatland
#   'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
#   'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
#   'Urban'    = '#a65628'),  #e0dd6e'), # Urban