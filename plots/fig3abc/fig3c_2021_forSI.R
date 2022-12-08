#  /----------------------------------------------------------------------------#
#/   Get regional wetloss data 
histcases <- readRDS("../data/hist_records/davidson_sites_gis/histcases_wdata_2021.rds") # Updated April 2021

# exclude polygons without a start date 
histcases <- histcases[!is.na(histcases$yr_start_rnd),]

# REmove certain polygons; to remove any overlap
histcases <- histcases[!histcases$src_id %in% c(31, 92, 129, 104, 136, 144),]

# Calculate area of polygons from m^2  to  million km2
histcases$areapoly_mkm2 <- areaPolygon(histcases) /10^5 /10^6

#  /----------------------------------------------------------------------------#
#/      Read WET index csv of point locations

wetindex_df <-  
  read.csv('../output/results/histcases/WETindex_cases_wmappedwetloss1700to1970.csv') %>% 
  filter(Ramsar.type != 'Human-made') %>% 
  filter(! Land.cover..from.paper. %in%  c('Seagrass','Mangroves', 'Oyster reef'))

# Convert to points
xy <- wetindex_df[,c("Longitude","Latitude")]   

wetindex_pts <- SpatialPointsDataFrame(coords = xy, data = wetindex_df, 
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# /----------------------------------------------------------------------------#
#/  Get gridded reconstructed loss during 1700-2020

ks_df <- data.frame()

pars='avg'
# Matrix facet plot
for (s_i in c(1,2,3,4)) {
  for (p_i in c(1,2,3)) {
    
    print(paste(s_i, p_i))
    
  
  f <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i, '_t', test_theta,'_', pars, '.csv')
  # Cacluate % loss per pixel
  grid_remwet_peryear <- 
    read.csv(f) %>% 
    bind_cols(., ciso_df) %>% 
    bind_cols(., landarea_df) %>%  
    mutate(cumloss_perc = (X1700 - X2020)/X1700*100) %>%  
    mutate(cumloss_perc = ifelse(cumloss_perc>100, 100, cumloss_perc),
           cumloss_perc = ifelse(cumloss_perc<0, 0, cumloss_perc)) %>%
    as_tibble() %>% 
    mutate(Fwet1700 = X1700 / landarea) %>% 
    dplyr::select(x, y, Fwet1700, cumloss_perc) %>% 
    # Remove pixels without present or potential wetland
    filter(Fwet1700 > 0.1)
  
  
  # Convert to raster
  r <- rasterFromXYZ(as.data.frame(grid_remwet_peryear)[, c('x', 'y', 'cumloss_perc')])
  
  
  # /----------------------------------------------------------------------------#
  #/  Extract reconstructed loss with the regional data
  
  # Extract to WET index points
  wetindex_df$cumloss_perc <- extract(r, wetindex_pts)
  
  # Extract region wetloss
  histcases_df <- extract(r,  histcases, fun=mean, na.rm=T, df=TRUE)
  
  
  
  # /----------------------------------------------------------------------------#
  #/   COMPUTE K-S TEST                                                 ----------
  # alternative = c("two.sided", "less", "greater"),
  
  # Between reconstruction & WETindex
  wetindex_row <- 
    tidy(ks.test(grid_remwet_peryear$cumloss_perc, wetindex_df$cumloss_perc,
          alternative = c("two.sided"), exact = NULL)) %>% 
    mutate(data='WET Index')
    
  
  # Between reconstruction & region
  region_row <- 
    tidy(ks.test(grid_remwet_peryear$cumloss_perc, histcases_df$cumloss_perc, 
          alternative = c("two.sided"), exact = NULL)) %>% 
    mutate(data='Regional data')
  
  ks_df <- bind_rows(ks_df, wetindex_row, region_row)
  
  }
}

# Calculate range of KS results
ks_df_rng <- 
  ks_df %>% 
  group_by(method, alternative, data) %>% 
  summarise_at(vars(statistic, p.value), .funs=list(min=min, mean=mean, max=max), na.rm=T)


# /----------------------------------------------------------------------------#
#/    Cumul plot  of mapped and case studies                            --------

fig3c_cumulplot <- 
  
  ggplot() +
  
  # Global background -----------------------------------
stat_bin(data=grid_remwet_peryear,
         aes(x=cumloss_perc, y=cumsum(..count..)/sum(..count..)),
         color='black', bins=500, geom='line') +
  
  geom_text(aes(x=16,y=.88), label='Wetland loss\nreconstruction\n(n~31,000)', color='black', size=2.7) +
  
  
  # WET-------------------------------
stat_bin(data=wetindex_df, # , mappedcumulloss>0 
         aes(x=cumloss_perc, y=cumsum(..count..)/sum(..count..)), 
         color='red', bins=500, geom='line') +
  
  geom_text(aes(x=80, y=0.7), label='Wetland Extent\nTrends (WET) Index\n(n=747)', color='red', size=2.7) +
  
  
  # Davidson  -----------------------------
stat_bin(data=histcases_df, #, map_perc_lost>0),
         aes(x=cumloss_perc, y=cumsum(..count..)/sum(..count..)),
         color='blue', bins=500, geom='line') +
  
  geom_text(aes(x=50, y=.45), label='Regional loss\n data (n=121)', color='blue', size=2.7) +
  
  line_plot_theme +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(0,100)) +
  #, expand=c(0,0)) +
  xlab('Mapped wetland loss 1700-2020 (%)') + 
  ylab('Cumulative density') +
  
  theme(panel.background = element_rect(color="black", size=0.5, fill=NA),
        plot.margin=unit(c(1, 1, 3, 1), 'mm'))


fig3c_cumulplot


### save plot ------------------------------------------------------------------
ggsave('../output/figures/hist_cases/cumul_wetloss_ks_v6.png',
       width=120, height=120, dpi=600, units='mm', type = 'cairo-png')

ggsave('../output/figures/hist_cases/cumul_wetloss_ks_v6.pdf',
       width=120, height=120, units='mm')

dev.off()



# cs_joined <- read.csv('../output/results/histcase_wetloss_joined.csv')
# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_v4_serialmcmc.csv')
# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_serialmcmcdf_v5.csv')


# #  /---------------------------------------------------------------------------#
# #/   Plot distribution of wetloss from the map                             -----
# 
# # Make function of plot
# histogram_dist_plot<- function(dfname, xname, fillcolor='white'){
#   
#   p <- ggplot() +
#     
#     ##  plot bars
#     geom_histogram(data=dfname, aes(x=dfname[[xname]], y=..density..), 
#                    binwidth=10, size=0.5,
#                    color='black', fill=fillcolor) +
#     
#     line_plot_theme +
#     coord_flip() +
#     scale_y_continuous(expand=c(0,0)) +
#     scale_x_continuous(limits=c(-100, 100)) +
#     
#     xlab('Mapped wetland loss (%)') + ylab('density') +
#     
#     theme(panel.border = element_rect(color='black', size=0.5),
#           legend.position = 'top')
#   
#   # return the plot
#   return(p)
# }
# 
# 
# # /----------------------------------------------------------------------------#
# #/ make histogram/density plot                      --------
# 
# # for background
# histogram_dist_plot(wetlossperc_1700_to_2000_df, 'layer', 'grey90')
# 
# # for Davidson et al case studies
# histogram_dist_plot(cs_joined, 'map_wetloss_prc_mean', 'lightblue')
# 
# # for WET index
# histogram_dist_plot(wetindex_df, 'wetloss_perc_from1700to1970', 'yellow')
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/    make histogram of mapped and case studies                    -------------
# ggplot() +
#   
#   geom_histogram(data=wetlossperc_1700_to_2000_df, 
#                  aes(x=layer, y=..count../sum(..count..)), 
#                  binwidth=10, size=0.1, color='black', fill='grey85') +
#   
#   # points
#   geom_histogram(data=cs_joined, 
#                  aes(x=map_wetloss_prc_mean, y=..count../sum(..count..)), 
#                  binwidth=10, size=0.1,
#                  color='black', fill='lightblue', alpha=0.5) +
#   
#   line_plot_theme +
#   coord_flip() +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(limits=c(-100, 100)) +
#   
#   xlab('Mapped wetland loss (%)') + ylab('density') +
#   
#   theme(panel.border = element_rect(color='black', size=0.5, fill=NA),
#         legend.position = 'top') 

# mutate(wetloss_perc_1700to1970 <- wetloss_perc_1700to1970 * -1) %>% 