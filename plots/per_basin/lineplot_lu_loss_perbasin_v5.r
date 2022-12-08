# /----------------------------------------------------------------------------#
#/   Get peatland regions
source('./data_proc/wettype/regionalize_peatmap.r')

# /----------------------------------------------------------------------------#
#/  PREP RIVER BASINS
source('./data_proc/wettype/prep_riv_basin.r')


# /-----------------------------------------------------------------------------
#/   Append regions to the remwet data

# Append columns of regions to remwet data
remwet_wettype <- bind_cols(remwet, peatmap_df, rivbasin_df)


# /-----------------------------------------------------------------------------
#/   Summarize per basin

remwet_perbasin <- remwet_wettype %>% 
                   filter(!is.na(basin_name)) %>% 
                   group_by(basin_name) %>% 
                   dplyr::select(basin_name, X1700:X2000) %>% 
                   summarize_all(.funs=sum, na.rm=T) %>% 
                   ungroup() %>%
                   pivot_longer(X1700:X2000, names_to='year', values_to='remwet_km2') %>%
                   mutate(year=as.numeric(substring(year,2,5))) %>% 
                   group_by(basin_name) %>% 
                   mutate(remwet_perc = remwet_km2/max(remwet_km2))


# Filter to only major basins 
# TODO: should use a specific criteria 
remwet_perbasin <- remwet_perbasin %>% 
                  filter(basin_name %in% c('Yangtze','Indus','Mississippi', 'Niger',
                                           'Murray','Nile','St. Lawrence','Mekong','Danube', 
                                           'Tigris & Euphrates', 'Syr-Darya','Ganges'))

# /-----------------------------------------------------------------------------
#/   Summarize per peatland region
remwet_perpeat <- remwet_wettype %>% 
                    filter(!is.na(peat_name)) %>% 
                    group_by(peat_name) %>% 
                    dplyr::select(peat_name, X1700:X2000) %>% 
                    summarize_all(.funs=sum, na.rm=T) %>% 
                    ungroup() %>%
                    pivot_longer(X1700:X2000, names_to='year', values_to='remwet_km2') %>%
                    mutate(year=as.numeric(substring(year,2,5))) %>% 
                    group_by(peat_name) %>% 
                    mutate(remwet_perc = remwet_km2/max(remwet_km2))


# /--------------------------------------
#/  Make peat lineplot
remwet_perc_perpeat_plot <- 
  
  ggplot(remwet_perpeat) +
  
  # add lines 
  geom_line(aes(x=year, y=remwet_perc*100, color=peat_name), size=0.35) +
  
  
  geom_text_repel(data = subset(remwet_perpeat, year==2000),
                  aes(x=year+25, y=remwet_perc*100, label = peat_name, color=peat_name),
                  segment.color='grey25',
                  size = 3.0,
                  nudge_x = 10,
                  segment.size = 0.25,
                  box.padding = unit(0.1, 'mm'),
                  point.padding = unit(0.1, 'mm')) +
  # axes limit
  xlab("")  + ylab("Wetland area percentage change since 1700 (%)") +
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2040)) +
  scale_y_continuous(limits=c(30, 100))+
  #scale_color_brewer(name="Wetland area (% of 1700 area)", palette = "Set1")+
  
  ggtitle('Peatland regions')+
  line_plot_theme +
  theme(legend.position = 'none')#c(0.1, 0.3))


remwet_perc_perpeat_plot


# /----------------------------------------------------------------------------#
#/  Make river basins                                                   --------

remwet_perc_perbasin_plot <- 

  ggplot(remwet_perbasin) +
  
  # add lines 
  geom_line(aes(x=year, y=remwet_perc*100, color=basin_name), size=0.35) +
  
  # make multiple facets per lat slices
  # facet_wrap(~type, nrow=1, scales="free_y") +
  xlab("")  + ylab("") +
  
  geom_text_repel(data = subset(remwet_perbasin, year==2000),
                  aes(x=year+25, y=remwet_perc*100, label = basin_name, color=basin_name),
                  segment.color='grey25',
                  size = 3.0,
                  nudge_x = 10,
                  segment.size = 0.25,
                  box.padding = unit(0.1, 'mm'),
                  point.padding = unit(0.1, 'mm')) +
  # axes limit
  scale_x_continuous(expand=c(0,0), limits=c(1700, 2040)) +
  scale_y_continuous(limits=c(0, 100))+
  #scale_color_brewer(name="Wetland area (% of 1700 area)", palette = "Set1")+
  
  ggtitle('River basins')+
  line_plot_theme +
  theme(legend.position = 'none') # c(0.1, 0.3))


remwet_perc_perbasin_plot

# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
# arealossmap <- arealossmap + theme(plot.margin=unit(c(-14, -4.5, -9, 1), 'mm'))
# drivermap   <- drivermap   + theme(plot.margin=unit(c(-14, -4.5, -9, 1), 'mm'))
# 


# /----------------------------------------------------------------------------#
#/  Arrange plots grob into layout 

p <- plot_grid(remwet_perc_perpeat_plot, remwet_perc_perbasin_plot,
               ncol=1, nrow=2, 
               rel_heights = c(1, 1),
               rel_widths = c(1, 1),
               labels = c('A', 'B'),
               align='hv')



# SAVE
ggsave('../output/figures/wettype/remwet_perc_perpeatbasin_2021_v1.png', p,
       width=90, height=160, dpi=300, units='mm') #type = 'cairo-png')
dev.off()





# # /----------------------------------------------------------------------------#
# #/  Get LU drivers of loss           ------
# # drain_Mkm2_stack <- readRDS('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul_best.rds')
# 
# # /----------------------------------------------------------------------------#
# #/ Function extracting LU PER BASIN                                       ------
# get_basin_wetloss_lu <- function(endyear) {
#   
#   # get 7 drainage rasters for year 2000
#   stack2000 <- drain_Mkm2_stack[[grep(pattern=endyear, names(drain_Mkm2_stack))]]
#   
#   names(stack2000) <- c('Rice', 'Wetland Cultiv.', 'Cropland', 
#                         'Urban', 'Pasture', 'Peat', 'Forestry')
#   
#   names(pres_wet) <- 'preswet'
#   stack2000 <- stack(stack2000, pres_wet)
# 
#   #/ Zonal stat of potential wetland per basin
#   z_ludrivers <- as.data.frame(zonal(stack2000, rivbasin, 'sum'))
#   
#   #/    Join Zonal stat and calculate % loss
#   perbasin <- z_ludrivers %>%
#     
#     # calculate sum of drainage in basin as the rowwise sum of columns 
#     mutate(sum_drain_area = rowSums(.[2:8])) %>% 
#     
#     # convert to long format
#     gather(key = "lu", value = "lu_drain_area", Rice:Forestry) %>%
#     
#     mutate(lu_perc_loss = lu_drain_area / (preswet + sum_drain_area) * 100,
#            tot_perc_loss =  (1 - (preswet / (preswet + sum_drain_area))) * 100) %>%
#     
#     # Join Basin name
#     left_join(., rivbasindat[,c(1,2,6)], by=c("zone"="ID")) %>%
#     
#     # Select a suset of basins
#     group_by(Name) %>%
#     filter(BasinArea > 10^5) %>%
#     top_n(20, wt=tot_perc_loss) %>%
#     filter(!Name %in% c('Noname (GHAASBasin12)','Noname (GHAASBasin14)','Noname (GHAASBasin30)')) %>% 
#     ungroup() %>% 
#     
#     mutate(endyear = endyear)
#   
#   return(perbasin)
# 
# }
# 
# 
# peatmapdat <- data.frame(ID=seq(1,6),
#                          Name=c('Amazon','Indonesia','Congo','Canada','Siberia','Northern Europe'))
# 
# 
# # /----------------------------------------------------------------------------#
# #/ Function extracting LU PER PEATLAND REGION                ----
# get_peatland_wetloss_lu <- function(endyear) {
#   
#   # get 7 drainage rasters for year 2000
#   stack2000 <- drain_Mkm2_stack[[grep(pattern=endyear, names(drain_Mkm2_stack))]]
#   
#   names(stack2000) <- c('Rice', 'Wetland Cultiv.', 'Cropland', 
#                         'Urban', 'Pasture', 'Peat', 'Forestry')
#   
#   names(pres_wet) <- 'preswet'
#   stack2000 <- stack(stack2000, pres_wet)
#   
# 
#   #/ Zonal stat of potential wetland per basin
#   z_ludrivers <- as.data.frame(zonal(stack2000, peatmap, 'sum'))
#   
# 
#   #/    Join Zonal stat and calculate % loss
#   perbasin <- z_ludrivers %>%
#     # calculate sum of drainage in basin as the rowwise sum of columns 
#     mutate(sum_drain_area = rowSums(.[2:8])) %>% 
#     
#     # convert to long format
#     gather(key = "lu", value = "lu_drain_area", Rice:Forestry) %>%
#     
#     mutate(lu_perc_loss = lu_drain_area / (preswet + sum_drain_area) * 100,
#            tot_perc_loss =  (1 - (preswet / (preswet + sum_drain_area))) * 100) %>%
#     
#     # Join Basin name
#     left_join(., peatmapdat, by=c("zone"="ID")) %>%
#     
#     mutate(endyear = endyear)
#   
#   return(perbasin)   }
# 
# 
# # /----------------------------------------------------------------------------#
# #/   Get PEAT REGIONS
# # source('regionalize_peatmap.r')
# # peatmap <- raster('../output/results/wettype/peatmap_region.tif')
# 
# 
# basin_lu_loss_df <- data.frame()
# basin_wetloss_ci_df <- data.frame()
# peat_lu_loss_df <- data.frame()
# 
# 
# for (y in seq(1700, 2000, 10)){
#   
#   y = as.character(y)
#   print(y)
#   
#   # Get lu
#   perbasin <- get_basin_wetloss_lu(y)
#   basin_lu_loss_df <- bind_rows(basin_lu_loss_df, perbasin)
#   
#   perbasin <- get_wetloss_perc(y)
#   basin_wetloss_ci_df <- bind_rows(basin_wetloss_ci_df, perbasin)
#   
#   
#   # Peat LU
#   perpeatregion <- get_peatland_wetloss_lu(y)
#   peat_lu_loss_df <- bind_rows(peat_lu_loss_df, perpeatregion)
#   
#   }
# 
# 
# 
# 
# # Subset basins to a few selected
# basin_lu_loss_df_sel <- basin_lu_loss_df %>% 
#   filter(Name %in% c('Yangtze','Indus','Mississippi', 'Danube', 'Tigris & Euphrates', 'Ganges'))
# 
# basin_wetloss_ci_df_sel <- basin_wetloss_ci_df %>% 
#   filter(Name %in% c('Yangtze','Indus','Mississippi', 'Danube', 'Tigris & Euphrates', 'Ganges'))
# # 'Nile', 'Yellow', 'St. Lawrence', 'St. Lawrence'
# 
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/   BASIN PLOT                                                 ------
# basinlossplot <- ggplot() +
# 
#   geom_bar(data=basin_lu_loss_df_sel, 
#            aes(x=endyear,#reorder(Name, lu_perc_loss), 
#                y=lu_perc_loss, fill=lu), width=0.88, 
#            position='stack', stat="identity") +
# 
#   line_plot_theme +
#   
#   scale_y_continuous(expand= c(0,0), limits = c(0,100)) +
# 
#   xlab("") +  ylab(expression(paste("Wetland lost (%)"))) +
#   
#   facet_wrap(~Name, nrow=1, scales = "free_x") +  # , strip.position='bottom'
# 
#   scale_fill_manual(#labels = driver_names,
#     values =
#       c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
#         'Wetland.Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
#         'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
#         'Peat' = '#ff7f00',#'brown',    # Peatland
#         'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
#         'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
#         'Urban'    = '#a65628'),  #e0dd6e'), # Urban
#     name="Driver of\nwetland loss") +
#   
# 
#   theme(#axis.line.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(angle=-90, vjust=0.5),
#         legend.text = element_text(size = 6),
#         panel.grid.major.y = element_line(color="grey80", size=0.3),
#         legend.position = 'none',#c(0.2, 0.8 ),
#         panel.spacing = unit(0.5, "lines"),
#         strip.placement = "outside",
#         strip.text = element_text(size=8, face='bold',hjust= 0.5)) #, vjust = -0.5),)
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/  PEATLAND LOSS PLOT                                    ------
# peatlossplot <- ggplot() +
#   
#   geom_bar(data=peat_lu_loss_df, 
#            aes(x=endyear, #reorder(Name, lu_perc_loss), 
#                y=lu_perc_loss, fill=lu), width=0.88, 
#            position='stack', stat="identity") +
#   
#   
#   # geom_point(data=basin_wetloss_ci_df_sel,
#   #            aes(x=endyear,#reorder(Name, lu_perc_loss), 
#   #                y=best), size=1) +
#   # 
#   # coord_flip() + 
#   line_plot_theme +
#   
#   
#   scale_y_continuous(expand= c(0,0), limits = c(0,20)) +
#   
#   xlab("") +  ylab(expression(paste("Wetland loss (%)"))) +
#   
#   facet_wrap(~Name, nrow=1, ncol=7, scales = "free_x", drop=F) +  # , strip.position='bottom'
#   
#   scale_fill_manual(#labels = driver_names,
#     values =
#       c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
#         'Wetland.Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
#         'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
#         'Peat' = '#ff7f00',#'brown',    # Peatland
#         'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
#         'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
#         'Urban'    = '#a65628'),  #e0dd6e'), # Urban
#     name="Driver of\nwetland loss") +
# 
# 
# theme(#axis.line.x = element_blank(),
#   axis.ticks.x = element_blank(),
#   axis.ticks.y = element_blank(),
#   axis.text.x = element_text(angle=-90, vjust=0.5),
#   legend.text = element_text(size = 6),
#   panel.grid.major.y = element_line(color="grey80", size=0.3),
#   legend.position = c(0.2, 0.8 ),
#   panel.spacing = unit(0.5, "lines"),
#   strip.placement = "outside",
#   strip.text = element_text(size=8, face='bold',hjust= 0.5)) #, vjust = -0.5),)
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/ arrange plots grob into layout                                       ------
# library(ggpubr)  #ggarrange
# p <- plot_grid(basinlossplot, peatlossplot,
#                
#                ncol=1, nrow=2, 
#                rel_heights = c(1, 1),
#                rel_widths = c(1, 1),
#                
#                labels = c('A', 'B'),
#                align='hv')
# 
# p
# 
# 
# 
# # /----------------------------------------------------------------------------#
# #/    Save plot 
# ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasinpeatregion_groupedbar.png", p,
#          width=180, height=100, dpi=400, units='mm' , type = "cairo-png")
# dev.off()
# 
# 
# ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasin_groupedbar.pdf", p, 
#          width=180, height=100, dpi=400, units='mm')
# dev.off()
