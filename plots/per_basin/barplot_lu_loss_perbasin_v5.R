# Description: Plot wetloss per watershed

# /----------------------------------------------------------------------------#
#/   Get peatland regions
source('./data_proc/wettype/regionalize_peatmap.r')

# /----------------------------------------------------------------------------#
#/  PREP RIVER BASINS
source('./data_proc/wettype/prep_riv_basin.r')




# /-------------------------------------------------------------
#/ Function extracting LU
get_basin_wetloss_lu <- function(endyear) {
  
  # get 7 drainage rasters for year 2000
  stack2000 <- drain_Mkm2_stack[[grep(pattern=endyear, names(drain_Mkm2_stack))]]
  
  names(stack2000) <- c('Rice', 'Wetland Cultiv.', 'Cropland', 
                        'Urban', 'Pasture', 'Peat', 'Forestry')
  
  names(pres_wet) <- 'preswet'
  stack2000 <- stack(stack2000, pres_wet)

  #/ Zonal stat of potential wetland per basin
  z_ludrivers <- as.data.frame(zonal(stack2000, rivbasin, 'sum'))
  

  #/    Join Zonal stat and calculate % loss
  perbasin <- z_ludrivers %>%
    
    # calculate sum of drainage in basin as the rowwise sum of columns 
    mutate(sum_drain_area = rowSums(.[2:8])) %>% 
    
    # convert to long format
    gather(key = "lu", value = "lu_drain_area", Rice:Forestry) %>%
    
    mutate(lu_perc_loss = lu_drain_area / (preswet + sum_drain_area) * 100,
           tot_perc_loss =  (1 - (preswet / (preswet + sum_drain_area))) * 100) %>%
    
    # Join Basin name
    left_join(., rivbasindat[,c(1,2,6)], by=c("zone"="ID")) %>%
    
    # Select a suset of basins
    group_by(Name) %>%
    filter(BasinArea > 10^5) %>%
    top_n(20, wt=tot_perc_loss) %>%
    filter(!Name %in% c('Noname (GHAASBasin12)','Noname (GHAASBasin14)','Noname (GHAASBasin30)')) %>% 
    ungroup() %>% 
    
    mutate(endyear = endyear)
  
  return(perbasin)

}


peatmapdat <- data.frame(ID=seq(1,6),
                         Name=c('Amazon','Indonesia','Congo','Canada','Siberia','Northern Europe'))


# /-------------------------------------------------------------
#/ Function extracting LU
get_peatland_wetloss_lu <- function(endyear) {
  
  # get 7 drainage rasters for year 2000
  stack2000 <- drain_Mkm2_stack[[grep(pattern=endyear, names(drain_Mkm2_stack))]]
  
  names(stack2000) <- c('Rice', 'Wetland Cultiv.', 'Cropland', 
                        'Urban', 'Pasture', 'Peat', 'Forestry')
  
  names(pres_wet) <- 'preswet'
  stack2000 <- stack(stack2000, pres_wet)
  

  #/ Zonal stat of potential wetland per basin
  z_ludrivers <- as.data.frame(zonal(stack2000, peatmap, 'sum'))
  

  #/    Join Zonal stat and calculate % loss
  perbasin <- z_ludrivers %>%
    # calculate sum of drainage in basin as the rowwise sum of columns 
    mutate(sum_drain_area = rowSums(.[2:8])) %>% 
    
    # convert to long format
    gather(key = "lu", value = "lu_drain_area", Rice:Forestry) %>%
    
    mutate(lu_perc_loss = lu_drain_area / (preswet + sum_drain_area) * 100,
           tot_perc_loss =  (1 - (preswet / (preswet + sum_drain_area))) * 100) %>%
    
    # Join Basin name
    left_join(., peatmapdat, by=c("zone"="ID")) %>%
    
    mutate(endyear = endyear)
  
  return(perbasin)
  
}


# /----------------------------------------------------------------------------#
#/   Get PEAT REGIONS
# source('regionalize_peatmap.r')
# peatmap <- raster('../output/results/wettype/peatmap_region.tif')




basin_lu_loss_df <- data.frame()
basin_wetloss_ci_df <- data.frame()

peat_lu_loss_df <- data.frame()


# for (y in c('1700','1750','1800','1850','1900','1950','2000')){
for (y in seq(1700,2000, 10)){
  
  y = as.character(y)
  print(y)
  
  # Get lu
  perbasin <- get_basin_wetloss_lu(y)
  basin_lu_loss_df <- bind_rows(basin_lu_loss_df, perbasin)
  
  perbasin <- get_wetloss_perc(y)
  basin_wetloss_ci_df <- bind_rows(basin_wetloss_ci_df, perbasin)
  
  
  # Peat LU
  perpeatregion <- get_peatland_wetloss_lu(y)
  peat_lu_loss_df <- bind_rows(peat_lu_loss_df, perpeatregion)
  
  
  
  }




# Subset basins to a few selected
basin_lu_loss_df_sel <- basin_lu_loss_df %>% 
  filter(Name %in% c('Yangtze','Indus','Mississippi', 'Danube', 'Tigris & Euphrates', 'Ganges'))

basin_wetloss_ci_df_sel <- basin_wetloss_ci_df %>% 
  filter(Name %in% c('Yangtze','Indus','Mississippi', 'Danube', 'Tigris & Euphrates', 'Ganges'))

# 'Nile', 'Yellow', 'St. Lawrence', 'St. Lawrence'


basin_lu_loss_df_sel$endyear <- as.numeric(basin_lu_loss_df_sel$endyear)
basin_wetloss_ci_df_sel$endyear <- as.numeric(basin_wetloss_ci_df_sel$endyear)
peat_lu_loss_df$endyear <- as.numeric(peat_lu_loss_df$endyear)




# /-----------------------------------------------------------------------------
#/ Plot
basinlossplot <- ggplot() +

  # geom_bar(data=basin_lu_loss_df_sel, 
  #          aes(x=endyear,#reorder(Name, lu_perc_loss), 
  #              y=lu_perc_loss, fill=lu), width=0.88, 
  #          position='stack', stat="identity") +

  geom_area(data=basin_lu_loss_df_sel, 
           aes(x=endyear,#reorder(Name, lu_perc_loss), 
               y=lu_perc_loss, fill=lu), width=0.88, 
           position='stack', stat="identity") +
  
  line_plot_theme +
  
  scale_y_continuous(expand= c(0,0), limits = c(0,100)) +
  scale_x_continuous(expand= c(0,0))+
  xlab("") +  ylab(expression(paste("Wetland lost (%)"))) +
  
  facet_wrap(~Name, nrow=1, scales = "free_x") +  # , strip.position='bottom'

  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
        'Wetland.Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
        'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
        'Peat' = '#ff7f00',#'brown',    # Peatland
        'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
        'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
        'Urban'    = '#a65628'),  #e0dd6e'), # Urban
    name="Driver of\nwetland loss") +
  

  theme(#axis.line.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=-90, vjust=0.5),
        legend.text = element_text(size = 6),
        panel.grid.major.y = element_line(color="grey80", size=0.15),
        legend.position = 'none',#c(0.2, 0.8 ),
        panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text = element_text(size=8, face='bold',hjust= 0.5),
        plot.margin=unit(c(0, 20, 0, 2), 'mm')) #, vjust = -0.5),)



# /-----------------------------------------------------------------------------
#/ Plot  PEATLAND LOSS
peatlossplot <- ggplot() +
  

  geom_area(data=peat_lu_loss_df, 
            aes(x=endyear,#reorder(Name, lu_perc_loss), 
                y=lu_perc_loss, fill=lu), width=0.88, 
            position='stack', stat="identity") +
  
  line_plot_theme +
  
  
  scale_y_continuous(expand= c(0,0), limits = c(0,20)) +
  scale_x_continuous(expand= c(0,0))+
  
  xlab("") +  ylab(expression(paste("Wetland loss (%)"))) +
  
  facet_wrap(~Name, nrow=1, ncol=7, scales = "free_x", drop=F) +  # , strip.position='bottom'
  
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#e41a1c', #'#ff5b4f',  # Cropland
        'Wetland.Cultiv.' = '#377eb8',#'#507dc7',  # Wetcultiv - blue
        'Forestry'   = '#4daf4a', #'#8df0a9',  # Forestry
        'Peat' = '#ff7f00',#'brown',    # Peatland
        'Rice'     = '#984ea3',#'#a177e0',  # Irrig Rice
        'Pasture'  = '#ffff33',#'#95f5f0',  # Pasture
        'Urban'    = '#a65628'),  #e0dd6e'), # Urban
    name="Driver of\nwetland loss") +


theme(#axis.line.x = element_blank(),
  # axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_text(angle=-90, vjust=0.5),
  legend.text = element_text(size = 6),
  panel.grid.major.y = element_line(color="grey80", size=0.15),
  legend.position = c(0.2, 0.8 ),
  panel.spacing = unit(0.5, "lines"),
  strip.placement = "outside",
  strip.text = element_text(size=8, face='bold',hjust= 0.5),
  plot.margin=unit(c(4, 20, -2, 2), 'mm')) #, vjust = -0.5),)



# /----------------------------------------------------------------------------#
#/ arrange plots grob into layout 
library(ggpubr)  #ggarrange



p <- plot_grid(basinlossplot, peatlossplot,
               
               ncol=1, nrow=2, 
               rel_heights = c(1, 1),
               rel_widths = c(1, 1),
               
               labels = c('A', 'B'),
               align='hv')

p



# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasinpeatregion_groupedbar_v2.png", p,
         width=180, height=100, dpi=400, units='mm' , type = "cairo-png")
dev.off()


ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasin_groupedbar_v2.pdf", p, 
         width=180, height=100, dpi=400, units='mm')
dev.off()
