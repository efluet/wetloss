# Description: Plot wetloss per watershed


# /----------------------------------------------------------------------------#
#/  Get LU drivers of loss

drain_Mkm2_stack <- readRDS('../output/results/wetloss/grid/wetloss_bydriver_stack_0.5deg_serial_cumul_best.rds')

# get 7 drainage rasters for year 2000
stack2000 <- drain_Mkm2_stack[[grep(pattern='2000', names(drain_Mkm2_stack))]]

names(pres_wet) <- 'preswet'
stack2000 <- stack(stack2000, pres_wet)


# /----------------------------------------------------------------------------#
#/   Get wetloss area
# remwet_Mkm2_stack <- readRDS('../output/results/wetloss/grid/remwet_tot_stack_0.5deg_serialbestvar.rds')
# 
# 
# remwet2000 <- remwet_Mkm2_stack[[grep(pattern='2000', names(remwet_Mkm2_stack))]]
# remwet1700 <- remwet_Mkm2_stack[[grep(pattern='1700', names(remwet_Mkm2_stack))]]
# # sum_loss2000 <- remwet1700 - remwet2000



# /----------------------------------------------------------------------------#
#/   RIVER BASINS                                                    ------

# Get CSV of riverbasin data
rivbasindat <- read.csv("../data/ISLSCP_RIVER_ROUTING_1005/data/stn_basin_attribute_hd.csv") %>% 
  mutate(Name= ifelse(Name=='Chang Jiang', 'Yangtze', Name),
         Name= ifelse(Name=='Huang He', 'Yellow', Name)) %>% 
  rename_('ID'="ï..ID")

  



#/  Get river basin IDs
rivbasin <- raster("../data/ISLSCP_RIVER_ROUTING_1005/data/stn_basin_id_hd.asc")
rivbasin[rivbasin > 35] <- NA
rivbasin[rivbasin < 0] <- NA
# rivbasin[rivbasin != 15] <- NA

# rivbasin <- disaggregate(rivbasin, fact=2, method='')
plot(rivbasin)

# /----------------------------------------------------------------------------#
#/ Zonal stat of potential wetland per basin

z_ludrivers <- as.data.frame(zonal(stack2000, rivbasin, 'sum'))

# z_pres_wet <- as.data.frame(zonal(pres_wet,  rivbasin, 'sum'))
# names(zstatout)<-c("zone","sum_pres_wet")


# /----------------------------------------------------------------------------#
#/    Join Zonal stat and calculate % loss
perbasin <- z_ludrivers %>%
  # calculate sum of drainage in basin as the rowwise sum of columns 
  mutate(sum_drain_area = rowSums(.[2:8])) %>% 
  
  # convert to long format
  gather(key = "lu", value = "lu_drain_area", ir_rice_drained2000:forest_harv_drained2000) %>%
  
  mutate(lu_perc_loss = lu_drain_area / (preswet + sum_drain_area) * 100,
         tot_perc_loss =  (1 - (preswet / (preswet + sum_drain_area))) * 100) %>%
  
  # Join Basin name
  left_join(., rivbasindat[,c(1,2,6)], by=c("zone"="ID")) %>%
  
  # Select a suset of basins
  group_by(Name) %>%
  filter(BasinArea > 10^5) %>%
  top_n(20, wt=tot_perc_loss) %>%
  filter(!Name %in% c('Noname (GHAASBasin12)','Noname (GHAASBasin14)','Noname (GHAASBasin30)')) %>% 
  ungroup()


glimpse(perbasin)





# /-----------------------------------------------------------------------------
#/ Plot
ggplot(perbasin) +
  
  geom_bar(aes(x=reorder(Name, lu_perc_loss), 
               y=lu_perc_loss, fill=lu), width=0.7, stat="identity") +
  
  coord_flip() + 
  line_plot_theme +
  

  scale_y_continuous(expand= c(0,0), limits = c(0,100)) +
  
  xlab("") +  ylab(expression(paste("Percentage wetland lost (%)"))) +
  

  # scale_fill_manual(labels = driver_names,
  #                   values =
  #                     c('cropland_loss'  = '#ff5b4f',     # Cropland
  #                       'NA'  = '#507dc7', # Wetcultiv - blue
  #                       'forest_harv_loss'  = '#8df0a9',   # Forestry
  #                       'peatland_loss'  = 'brown',   # Peatland
  #                       'ir_rice_loss'  = '#a177e0',  # Irrig Rice
  #                       'pasture_loss'  = '#95f5f0',    # Pasture
  #                       'uopp__loss'  = '#e0dd6e'), # Urban
  #                   name="Driver of\nwetland loss") +
  
  
  #scale_fill_brewer(palette="Set1") +  
  
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color="grey80", size=0.4),
        legend.position = c(0.4, 0.3 ))

# labs(title = "Top 10 Wetland Loss Per Watershed",
#      subtitle = "",
#      caption = "Only basins > 10,000 km^2 original wetland area")


# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasin_v3_fixed.png",
         width=90, height=90, dpi=300, units='mm' , type = "cairo-png")
dev.off()


ggsave(  "../output/figures/per_basin/barplot_lu_loss_perbasin_v2.pdf",
         width=90, height=90, dpi=500, units='mm')
dev.off()
