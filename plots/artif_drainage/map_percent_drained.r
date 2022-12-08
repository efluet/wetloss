# /----------------------------------------------------------------------------#
#/ Convert polygons to df for plotting
# Using custom function
artdrain_type_nat_df <- prep_poly_into_robin_map_wdata(artdrain_type_nat)

# Assign colors to drainage type
# shouldn't this subset to the latest year of data?
if(draintype=="cropland"){ typecolor <- "Blues"}
if(draintype=="forestry"){ typecolor <- 2}
if(draintype=="peatland"){ typecolor <- 7}



# /----------------------------------------------------------------------------#
#/      Make map
map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey90') +
  
  # add data countries
  geom_polygon(data=artdrain_type_nat_df, 
               aes(long, lat, group=group, fill= f_drained*100)) +  

  # add country outline
  geom_path(data=countries_robin_df, 
            aes(long, lat, group=group), color='white', size=0.1) +
  
  
  geom_path(data=artdrain_type_nat_df,
            aes(long, lat, group=group), color="black", size=0.11) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, 
            aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = typecolor) +
  theme(legend.position="right",
        legend.direction = "vertical",
        plot.margin = unit(c(-2,-1,-2,-4), "mm")) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 15)) +
  
  labs(fill = paste0(draintype, "  % drained"))


#map

# /----------------------------------------------------------------------------#
#/    Save figure to file
outdirfig <- paste0('./output/figures/artif_drainage/map/perc_drained_', draintype,'_v6.png')
ggsave(outdirfig, width=178, height=90, dpi=600, units="mm")
#dev.off()
