# description:  map drainage as percentage of country --------------------------
#==============================================================================#

# read drainage shp
artdrain_forest_natpoly <- readRDS("./output/results/artif_drainage/artdrain_nat_poly_forestry.rds")

# Fortify & reproject polygons to ggplot-mappable df 
artdrain_forest_natpoly_df <- prep_poly_into_robin_map_wdata(artdrain_forest_natpoly)


#==============================================================================#
# make ggplot map --------------------------------------------
#==============================================================================#

map <- 
  
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey85') +
  
  # add data countries
  geom_polygon(data=artdrain_forest_natpoly_df, 
               aes(long, lat, group=group, fill= fraction_drained*100), alpha=1) +
  
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 2) +
  theme(legend.position="top") +
  theme(plot.margin = unit(c(-2,-3,-2,-10), "mm")) +
  guides(fill = guide_colorbar(barwidth = 14, barheight = 0.5)) +
  
  labs(fill = "Secondary forest % drained")


map



### save figure to file  ==================================================

ggsave('./output/figures/artif_drainage/map/perc_drained_forestry.png',  
       width=178, height=90, dpi=600, units="mm")#, type = "cairo-png")
dev.off()

