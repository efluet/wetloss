# Make map of generalized regions

# Prepare country polygons
countries_robin@data$id <- rownames(countries_robin@data)
countries_robin_df <- fortify(countries_robin, region='id')
countries_robin_df <- merge(countries_robin_df, countries_robin@data, by="id")
countries_robin_df <- countries_robin_df[order(countries_robin_df$order), ] 


countries_robin_df <- countries_robin_df %>%
mutate(continent = countrycode(admin, "country.name", "region")) %>%
  mutate(continent = ifelse(continent %in% c("Central America","South America", "Caribbean"),"Central & South America",continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Europe","Southern Europe", "Northern Europe"), "Western Europe", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Asia","Central Asia"), "Western & Central Asia", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Northern America"), "North America", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Africa", "Southern Africa","Eastern Africa","Middle Africa"), "Africa", continent))

# /----------------------------------------------------------------------------#
#/    Cropland map
sig_param_map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  # geom_polygon(data=countries_robin_df,
  #              aes(long, lat, group=group), fill='grey90', color='white', size=0.1) +
  
  # add data countries
  geom_polygon(data=subset(countries_robin_df, !is.na(continent)), 
               aes(long, lat, group=group, fill=continent), color='black', size=0.1) +  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, 
            aes(long, lat, group=group), color="black", size=0.1) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_brewer(direction=1, palette = 'Set2') +
  
  theme(legend.position="bottom",
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_legend(raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Regions of simoid\nparameters generalization"))


sig_param_map

# /----------------------------------------------------------------------------#
#/    Save figure to file                                               --------

ggsave(paste0('../output/figures/artif_drainage/map/map_sig_params_continents_v2.png'), 
       sig_param_map,
       width=190, height=100, dpi=600, units='mm')
dev.off()
