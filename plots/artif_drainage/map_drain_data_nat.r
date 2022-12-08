

# Get data points for Wetland cultiv 
wc <- read.csv("../output/results/artif_drainage/drained_wetcult_km2_onlydata.csv") %>% as_tibble() %>% 
  rename(drained_area_tot = drained_area_irrig) %>% 
  mutate(type = "Wetland Cultiv.") %>% 
  mutate(type = str_to_title(type))


# Get datapoints for drainage (highly processed)
d <- read.csv('../output/results/artif_drainage/drained_data_fullproc_forfig1.csv', stringsAsFactors=F)
# Filter to only keep national data
d_nat <- d %>% filter(region=='')

# combine wetcultiv & drainage
d_wc_forplot <- 
  bind_rows(d_nat, wc) %>% as_tibble() %>% 
  mutate(iso_a3 = countrycode(country_name,'country.name','iso3c',warn=F)) %>% 
  group_by(type, iso_a3) %>% 
  summarise(numdatapts = n()) %>% ungroup()
  

# Prepare country polygons
countries_robin@data$id <- rownames(countries_robin@data)
countries_robin_df <- fortify(countries_robin, region='id')
countries_robin_df <- merge(countries_robin_df, countries_robin@data, by="id")
countries_robin_df <- countries_robin_df[order(countries_robin_df$order), ] 

# Per type
countries_robin_cropland <- merge(countries_robin_df, d_wc_forplot %>% filter(type=='Cropland'), by='iso_a3', all.x=F) #%>% fortify()
countries_robin_cropland <- countries_robin_cropland[order(countries_robin_cropland$order), ] 

countries_robin_forestry <- merge(countries_robin_df, d_wc_forplot %>% filter(type=='Forestry'), by='iso_a3', all.x=F) #%>% fortify()
countries_robin_forestry <- countries_robin_forestry[order(countries_robin_forestry$order), ] 

countries_robin_peatextr <- merge(countries_robin_df, d_wc_forplot %>% filter(type=='Peat Extraction'), by='iso_a3', all.x=F) #%>% fortify()
countries_robin_peatextr <- countries_robin_peatextr[order(countries_robin_peatextr$order), ] 

countries_robin_wetcultiv <- merge(countries_robin_df, d_wc_forplot %>% filter(type=='Wetland Cultiv.'), by='iso_a3', all.x=F) #%>% fortify()
countries_robin_wetcultiv <- countries_robin_wetcultiv[order(countries_robin_wetcultiv$order), ] 


# /----------------------------------------------------------------------------#
#/    Cropland map
cropland_nat_map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey90', color='white', size=0.1) +
  
  # add data countries
  geom_polygon(data=countries_robin_cropland, 
               aes(long, lat, group=group, fill=numdatapts), color='black', size=0.1) +  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, 
            aes(long, lat, group=group), color="black", size=0.1) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 'Reds', breaks=c(1,5,10,15,20,25)) +

  theme(legend.position="bottom",
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  ggtitle('Cropland') +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "No. of national\ndata points"))

# /----------------------------------------------------------------------------#
#/    Forestry
forestry_nat_map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey90', color='white', size=0.1) +
  
  # add data countries
  geom_polygon(data=countries_robin_forestry, 
               aes(long, lat, group=group, fill=numdatapts), color='black', size=0.1) +  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 'Greens', breaks=c(1,5,8)) +
  
  theme(legend.position="bottom",
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  ggtitle('Forestry') +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "No. of national\ndata points"))

# /----------------------------------------------------------------------------#
#/    Peat extraction
peatextr_nat_map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey90', color='white', size=0.1) +
  
  # add data countries
  geom_polygon(data=countries_robin_peatextr,  aes(long, lat, group=group, fill=numdatapts), color='black', size=0.1) +  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 'Oranges', breaks=c(1,5,10,20, 30)) +
  
  theme(legend.position="bottom",
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  ggtitle('Peat extraction') +
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "No. of national\ndata points"))

# /----------------------------------------------------------------------------#
#/   Wetland Cultiv.
wetcultiv_nat_map <- 
  
  # set common parameters
  ggplot(bbox_robin_df, aes(long, lat)) + 
  
  #  Add country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey90', color='white', size=0.1) +
  
  # add data countries
  geom_polygon(data=countries_robin_wetcultiv, 
               aes(long, lat, group=group, fill=numdatapts), color='black', size=0.1) +  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, 
            aes(long, lat, group=group), color="black", size=0.1) +
  
  
  coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 'Blues', breaks=c(1,5,6)) +
  
  theme(legend.position="bottom",
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "No. of national\ndata points")) +
  ggtitle('Wetland Cultivation') 


# /----------------------------------------------------------------------------#
#/     arrange plots grob into layout 
nat_map <- plot_grid(cropland_nat_map, forestry_nat_map, 
                  peatextr_nat_map, wetcultiv_nat_map,
                  ncol=2, nrow=2, 
                  align='hv')

# /----------------------------------------------------------------------------#
#/    Save figure to file                                               --------

ggsave(paste0('../output/figures/artif_drainage/map/map_nat_drain_datapts_v8.pdf'), 
       nat_map,
       width=190, height=140, dpi=600, units='mm')

ggsave(paste0('../output/figures/artif_drainage/map/map_nat_drain_datapts_v8.png'), 
       nat_map,
       width=190, height=140, dpi=600, units='mm')


