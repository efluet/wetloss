# Make map of subnational units per LU type




# Get data points for Wetland cultiv 
wc <- read.csv("../output/results/artif_drainage/drained_wetcult_km2_onlydata.csv") %>% as_tibble() %>% 
  rename(drained_area_tot = drained_area_irrig) %>% 
  mutate(type = "Wetland Cultiv.") %>% 
  mutate(type = str_to_title(type))


# Get data points for drainage (highly processed)
d <- read.csv('../output/results/artif_drainage/drained_data_fullproc_forfig1.csv', stringsAsFactors=F)
# Filter to only keep national data
d_subnat <- d %>% filter(region!='')
d_nat <- d %>% filter(region!='')


# /-----------------------------------------------------
#/ Function to reprojects polygons and generates maps
subnat_robin_map_lu <- function(gadm1_lu_wgs84) {
  
  # Reproject
  gadm1_lu_wgs84_robin <- st_transform(gadm1_lu_wgs84, crs = st_crs(CRS('+proj=robin')))
  
  gadm1_lu_wgs84_robin <- gadm1_lu_wgs84_robin %>% 
    group_by(NAME_0) %>% 
    mutate(alpha_perc = (prc_sb_ - min(prc_sb_))/max(prc_sb_) * 100) %>% 
    mutate(iso_a3 = countrycode(NAME_0,'country.name','iso3c',warn=F))
  
  return(gadm1_lu_wgs84_robin)
}


# /----------------------------------------------------------------------------#
#/  Get subnat polygons
gadm1_cropland <- st_read(paste0('../output/results/artif_drainage/subnational/poly/subnat_perc_Cropland.shp')) %>% subnat_robin_map_lu()
gadm1_forestry <- st_read(paste0('../output/results/artif_drainage/subnational/poly/subnat_perc_Forestry.shp')) %>% subnat_robin_map_lu()
gadm1_peatextr <- st_read(paste0('../output/results/artif_drainage/subnational/poly/subnat_perc_Peat Extraction.shp')) %>% subnat_robin_map_lu()
gadm1_wetcultiv <- st_read(paste0('../output/results/artif_drainage/subnational/poly/subnat_perc_Wetland Cultiv..shp')) %>% subnat_robin_map_lu()




# Prepare country polygons
countries_robin@data$id <- rownames(countries_robin@data)
countries_robin_df <- fortify(countries_robin, region='id')
countries_robin_df <- merge(countries_robin_df, countries_robin@data, by="id")
countries_robin_df <- countries_robin_df[order(countries_robin_df$order), ] 

# Filter national polygon, down to those w/ subnat data
countries_robin_cropland <- countries_robin_df %>% filter(iso_a3 %in% gadm1_cropland$iso_a3)
countries_robin_cropland <- countries_robin_cropland[order(countries_robin_cropland$order), ] 

countries_robin_forestry <- countries_robin_df %>% filter(iso_a3 %in% gadm1_forestry$iso_a3)
countries_robin_forestry <- countries_robin_forestry[order(countries_robin_forestry$order), ] 

countries_robin_peatextr <- countries_robin_df %>% filter(iso_a3 %in% gadm1_peatextr$iso_a3)
countries_robin_peatextr <- countries_robin_peatextr[order(countries_robin_peatextr$order), ] 

countries_robin_wetcultiv <- countries_robin_df %>% filter(iso_a3 %in% gadm1_wetcultiv$iso_a3)
countries_robin_wetcultiv <- countries_robin_wetcultiv[order(countries_robin_wetcultiv$order), ] 

# /----------------------------------------------------------------------------#
#/ Make Cropland map
cropland_subnat_map <- 
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey93', color='grey70', size=0.05) +
  coord_sf(datum=st_crs(CRS('+proj=robin'))) +
  
  # Subnational units
  # geom_sf(data=gadm1_lu_wgs84_robin, aes(fill=country, alpha=alpha_perc), color='grey80', size=0.1) +
  geom_sf(data=gadm1_cropland, aes(fill=alpha_perc), color='grey80', size=0.1) +
  
  # Countries with subnat data
  geom_polygon(data=countries_robin_cropland, aes(long, lat, group=group), fill='NA', color='black', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  scale_fill_distiller(type="seq", direction=1, palette = 'Reds', breaks=c(0,25,50,75,100)) +
  
  theme_raster_map() +
  theme(legend.position="bottom",
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Percentage of\nnational total (%)")) +
  ggtitle('Cropland')


# /----------------------------------------------------------------------------#
#/ Make Forestry map
forestry_subnat_map <- 
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey93', color='grey70', size=0.05) +
  
  coord_sf(datum=st_crs(CRS('+proj=robin'))) +
  
  # Subnational units
  # geom_sf(data=gadm1_lu_wgs84_robin, aes(fill=country, alpha=alpha_perc), color='grey80', size=0.1) +
  geom_sf(data=gadm1_forestry, aes(fill=alpha_perc), color='grey80', size=0.1) +
  
  # Countries with subnat data
  geom_polygon(data=countries_robin_forestry, aes(long, lat, group=group), fill='NA', color='black', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  scale_fill_distiller(type="seq", direction=1, palette = 'Greens', breaks=c(0,25,50,75,100)) +
  
  theme_raster_map() +
  theme(legend.position="bottom",
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Percentage of\nnational total (%)")) +
  ggtitle('Forestry')


# /----------------------------------------------------------------------------#
#/ Make peat extraction map
peatextr_subnat_map <- 
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey93', color='grey70', size=0.05) +
  coord_sf(datum=st_crs(CRS('+proj=robin'))) +
  
  # Subnational units
  # geom_sf(data=gadm1_lu_wgs84_robin, aes(fill=country, alpha=alpha_perc), color='grey80', size=0.1) +
  geom_sf(data=gadm1_peatextr, aes(fill=alpha_perc), color='grey80', size=0.1) +
  
  # Countries with subnat data
  geom_polygon(data=countries_robin_peatextr, aes(long, lat, group=group), fill='NA', color='black', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  scale_fill_distiller(type="seq", direction=1, palette = 'Oranges', breaks=c(0,25,50,75,100)) +
  
  theme_raster_map() +
  theme(legend.position="bottom",
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Percentage of\nnational total (%)")) +
  ggtitle('Peat extraction')



# /----------------------------------------------------------------------------#
#/   Wetland Cultiv.
wetcultiv_subnat_map <- 
  ggplot() +
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey93', color='grey70', size=0.05) +
  coord_sf(datum=st_crs(CRS('+proj=robin'))) +
  
  # Subnational units; color ramp by n.data points
  geom_sf(data=gadm1_wetcultiv, aes(fill=alpha_perc), color='grey80', size=0.1) +
  
  # Outline of Countries with subnat data
  geom_polygon(data=countries_robin_wetcultiv, aes(long, lat, group=group), fill='NA', color='black', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  
  # coord_equal() +  theme_fig() +
  scale_fill_distiller(type="seq", direction=1, palette = 'Blues', breaks=c(1, 50, 80)) +
  
  theme_raster_map() +
  theme(legend.position="bottom",
        legend.title = element_text(size=6, color='black'), 
        legend.text = element_text(size=6, color='black'), 
        plot.title = element_text(size=8, color='black', hjust = 0.5),
        plot.margin = unit(c(-4,-2,-4,-2), "mm")) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Percentage of\nnational total (%)")) +
  ggtitle('Wetland cultivation')


# /----------------------------------------------------------------------------#
#/   Arrange plots grob into layout 
p <- plot_grid(cropland_subnat_map, forestry_subnat_map, 
               peatextr_subnat_map, wetcultiv_subnat_map,
               ncol=2, nrow=2, 
               align='hv')


# save plot
fname <- "../output/figures/artif_drainage/map/map_subnat_units_v8.png"
ggsave(plot=p, fname, width=190, height=135, dpi=500, units='mm', type = "cairo-png")
dev.off()


# save plot
fname <- "../output/figures/artif_drainage/map/map_subnat_units_v8.pdf"
ggsave(plot=p, fname, width=190, height=135, dpi=500, units='mm')
dev.off()


# rel_heights = c(1, 1, 1),
# rel_widths = c(.6, 1),
# labels = c('Cropland', 'Forestry', 'Peat Extraction'),
