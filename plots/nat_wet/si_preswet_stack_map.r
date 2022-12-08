

# /---------------------------------------------------
#/    Get simwet stack
simwet_stack <- stack('../output/results/natwet/simwet/simwet_stack.tif')
names(simwet_stack) <- c('orchidee2_km2', 'SDGVM2_km2', 'dlem2_km2', 'zhang_wpot')
simwet_stack <- raster::aggregate(simwet_stack, fact=2, fun=sum, na.rm=TRUE)


# # /---------------------------------------------------
# #/    Get preswet mamax stack
# preswet_stack <- stack('../output/results/natwet/preswet/preswet_stack.tif')
# names(preswet_stack) <- c('wad2m_Aw_mamax', 'glwd3_akmw', 'giems2_mamax_corr')
# preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum, na.rm=TRUE)


# /---------------------------------------------------
#/   Present-day wetland - maximum area
preswet_max_stack <- stack( '../output/results/natwet/preswet/preswet_stack_max.tif')
names(preswet_max_stack) <- c('wad2m', 'glwd3', 'giems2')
# preswet_stack <- aggregate(preswet_stack, fact=2, fun=sum, na.rm=TRUE)





names(preswet_max_stack) <- c('WAD2M', 'GLWD', 'GIEMSv2')
names(simwet_stack) <- c('ORCHIDEE', 'SDGVM', 'DLEM', 'LPJ-wsl')



preswet_stack_df <- data.frame()

preswet <- preswet_max_stack[[1]] / area(preswet_max_stack[[1]]) * 100
preswet_df <- WGSraster2dfROBIN(preswet) 
preswet_df$name <- 'WAD2M'
preswet_stack_df <- preswet_df


preswet <- preswet_max_stack[[2]] / area(preswet_max_stack[[2]]) *100 
preswet_df <- WGSraster2dfROBIN(preswet) 
preswet_df$name <- 'GLWD'
preswet_stack_df <- bind_rows(preswet_stack_df, preswet_df)


preswet <- preswet_max_stack[[3]] / area(preswet_max_stack[[3]]) *100
preswet_df <- WGSraster2dfROBIN(preswet) 
preswet_df$name <- 'GIEMSv2'
preswet_stack_df <- bind_rows(preswet_stack_df, preswet_df)



# /----------------------------------------------------------------------------#
#/    MAP PRESWET 

preswet_facet_map <- 
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  geom_tile(data=subset(preswet_stack_df, layer>1), 
                        aes(x=x, y=y, fill=layer)) +
  
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  
  theme_raster_map() +
  facet_wrap(~name, ncol=2) +
  
  scale_y_continuous(limits=c(-6600000, 8953595)) +
  
  scale_fill_gradient(low='#99ccff', high='#003d99',
                      breaks=c(1, 25, 50, 75, 100),
                      limits=c(1, 100)) +
  
  guides(fill = guide_colorbar(nbin=6, raster=F,
                               barheight = 0.4, barwidth=7,
                               # reverse=T,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal', 
                               title = expression(paste('Present-day wetland\nfraction (% of cell)')))) +

  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin=unit(c(-1, -1, -1, -1), "mm"),
        panel.spacing = unit(-2.5, "mm"),
        strip.background = element_blank(),
        strip.text = element_text(hjust= 0.5, vjust = -1, face='bold'))
  

# preswet_facet_map


# /----------------------------------------------------------------------------#
#/
ggsave('../output/figures/preswet_stack_map_v2.pdf',
       preswet_facet_map,
       width=190, height=120, dpi=600, units='mm' )


ggsave('../output/figures/preswet_stack_map_v2.png',
       preswet_facet_map,
       width=190, height=120, dpi=600, units='mm' )

dev.off()


