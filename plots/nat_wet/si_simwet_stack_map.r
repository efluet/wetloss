

# /---------------------------------------------------
#/    Get simwet stack
simwet_stack <- stack('../output/results/natwet/simwet/simwet_stack.tif')
names(simwet_stack) <- c('orchidee2_km2', 'SDGVM2_km2', 'dlem2_km2', 'zhang_wpot')
simwet_stack <- raster::aggregate(simwet_stack, fact=2, fun=sum, na.rm=TRUE)



# names(simwet_max_stack) <- c('WAD2M', 'GLWD', 'GIEMS2')
names(simwet_stack) <- c('ORCHIDEE', 'SDGVM', 'DLEM', 'LPJ-wsl')



simwet_stack_df <- data.frame()

simwet <- simwet_stack[[1]] / area(simwet_stack[[1]]) * 100
simwet_df <- WGSraster2dfROBIN(simwet) 
simwet_df$name <- 'ORCHIDEE'
simwet_stack_df <- simwet_df


simwet <- simwet_stack[[2]] / area(simwet_stack[[2]]) *100 
simwet_df <- WGSraster2dfROBIN(simwet) 
simwet_df$name <- 'SDGVM'
simwet_stack_df <- bind_rows(simwet_stack_df, simwet_df)


simwet <- simwet_stack[[3]] / area(simwet_stack[[3]]) *100
simwet_df <- WGSraster2dfROBIN(simwet) 
simwet_df$name <- 'DLEM'
simwet_stack_df <- bind_rows(simwet_stack_df, simwet_df)


simwet <- simwet_stack[[4]] / area(simwet_stack[[4]]) *100
simwet_df <- WGSraster2dfROBIN(simwet) 
simwet_df$name <- 'LPJ-wsl'
simwet_stack_df <- bind_rows(simwet_stack_df, simwet_df)



simwet_stack_df <- simwet_stack_df %>% 
  mutate(layer= ifelse(layer>100, 100, layer))


# /----------------------------------------------------------------------------#
#/    MAP simWET 

simwet_facet_map <- 
  ggplot()+
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # Add high wetland regions
  geom_tile(data=subset(simwet_stack_df, layer>1), 
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
                               title = expression(paste('Simulated wetland\nfraction (% of cell)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin=unit(c(-1, -1, -1, -1), "mm"),
        panel.spacing = unit(-2.5, "mm"),
        strip.background = element_blank(),
        strip.text = element_text(hjust= 0.5, vjust = -1, face='bold'))


simwet_facet_map


# /----------------------------------------------------------------------------#
#/
ggsave('../output/figures/simwet_stack_map.pdf',
       simwet_facet_map,
       width=190, height=120, dpi=600, units='mm' )


ggsave('../output/figures/simwet_stack_map.png',
       simwet_facet_map,
       width=190, height=120, dpi=600, units='mm' )

dev.off()


