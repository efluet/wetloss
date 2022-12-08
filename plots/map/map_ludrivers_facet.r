# /----------------------------------------------------------------------------#
#/   Get input grid drain perLU
s_i=4; p_i=1; pars='avg'

f_out <- paste0('../output/results/wetloss/grid/grid_cumul_drain_perlu/grid_cumul_drain_perlu_s', s_i, '_p', p_i,'_t',test_theta, '_', pars, '_v1.csv')
grid_drain_perlu <- read.csv(f_out) %>% dplyr::select(-X) 
grid_drain_perlu <- grid_drain_perlu %>% mutate(pixlandarea = maxlncr_df[,1])

### AS RASTER
r <- rasterFromXYZ(as.data.frame(grid_drain_perlu)[, c('x', 'y', 'wetcultiv','ir_rice','cropland','urban','pasture','peatextr','forestry','pixlandarea')])

# Reproject
grid_drain_perlu_robin_df <- 
  WGSraster2dfROBIN(r) %>% 
  # Convert from km2 to % of land area
  mutate(wetcultiv = wetcultiv /pixlandarea*100,
         ir_rice   = ir_rice /pixlandarea*100,
         cropland  = cropland /pixlandarea*100,
         urban=urban/pixlandarea*100,
         pasture=pasture/pixlandarea*100,
         peatextr=peatextr/pixlandarea*100,
         forestry=forestry/pixlandarea*100,
         pixlandarea=pixlandarea/pixlandarea*100) %>% 
  dplyr::select(-pixlandarea) %>% 
  # Pivot
  pivot_longer(cols=wetcultiv:forestry,
               names_to='lu_type',
               values_to='areakm2') %>% 
  mutate(lu_type= stri_trans_totitle(lu_type)) %>%
  mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Irrigated Rice', lu_type),
         lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
         lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
         lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
         lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultivation', lu_type)) %>% 
  # mutate(areakm2 = ifelse(areakm2==0, NA, areakm2))
  filter(areakm2 > 0.1) %>% 
  mutate(areakm2=ifelse(areakm2>30, 30, areakm2))
  # mutate(areakm2=ifelse(areakm2>500, 500, areakm2))



grid_drain_perlu_robin_df

# /----------------------------------------------------------------------------#
#/   Max Driver Robin Map
facet_lugrids <-
  ggplot() +
  
  # countries background & outline
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey94', color=NA, size=0.08) +
  
  # Coastline
  geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  
  # add max driver raster;  geom_tile
  geom_raster(data=grid_drain_perlu_robin_df, aes(x=x, y=y, fill=areakm2)) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
  
  coord_equal() +  theme_raster_map() +
  
  facet_wrap(~lu_type, ncol=3) +

  scale_fill_gradientn(colours=c('yellow', '#ffc003','orange','red','#C90900'),
                       #low='#ffd11a', high='#e60000',
                       # breaks=c(0, 25, 50, 75, 100),
                       limits=c(0, 30)) +
  
  # scale_fill_gradientn(colours=c('yellow', '#ffc003','orange','red','#C90900'),
  #                     #low='#ffd11a', high='#e60000',
  #                     # breaks=c(0, 25, 50, 75, 100),
  #                     limits=c(0, 500)) +
  
  guides(fill = guide_colorbar(nbin=10, raster=F,
                               barheight = 0.4, barwidth=7,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = "Wetland area lost, drained or converted\nper grid cell (percentage of land area)")) +
  # title = expression(paste("Wetland area lost, drained or converted\nper grid cell (","km"^{2},")")))) +
  
  theme(legend.position = c(0.5, 0.15), #'bottom',
        legend.direction = 'horizontal',
        plot.margin=unit(c(0, 0, 0, 0), 'mm'),
        # panel.spacing = unit(-1, 'mm'),
        panel.margin.y=unit(0,"mm"),
        panel.margin.x=unit(-4.1,"mm"),
        strip.background = element_blank(),
        strip.text = element_text(size=8, face='bold'))



# Save figure to file
ggsave(paste0('../output/figures/forsi/map_ludrivers_facet_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_perc_v2.png'), 
       facet_lugrids,
       width=310, height=170, dpi=600, units='mm')

# Save figure to file
ggsave(paste0('../output/figures/forsi/map_ludrivers_facet_s',s_i,'_p',p_i,'_t', test_theta, '_', pars, '_perc_v2.pdf'), 
       facet_lugrids,
       width=310, height=170, dpi=600, units='mm')





# /----------------------------------------------------------------------------#
#/ Compile all drain perlu over time into single db TO MAKE GIF
lulist <- c('cropland','urban','pasture','peatextr','forestry','wetcultiv','ir_rice')

grid_drain_perlu_all <- data.frame()

for (lu in lulist){
  
  print(lu)
  f_out <- paste0('../output/results/wetloss/grid/grid_drain_perlu/grid_drain_', lu, '_s', s_i, '_p', p_i,'_t', test_theta, '_', pars, '_v1.csv')
  grid_drain_lu <- read.csv(f_out) %>% mutate(lutype = lu)
  
  # Append rows 
  grid_drain_perlu_all <- bind_rows(grid_drain_perlu_all, grid_drain_lu)
  }

# Format df
grid_drain_perlu_all <- 
  grid_drain_perlu_all %>%
  select(-X) %>%
  pivot_longer(cols=X1700:X2020, names_to='year', values_to='areakm2') %>%
  mutate(year = as.numeric(substring(year, 2, 5)))



# /----------------------------------------------------------------------------#
#/   Function that makes map plot

make_grid_drain_perlu_facetmaps <- function(grid_drain_perlu, y){

  
  # grid_drain_perlu <- grid_drain_perlu_all
  
  # Filter by year bc fkin dplyr doesnt filter
  grid_drain_perlu_temp <- grid_drain_perlu[grid_drain_perlu$year==y,]
  
  # glimpse(grid_drain_perlu_temp)
  
  grid_drain_perlu_temp <- grid_drain_perlu_temp %>%
         pivot_wider( names_from='lutype', values_from='areakm2')

    # glimpse(grid_drain_perlu_temp)
    # arrange(grid_drain_perlu_temp, x ,y)
  
    r <- rasterFromXYZ(as.data.frame(grid_drain_perlu_temp)[, c('x', 'y', 'wetcultiv','ir_rice','cropland','urban','pasture','peatextr','forestry')])
    
    grid_drain_perlu_robin_df <- 
        WGSraster2dfROBIN(r) %>%
        pivot_longer(cols=wetcultiv:forestry, names_to='lu_type', values_to='areakm2') %>% 
        mutate(lu_type= stri_trans_totitle(lu_type)) %>%
        mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
               lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
               lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
               lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
               lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type)) %>%
        mutate(areakm2= ifelse(areakm2 > 0.1, areakm2, NA)) %>%
        mutate(areakm2=ifelse(areakm2>500, 500, areakm2))
        # mutate(areakm2= ifelse(areakm2 > 500, 500, areakm2))
        # filter(areakm2 > 0.1) %>% 
        

      
    facet_lugrids <-
      ggplot() +
      
      # countries background & outline
      geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey96', color=NA, size=0.08) +
      
      # Coastline
      geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
      
      # add max driver raster;  geom_tile
      geom_raster(data=grid_drain_perlu_robin_df, aes(x=x, y=y, fill=areakm2)) +
      
      # Add outline bounding box
      geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
      
      coord_equal() +  theme_raster_map() +
      
      facet_wrap(~lu_type, ncol=3, drop=F) +
      
      scale_fill_gradientn(colours=c('#ffc003','orange','red','#C90900'), # 'yellow', 
                           limits=c(0, 700), na.value="grey96") +
      
      guides(fill = guide_colorbar(nbin=10, raster=F,
                                   barheight = 0.4, barwidth=7,
                                   frame.colour=c('black'), frame.linewidth=0.7,
                                   ticks.colour='black',  direction='horizontal',
                                   title = expression(paste("Wetland area lost per gridcell (","km"^{2},")")))) +
      
      ggtitle(y) +
      
      theme(legend.position = c(0.75, 0.15),
            legend.direction = 'horizontal',
            plot.margin=unit(c(0, 0, 0, 0), 'mm'),
            panel.spacing = unit(-1, 'mm'),
            strip.background = element_blank(),
            strip.text = element_text(size=8, face='bold'))
    
    
    return(facet_lugrids)
    }




library(animation)

# /----------------------------------------------------------------------------#
#/    Make Gif over time
ani.options("convert")
saveGIF({
  
  # why are 1850 and 1950 disappearing? plot global time line?
  
  # /--------------------------------------------------------------------------#
  #/  Loop through hyde years
  for (i in years[1:33]){
    
    print(i)
    
    p <-make_grid_drain_perlu_facetmaps(grid_drain_perlu_all, i)
    show(p)
    }
  },  movie.name = "/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/output/figures/gif/grid_drain_perlu_gif6.gif", 
  ani.width=3200, ani.height=1800, ani.res= 400, 
  interval = 0.4, loop=TRUE, clean=TRUE)




