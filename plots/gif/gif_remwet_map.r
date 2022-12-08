# Gif of remwet over time

# /----------------------------------------------------------------------------#
#/   Get input grid drain perLU


s_i = 2; p_i=1; test_theta=0;  pars='avg' 

grid_remwet <- paste0('../output/results/wetloss/grid/grid_remwet/grid_remwet_s', s_i, '_p', p_i,'_t',test_theta, '_', pars, '_v1.csv')
grid_remwet <- read.csv(grid_remwet) %>% dplyr::select(-X)
grid_remwet <- bind_cols(maxlncr_df_xy, grid_remwet)
glimpse(grid_remwet)


### AS RASTER
grid_remwet_r <- rasterFromXYZ(as.data.frame(grid_remwet)) 
crs(grid_remwet_r) <- crs(simwet_stack)
grid_remwet_r <- grid_remwet_r / area(grid_remwet_r) * 100

grid_remwet_r[grid_remwet_r>100] <- 100
# grid_remwet_rdf <- as.data.frame(grid_remwet_r)



# /----------------------------------------------------------------------------#
#/   Function that makes plot

make_remwet_map <- function(grid_remwet_r, y){
  
  # Subset
  grid_remwet_r_temp <- grid_remwet_r[[y]]
  grid_remwet_robin_df <- WGSraster2dfROBIN(grid_remwet_r_temp)
  names(grid_remwet_robin_df) <- c('x','y','remwet_perc')
  grid_remwet_robin_df <- grid_remwet_robin_df %>% filter(remwet_perc > 2)

  
  years <- seq(1700, 2020, 10)
  
  # make ggplot
  remwetmap <-
    ggplot() +
    
    # countries background & outline
    geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey96', color=NA, size=0.08) +
    
    # Coastline
    geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
    
    # add max driver raster;  geom_tile
    geom_raster(data=grid_remwet_robin_df, aes(x=x, y=y, fill=remwet_perc)) +
    
    # Add outline bounding box
    geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08) +
    
    coord_equal() +  theme_raster_map() +
    
    # facet_wrap(~lu_type, ncol=3, drop=F) +
    
    # scale_fill_gradientn(colours=c('#B7E8EB','#9CDEEB','#1165C1','#043F98'), 
    #                      limits=c(0, 100), na.value="grey96") +
    scale_fill_gradientn(colours=c('#FFF5AD','#CFC6A7','#9F97A2','#6F699D','#544E9A','#393497','#1E1A94','#040092'), 
                         limits=c(0, 100), na.value="grey96") +
    
    guides(fill = guide_colorbar(nbin=10, raster=F,
                                 barheight = 0.4, barwidth=7,
                                 frame.colour=c('black'), frame.linewidth=0.7,
                                 ticks.colour='black',  direction='horizontal',
                                 title = expression(paste("Wetland fraction per gridcell (%)")))) +
    
    ggtitle(paste0(years[y], '   -   ',  'ORCHIDEE-WADM reconstruction')) +
    
    theme(plot.title = element_text(size=5, hjust = 0.5),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          plot.margin=unit(c(-1, -1, -1, -1), 'mm'),
          panel.spacing = unit(-1, 'mm'),
          strip.background = element_blank(),
          strip.text = element_text(size=8, face='bold'))
  
  
  return(remwetmap)
}




library(animation)

# /----------------------------------------------------------------------------#
#/    Make Gif over time
ani.options("convert")
saveGIF({
  
  # why are 1850 and 1950 disappearing? plot global time line?
  
  # /--------------------------------------------------------------------------#
  #/  Loop through hyde years
  # for (i in years[1:33]){
  for (i in c(1:33)){
    
    print(i)
    
    p <-make_remwet_map(grid_remwet_r, i)
    show(p)
  }
},  movie.name = "/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/output/figures/gif/grid_remwet_gif_orchidee_wad2m.gif", 
ani.width=2000, ani.height=1200, ani.res= 350,   # ani.width=3000, ani.height=1600, ani.res= 400, 
interval = 0.8, loop=TRUE, clean=TRUE)





#   pivot_longer(cols=wetcultiv:forestry, names_to='lu_type', values_to='areakm2') %>% 
#   mutate(lu_type= stri_trans_totitle(lu_type)) %>%
#   mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
#          lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
#          lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
#          lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
#          lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type)) %>%
#   mutate(areakm2= ifelse(areakm2 > 0.1, areakm2, NA)) %>%
#   mutate(areakm2=ifelse(areakm2>500, 500, areakm2))
# # mutate(areakm2= ifelse(areakm2 > 500, 500, areakm2))
# # filter(areakm2 > 0.1) %>% 

# grid_remwet <- grid_remwet_all

# Filter by year bc fkin dplyr doesnt filter
# grid_remwet_temp <- grid_remwet[grid_remwet$year==y,]

# glimpse(grid_remwet_temp)

# grid_remwet_temp <- grid_remwet_temp %>%
# pivot_wider( names_from='lutype', values_from='areakm2')

# glimpse(grid_remwet_temp)
# arrange(grid_remwet_temp, x ,y)

# r <- rasterFromXYZ(as.data.frame(grid_remwet))
# r <- rasterFromXYZ(as.data.frame(grid_remwet_temp)[, c('x', 'y', 'wetcultiv','ir_rice','cropland','urban','pasture','peatextr','forestry')])


# # Reproject
# grid_remwet_robin_df <- 
#   WGSraster2dfROBIN(r) %>% 
#   pivot_longer(cols=wetcultiv:forestry,
#                names_to='lu_type',
#                values_to='areakm2') %>% 
#   mutate(lu_type= stri_trans_totitle(lu_type)) %>%
#   mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
#          lu_type= ifelse(lu_type=='Urban'  , 'Urban', lu_type),
#          lu_type= ifelse(lu_type=='Forestry' , 'Forestry', lu_type),
#          lu_type= ifelse(lu_type=='Peatextr' , 'Peat Extraction', lu_type),
#          lu_type= ifelse(lu_type=='Wetcultiv' , 'Wetland Cultiv.', lu_type)) %>% 
#   # mutate(areakm2 = ifelse(areakm2==0, NA, areakm2))
#   filter(areakm2 > 0.1) %>% 
#   mutate(areakm2=ifelse(areakm2>500, 500, areakm2))
# 
# 
# grid_remwet_robin_df



# /----------------------------------------------------------------------------#
#/ Compile all drain perlu over time into single db 
# lulist <- c('cropland','urban','pasture','peatextr','forestry','wetcultiv','ir_rice')
# 
# grid_remwet_all <- data.frame()
# 
# for (lu in lulist){
#   
#   print(lu)
#   f_out <- paste0('../output/results/wetloss/grid/grid_remwet/grid_drain_', lu, '_s', s_i, '_p', p_i,'_t', test_theta, '_', pars, '_v1.csv')
#   grid_drain_lu <- read.csv(f_out) %>% mutate(lutype = lu)
#   
#   # Append rows 
#   grid_remwet_all <- bind_rows(grid_remwet_all, grid_drain_lu)
# }
# 
# # Format df
# grid_remwet_all <- 
#   grid_remwet_all %>%
#   select(-X) %>%
#   pivot_longer(cols=X1700:X2020, names_to='year', values_to='areakm2') %>%
#   mutate(year = as.numeric(substring(year, 2, 5)))
# 
