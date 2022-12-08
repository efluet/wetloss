library(rnaturalearth)
library(rworldmap)
library(rgeos)
library(ggrepel)
# set global extent to map ( excludes Antarctica)
# com_ext <- extent(-180, 180,  -60, 88)



# /------------------------------------------------------------------
#/ Get land area df 
landarea <- area(preswet) # 1-3
landarea_df <- raster2df(landarea)
names(landarea_df) <- c('landarea', 'x', 'y')

landarea_df <- left_join(maxlncr_df, landarea_df, by=c('x','y')) %>% 
  select(landarea)
# landarea_df <- landarea_df[,'landarea']
glimpse(landarea_df)






# /----------------------------------------------------------------------------#
#/  Get remwet
# f <- paste0('../output/results/wetloss/grid/grid_remwet_s', s_i, '_p', p_i, '.csv')
f <- paste0('../output/results/wetloss/grid/grid_remwet_s', s_i, '_p', p_i, '_', test_theta, '.csv')
grid_drain_remwet_peryear <- read.csv(f)
glimpse(grid_remwet_peryear)  

a <- pivot_longer(grid_remwet_peryear, cols=X1700:X2020)
glimpse(a)


newdrain_df <- data.frame()

for (y in seq(1700, 2010, 10)) {
  
  start <- grid_remwet_peryear[, eval(paste0('X', y))]
  end <- grid_remwet_peryear[, eval(paste0('X', y+10))]
  diff <- data.frame(start - end)

  names(diff) <- paste0('X', y)
  
  if(y==1700){ newdrain_df <- diff 
    } else {
  newdrain_df <- bind_cols(newdrain_df, diff) }
  
  }

glimpse(newdrain_df)


newdrain_df$maxtime <- colnames(newdrain_df)[max.col(newdrain_df,ties.method="first")]
newdrain_df$maxtime <- as.numeric(str_sub(newdrain_df$maxtime, start = 2, end = 5))
hist( newdrain_df$maxtime )

newdrain_df <- bind_cols(maxlncr_df, newdrain_df)



# TODO:  Reproject maxtime grid
# TODO:  Mask maxtime with regions above 10% loss

# /----------------------------------------------------------------------------#
#/    FIG 1-A :  MAP AREA LOST / Converted

library(ggnewscale)

peakdrainmap <- 
  
  ggplot()+
  
  # # countries background & outline
  # geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey90', color=NA, size=0.08) +
  # 
  # # Coastline
  # geom_path(data=coastsCoarse_robin_df, aes(long, lat, group=group), color='grey70', size=0.1) +
  # 

  # Add wetloss raster
  geom_tile(data=newdrain_df, aes(x=x, y=y, fill=as.factor(maxtime)))
  
  # # Add outline bounding box
  # geom_path(data=bbox_robin_df, aes(long, lat, group=group), color='black', size=0.08)
  # 
  # coord_equal() +  theme_raster_map() +
  # 
  # theme(legend.position = 'bottom',
  #       legend.direction = 'horizontal')


peakdrainmap




# /----------------------------------------------------
#/  For legend
ggplot()+
  # Add high wetland regions
  geom_tile(data=preswet_df, aes(x=x, y=y, fill=Fpreswet)) +
  scale_fill_gradient(low='#99eaff', high='#0091b8',
                      breaks=c(0.15, 0.5, 0.75, 1),
                      limits=c(0, 1)) +
  
  guides(fill = guide_colorbar(nbin=6, raster=F,
                               barheight = 0.4, barwidth=10,
                               reverse=T,
                               frame.colour=c('black'), frame.linewidth=0.7,
                               ticks.colour='black',  direction='horizontal',
                               title = expression(paste('Present-day wetland\nfraction (% of cell)')))) +
  
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

