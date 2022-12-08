

# read input data
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/wetloss_Mk2_stack_0.5deg.rds')

# create output stack
diff_stack <- stack()

# create label df
label_for_legend <- data.frame()


# get hyde years
hyde_yrs <- c(1700, 1750, 1800, 1850, 1900, 1950, 2000)

# loop through periods 
for (i in seq(length(hyde_yrs)-1)){
  
  # get start and end years
  hyde_yr_start = hyde_yrs[i]
  hyde_yr_end = hyde_yrs[i+1]
  
  
  # get rasters for start & end years by matching names
  r_start <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_start, names(remwet_Mkm2_stack))]]
  r_end   <-remwet_Mkm2_stack[[grep(pattern=hyde_yr_end, names(remwet_Mkm2_stack))]]
  
  r_start = mean(r_start)
  r_end = mean(r_end)
  
  # make raster of 
  r_diff = (r_end - r_start) / abs(hyde_yr_end - hyde_yr_start) * 10^6
  
  
  print(paste0("diff_", hyde_yr_start, "_to_", hyde_yr_end))
  names(r_diff) <- paste0("diff_", hyde_yr_start, "_to_", hyde_yr_end)
  
  # add the raster  to the stack
  diff_stack <- stack(diff_stack, r_diff)  
  
  label_for_legend <- bind_rows(label_for_legend, 
                                data.frame(id=i, label= paste0(hyde_yr_start, " to ", hyde_yr_end)))
  
}

# get period of max wetloss rate
diff_stack_maxperiod <- calc(diff_stack, which.max.na)
#values(diff_stack_maxperiod)[values(diff_stack_maxperiod)== 0] = NA



# get area with loss rate above a threshold 
diff_stack_range <- max(diff_stack) - min(diff_stack)
diff_stack_range[diff_stack_range < 0.01 | max(diff_stack) < 0.01]  <- NA
diff_stack_range[!is.na(diff_stack_range)] <- 1


# mask the period data
diff_stack_maxperiod <- mask(diff_stack_maxperiod, diff_stack_range)


# convert raster into data plottable in ggplot
diff_stack_maxperiod_robin <- prep_raster_into_robin_map(diff_stack_maxperiod)
diff_stack_maxperiod_robin <- left_join(diff_stack_maxperiod_robin, label_for_legend, by=c('layer'='id'))
diff_stack_maxperiod_robin <- filter(diff_stack_maxperiod_robin, !is.na(label))



# make color gradient
ccF <- scales::div_gradient_pal(low="green", mid="orange", high="blue",  
                               space="Lab")(seq(0,1,length.out=length(unique(diff_stack_maxperiod_robin$layer))))


# MAP HISTORICAL CASES COUNTRIES
map_max_wetlossrate_since1700 <- ggplot() +
  
  # add background country polygons
  geom_polygon(data=countries_robin_df, aes(long, lat, group=group), fill='grey85') +
  
  # add raster  
  geom_tile(data=diff_stack_maxperiod_robin, aes(x=x, y=y, fill=label)) +
  
  # add raster of low conversion rate
  #geom_tile(data=diff_stack_maxrate_robin, aes(x=x, y=y), fill='grey85') +
  
  # add outline of background countries
  # geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.2) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  coord_equal() +  theme_raster_map() +
  scale_fill_manual(values=ccF, name="Period of max\nwetland loss") +
  
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -3,-15, -3), "mm"))




#saveRDS(map_max_wetlossrate_since1700, './output/results/plot_obj/map_max_wetlossrate_since1700.rds')

### Save figure to file --------------------------------------------------------
# ggsave('./output/figures/map_max_wetlossrate_since1700.png',
#        width=178, height=80, dpi=600, units="mm", type = "cairo-png")
# dev.off()