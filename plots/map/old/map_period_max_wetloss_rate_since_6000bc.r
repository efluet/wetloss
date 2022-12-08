
# read remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/wetloss_Mk2_stack_2.5deg.rds')


# only use pref & rdm because the  avoid  wetloss is mostly zero
pref <- sel.by.pattern(remwet_Mkm2_stack, "pref")
rdm <- sel.by.pattern(remwet_Mkm2_stack, "rdm")
remwet_Mkm2_stack <- stack(pref, rdm)


# get hyde years used as period limits
hyde_yrs          <- c(-6000, -4000, -2000, 0, 1000, 1700, 1980)
hyde_yrs_patterns <- c(".6000", ".4000", ".2000", "r0w", "r1000w", "1700", "1980")

# raster stack of differences
diff_stack <- stack()

# df of labels and numeric codes
label_for_legend <- data.frame()



# loop through period bounds 
for (i in seq(length(hyde_yrs)-1)){
  
  
  # get start and end years
  hyde_yr_start = hyde_yrs[i]
  hyde_yr_end = hyde_yrs[i+1]
  
  
  r_start <- sel.by.pattern(remwet_Mkm2_stack, hyde_yrs_patterns[i])
  r_end <- sel.by.pattern(remwet_Mkm2_stack, hyde_yrs_patterns[i+1])
  
  # get rasters for start & end years by matching names
  r_start <- mean(r_start)
  r_end   <- mean(r_end)
  

  # calc make raster of rate of change (km2)
  # negative value is an increase in weltand 
  r_diff = (r_end - r_start) / abs(hyde_yr_end - hyde_yr_start) * 10^6
  
  
  # name the output raster
  names(r_diff) <- paste0("diff_", hyde_yr_start, "_to_", hyde_yr_end)
  
  # cropland in norther china truly went down between 0-1000ad
  
  # add period to stack 
  diff_stack <- stack(diff_stack, r_diff)  
  
  # add to list of labels
  label_for_legend <- bind_rows(label_for_legend, 
                                data.frame(id=i, label = paste0(hyde_yr_start, " to ", hyde_yr_end)))
}

# get period of max wetloss rate
diff_stack_maxperiod <- calc(diff_stack, which.max.na)


# get area with loss rater above a threshold 
diff_stack_range <- max(diff_stack) - min(diff_stack)
diff_stack_range[diff_stack_range < 0.01 | max(diff_stack) < 0.01]  <- NA
diff_stack_range[!is.na(diff_stack_range)] <- 1

# mask the period data
diff_stack_maxperiod <- mask(diff_stack_maxperiod, diff_stack_range)



# convert raster into data plottable in ggplot
diff_stack_maxperiod_robin <- prep_raster_into_robin_map(diff_stack_maxperiod)
diff_stack_maxperiod_robin <- left_join(diff_stack_maxperiod_robin, label_for_legend, by=c('layer'='id'))
diff_stack_maxperiod_robin <- filter(diff_stack_maxperiod_robin, !is.na(label))



# set order (from last to first )
lengend_order <- c("-6000 to -4000", "-4000 to -2000", "-2000 to 0", "0 to 1000", "1000 to 1700", "1700 to 1980")

diff_stack_maxperiod_robin$label <- factor(diff_stack_maxperiod_robin$label, levels = lengend_order)
levels(diff_stack_maxperiod_robin$label)




# make color gradient
ccE <- scales::div_gradient_pal(low="green", mid="orange", high="blue",  
                                space="Lab")(seq(0,1,length.out=length(unique(diff_stack_maxperiod_robin$layer))))



# MAP HISTORICAL CASES COUNTRIES
map_max_wetlossrate_since6000bc <- ggplot() +
  
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
  scale_fill_manual(values=ccE, name="Period of max.\nwetland loss") +
  
  theme(legend.position= l_pos)+  
  theme(plot.margin = unit(c(-15, -1,-15, -1), "mm"))




#saveRDS(map_max_wetlossrate_since6000bc, './output/results/plot_obj/map_max_wetlossrate_since6000bc.rds')

### Save figure to file --------------------------------------------------------
# ggsave('./output/figures/map_max_wetlossrate_since6000bc.png',
#        width=178, height=80, dpi=600, units="mm", type = "cairo-png")
# dev.off()
