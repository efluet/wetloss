

# read in drainage fraction
d <- read.csv("./output/results/artif_drainage/drained_wetcult_ha.csv")

names(d)[names(d)=="type"] <- "driver"



# remove columns with only NAs
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))


# modify database to plot every combination of 
d <- d %>% select(driver,  country_name, year, 
                  drained_area_tot, 
                  drained_area_irrig, 
                  drained_area_rainfed) %>%
           gather(key="type", value="drained_area", drained_area_tot:drained_area_rainfed) %>%
           mutate(driver_type = paste0(driver, "_", type)) %>%
           #select(-one_of(c("driver", "type")))  %>%
           filter(!is.na(drained_area))

           # spread(driver_type, drained_area) %>%
           # select_if(not_all_na) %>%
           # gather(key="driver_type", value="drained_area", cropland_drained_area_irrig:"Spate irrig._drained_area_irrig") 
  

# plot all data  ---------------------------------------------------------------
drain_cultwet <- ggplot(d) +
  
  geom_point(aes(x=year, y=drained_area, color=driver_type), size=0.4) +
  geom_line(aes(x=year, y=drained_area, color=driver_type), size=0.4) +
  
  expand_limits(y=0) +
  facet_wrap(~country_name, scales="free") +
  
  line_plot_theme +
  theme(legend.position = c(0.9, 0.03)) +
  ylab("Area drained (1000 ha)") + xlab("")




### save plot ------------------------------------------------------------------
ggsave(plot=drain_cultwet, "./output/figures/artif_drainage/drainage_wetcult_alldrivertypes_ha_percountry.png",
       dpi=300, width=600, height=400, units='mm' , type = "cairo-png")

dev.off()


#drained_area_plot


# # for labelling
# d <- d %>% group_by(type, country_name) %>% 
#             mutate(max_year = max(year)) %>% ungroup()




# lineplot of each indiv country of area 1000ha drained      ----------------------------------------

drained_area_plot <- ggplot(d) +
  
    geom_point(data=subset(d, type=="cropland"), aes(x=year, y=drained_area_tot), color='blue', size=0.4) +
    geom_line(data=subset(d, type=="cropland"), aes(x=year, y=drained_area_tot), color='blue',size=0.4) +

    geom_point(data=subset(d, type=="forestry"), aes(x=year, y=drained_area_tot), color='green', size=0.4) +
    geom_line(data=subset(d, type=="forestry"), aes(x=year, y=drained_area_tot), color='green',size=0.4) +

    geom_point(data=subset(d, type=="peatland"), aes(x=year, y=drained_area_tot), color='brown', size=0.4) +
    geom_line(data=subset(d, type=="peatland"), aes(x=year, y=drained_area_tot), color='brown',size=0.4) +

    #geom_bar(data=a, (aes=))

    expand_limits(y=0) +
    facet_wrap(~country_name, scales="free") +
    #facet_grid(type~continent, scales="free") +
    line_plot_theme +
    theme(legend.position = c(0.8, 0.1)) +
    ylab("Area drained (1000 ha)") + xlab("")

drained_area_plot



### save plot ------------------------------------------------------------------
ggsave(plot=drained_area_plot, "./output/figures/artif_drainage/drainage_wetcult_ha_percountry.png",
       dpi=300, width=500, height=320, units='mm' , type = "cairo-png")

dev.off()
