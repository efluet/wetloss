

# read natural wetland area 6000BC to 1700
wetarea <-  read.csv("./output/results/wetloss/sum/wetloss_all_area.csv") %>%
            filter(year >= -6000 & year <= 1600) %>%
            filter(grepl("000", year) | year == 0 | year == 1600) %>%
            mutate(tot_convtocrop_Mkm2 = tot_wetloss_Mkm2 - tot_convtorice_Mkm2) %>%
            dplyr::select(-one_of("X.1", "X", "res", "name", "overlap", 
                                  "tot_remwet_Mkm2_in1700","remwet_prc_since1700", 
                                  "tot_irrice_Mkm2","tot_cropland_Mkm2",
                                  "tot_remwet_Mkm2", "tot_wetloss_Mkm2")) %>%


            gather(type, value, tot_wet_Mkm2:tot_convtocrop_Mkm2) %>%
            group_by(year, type) %>%
            summarize_all(funs(min, mean, max)) %>%
            dplyr::select(year, type, mean) 


wetarea <- wetarea %>%
            group_by(type) %>%
            mutate(area_change = lag(mean, order_by=year) - mean,
                   period = paste0(lag(year, order_by=year)," to\n",year)) %>%
            mutate(midyear_ofperiod = (lag(year, order_by=year) + year) /2) %>%
            mutate(area_change=ifelse(type=="tot_wet_Mkm2", area_change*-1,area_change)) %>%
              filter(!is.na(area_change))

  
  


# BARPLOT PRE-1700 =============================================================

#surv_data$mean_dif_perc_cut <- factor(surv_data$mean_dif_perc_cut)
lengend_order <- (c("-6000 to\n-5000", "-5000 to\n-4000", "-4000 to\n-3000", 
                       "-3000 to\n-2000","-2000 to\n-1000", "-1000 to\n0", "0 to\n1000", "1000 to\n1600"))
wetarea$period <- factor(wetarea$period, levels = lengend_order)

# shading <- data.frame(min = seq(from = -6000, to = 1000, by = 1000),
#                       max = seq(from = -5000, to = 2000, by = 1000),
#                       col = c(1,0))


shading <- data.frame(min = seq(from = 0.5, to = 8, by = 1),
                      max = seq(from = 1.5, to = 8.5, by = 1),
                      col = c(1,0))



pre_1700_plot <- ggplot(wetarea) +
  
  geom_bar(aes(x=period, y=area_change, group=type, fill=type), 
           stat='identity', position = position_dodge(width = 0.8), width=0.7) + 
  
  geom_rect(data = shading,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, alpha = col),
            fill='grey95') +
  
  geom_bar(aes(x=period, y=area_change, group=type, fill=type), 
           stat='identity', position = position_dodge(width = 0.8), width=0.7) + 
  
  geom_hline(yintercept=0, size=0.2, color="grey25") +
  
  #scale_x_continuous(expand=c(0,0)) +  #limits=c(-6000, 2000), breaks=c(seq(-6000,1000,1000), 1700)) +
  scale_y_continuous(expand=c(0,0), limits=c(-0.55, 0.6)) +
  line_plot_theme +
  theme(legend.position = c(0.2, 0.8)) +
  guides(alpha = FALSE) +
  ylab(expression(Global~wetland~area~(10^6~km^2))) +
  xlab("Year") +
  theme(legend.position = c(0.2, 0.3), #"none",
        plot.margin = margin(1,-3.15,1,1,"mm"))


### SINCE 1700 =================================================================

# read natural wetland area 6000BC to 1700
wetarea <-  read.csv("./output/results/wetloss/sum/wetloss_all_area.csv") %>%
            filter(year >= 1700 & year <=1980) %>%
            filter(grepl("00", year) | grepl("50", year) | year==1980) %>%
            mutate(tot_convtocrop_Mkm2 = tot_wetloss_Mkm2 - tot_convtorice_Mkm2) %>%
            dplyr::select(-one_of("X.1", "X", "res", "name", "overlap", 
                                  "tot_remwet_Mkm2_in1700","remwet_prc_since1700", 
                                  "tot_irrice_Mkm2","tot_cropland_Mkm2",
                                  "tot_remwet_Mkm2", "tot_wetloss_Mkm2")) %>%
  
            gather(type, value, tot_wet_Mkm2:tot_convtocrop_Mkm2) %>%
            group_by(year, type) %>%
            summarize_all(funs(min, mean, max)) %>%
            dplyr::select(year, type, mean) 


wetarea <- wetarea %>%
  group_by(type) %>%
  mutate(area_change = lag(mean, order_by=year) - mean,
         period = paste0(lag(year, order_by=year)," to\n",year)) %>%
  filter(!is.na(area_change)) %>%
  mutate(area_change=ifelse(type=="tot_wet_Mkm2", area_change*-1,area_change)) %>%
  mutate(midyear_ofperiod = (lag(year, order_by=year) + year) /2)


# Make plot -----------------------------------------------
# shading <- data.frame(min = seq(from = 1700, to = 1950, by = 50),
#                       max = seq(from = 1750, to = 2000, by = 50),
#                       col = c(1,0))

#shading <- 
shading <- shading[1:6,]


post_1700_plot <- ggplot(wetarea) +

  geom_bar(aes(x=period, y=area_change, group=type, fill=type), 
           stat='identity', position = position_dodge(width = 0.8), width=0.7) +  
  geom_rect(data = shading,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, alpha = col), fill='grey95') +
  

  
  geom_bar(aes(x=period, y=area_change, group=type, fill=type), 
           stat='identity', position = position_dodge(width = 0.8), width=0.7) +
  
  geom_hline(yintercept=0, size=0.2, color="grey25") +
  
  #scale_x_continuous(limits=c(-6000, 1700), expand=c(0,0), breaks=c(seq(-6000,1000,1000), 1700)) +
  scale_y_continuous(expand=c(0,0), limits=c(-0.55, 0.6)) +
  line_plot_theme +
  theme(legend.position = c(1, 100)) +
  guides(alpha = FALSE) +
  #ylab(expression(Global~wetland~area~(10^6~km^2))) +
  ylab("") +
  xlab("Year") +
  theme(legend.position = c(0.05, 0.3),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(1,1,1,-3.15,"mm"))


post_1700_plot



# arrange together
pre_n_post <- grid.arrange(pre_1700_plot, post_1700_plot, nrow=1,
                           widths=c(0.55, 0.45))




### save plot ------------------------------------------------------------------
ggsave("./output/figures/barplot_wetarea_change_overtime.png", pre_n_post,
       dpi=800, width=178, height=80, units='mm' , type = "cairo-png")

ggsave("./output/figures/barplot_wetarea_change_overtime.pdf", pre_n_post,
       dpi=800, width=178, height=80, units='mm')

dev.off()

