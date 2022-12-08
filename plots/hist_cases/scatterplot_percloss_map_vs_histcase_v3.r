# Scatterplot of map and historical records
# x: mapped loss
# y: Davidson cases

# Get data table of davidson & mapped loss
cs_joined <- readRDS("../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod_wmaploss_v2_serial.rds")
cs_joined <- cs_joined@data

# Group the cases by time period
cs_joined <- cs_joined %>%
  mutate(period = paste0(hyde_start_yr, "-", hyde_end_yr)) %>%
  mutate(period = ifelse(hyde_start_yr >= 1900, "20th century", period)) %>%
  mutate(period = ifelse(period %in%  c("1700-1980", "1780-1980", "1790-1980", "1800-1980"), "~1780-1980", period)) %>%
  mutate(period = ifelse(period %in%  c("1880-1990", "1870-1960", "1810-1870"), "19th to 20th century", period))  %>%
  
  mutate(label = as.character(name)) 
  
  # select(name, country.x, period, remwet_start, remwet_end, perc_change_numeric, map_wetloss_prc_mean)

  
# /----------------------------------------------------------------------------#
#/     Make scatterplot by period                                         ------
ggplot(cs_joined) +
  
  # make 1:1 line
  geom_abline(slope=1, intercept=0, color='grey65', size=0.2) +
  
  # points
  geom_point(aes(x=perc_change_numeric, y=map_wetloss_prc_mean, color=period, shape=wet_categ), size=2.75) +
  
  # error bars
  geom_errorbar(aes(x= perc_change_numeric,
                    ymin= map_wetloss_prc_min,
                    ymax= map_wetloss_prc_max,
                    color=period),
                width=0, size=0.15) +
  
  # histcase label
  geom_text_repel(data = cs_joined,
                  aes(x=perc_change_numeric,
                      y=map_wetloss_prc_mean,
                      label = label),
                  color='grey30',
                  segment.color='grey30',
                  size = 1.4,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +

  # axis labels
  xlab("Literature records of wetland loss (%)") + 
  ylab("Mapped wetland loss (%)") +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100), expand=c(0,0)) +
  scale_y_continuous(limits=c(0, 100), expand=c(0,0)) +
  
  # fixed axis ratio
  coord_fixed() +
  line_plot_theme +
  theme(panel.background = element_rect(color="black", size=0.5, fill=NA),
        legend.position = "top",
        legend.direction = "vertical")


### save plot    -----------------------------------------------------
ggsave("../output/figures/hist_cases/scatterplot_wetloss_alldavidson_additive_v2_serial.png",
       width=190, height=180, dpi=900, units='mm', type = "cairo-png")

dev.off()


# /----------------------------------------------------------------------------#
#/ Calculate agreement score 
library(Metrics)

cs_joined <- cs_joined %>%
  filter(!is.na(map_wetloss_prc_mean))

rmse(cs_joined$perc_change_numeric, 
     cs_joined$map_wetloss_prc_mean)


fitlm = lm( perc_change_numeric ~ map_wetloss_prc_mean, 
            data= cs_joined)

summary(fitlm)$r.squared

#rm(f, histcases, cs_extract, cs_joined)












# make timeline of diff =============================================================

# cs_joined_mod <- cs_joined %>%
#                  dplyr::select(src_id, hyde_start_yr, hcase_end_year) %>%
#                  gather(a, b, )
# 
# 
# ggplot(cs_joined) +
#   
#   # points
#   geom_point(aes(x=perc_change_numeric, y=map_wetloss_prc_mean), color='blue') +
#   
#   # error bars
#   geom_errorbar(aes(x= perc_change_numeric,
#                     ymin= map_wetloss_prc_min, ymax= map_wetloss_prc_max), 
#                 color='blue', width=0) +
#   
#   # make 1:1 line
#   geom_abline(slope=1, intercept=0, color='grey85') +
#   
#   # axis labels
#   xlab("Historical record wetland loss (%)") + ylab("Mapped wetland loss (%)") +
#   
#   # axis limits
#   # scale_x_continuous(limits=c(0, 100)) +
#   # scale_y_continuous(limits=c(0, 100)) +
#   
#   # fixed axis ratio
#   coord_fixed() +
#   
#   
#   # histcase label
#   geom_text_repel(data = cs_joined,
#                   aes(x=perc_change_numeric, y=map_wetloss_prc_mean,
#                       label = label),
#                   color='grey30',
#                   segment.color='grey30',
#                   size = 2,
#                   nudge_x = 0,
#                   segment.size = 0.25,
#                   box.padding = unit(3, 'mm'),
#                   point.padding = unit(3, 'mm'))
# 
