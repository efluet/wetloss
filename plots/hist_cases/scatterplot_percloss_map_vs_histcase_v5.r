# Scatterplot of map and historical records
# x: mapped loss
# y: Davidson cases

# Get data table of davidson & mapped loss
# cs_joined <- readRDS("../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod_wmaploss_v2_serial.rds")
# cs_joined <- cs_joined@data
# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_v4_serialmcmc.csv')
# cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_serialmcmcdf_v5.csv')
cs_joined <- read.csv('../output/results/histcases/histcase_mappedwetloss_extracted_serialmcmcdf_v8.csv')

cs_joined <- cs_joined %>% 
             pivot_wider(id_cols=c(rec_id, name, yr_start_rnd, yr_end_rnd, perc_change_numeric, areapoly_mkm2, continent), 
             names_from=param_type, 
             values_from = map_wetloss_prc_mean)

# Group the cases by time period
cs_joined <- cs_joined %>%
  mutate(period = paste0(yr_start_rnd, "-", yr_end_rnd)) %>%
  mutate(period = ifelse(yr_start_rnd >= 1900, "20th century", period)) %>%
  mutate(period = ifelse(period %in%  c("1700-1980", "1780-1980", "1790-1980", "1800-1980"), "~1780-1980", period)) %>%
  mutate(period = ifelse(period %in%  c("1880-1990", "1870-1960", "1810-1870"), "19th to 20th century", period))  %>%
  
  mutate(label = as.character(region)) %>% 
  mutate(perc_change_numeric = abs(perc_change_numeric)) %>%
  mutate(continent = paste0(continent, ' (', period, ')')) 
    # mutate(diff= abs(perc_change_numeric - best))
  


# /----------------------------------------------------------------------------#
#/ Calculate agreement score 
library(Metrics)

cs_joined <- cs_joined %>%   filter(!is.na(map_perc_lost))

# RMSE
rmse(cs_joined$map_perc_lost, 
     cs_joined$perc_change_numeric)

# UNWEIGHTED
fitlm = lm(perc_change_numeric ~ 0 + map_perc_lost, 
            data= cs_joined)

f <- summary(fitlm)
f$r.squared
f$adj.r.squared

# WEIGHTED
fitlm = lm(perc_change_numeric ~ 0 + map_perc_lost,
           weights = cs_joined$areapoly_mkm2 / var(cs_joined$areapoly_mkm2),
           data= cs_joined)


fw <- summary(fitlm)
fw$r.squared


# /----------------------------------------------------------------------------#
#/     Make scatterplot by period                                         ------

# Set a subset of labels
labels_ls = c('Illinois', 'Yangtze & Han', 'Po Delta', 'Mesopotamian marshes', 'Yangtze & Han\nRiver basins','West Songnen Plain',
              'California', 'Rhine valley', 'Lake Chad', 'Ohio', 'Indiana', 'Veracruz Port', 'Peloponnese', 'Po delta', 'West Virginia',
              'Magdalena River delta', 'Castille-La\nMancha regio', 'Lake Urmia', 'Italy: Friuli-Venezia\nGiulia', 'Wyoming', 'Idaho', 'Montana',
              'Najafgarh Lake', 'Lake Amik')



wetloss_scatterplot <- ggplot() +
  
  # make 1:1 line
  geom_abline(data=cs_joined, slope=1, intercept=0, color='grey85', size=0.3) +
  
  # points labeled by continent/period
  geom_point(data=cs_joined,
             aes(x=perc_change_numeric, 
                 y=map_perc_lost, 
                 fill=continent, size=areapoly_mkm2),
             shape=21, color='black', stroke=0.4, alpha=0.75) +
  
  geom_text(aes(x=0, y=95), label='Radj=0.46 \np<0.0001\nn=78', size=2.5, hjust=0) +
  
  # histcase label
  geom_text_repel(data = subset(cs_joined, label %in% labels_ls),
                  aes(x=perc_change_numeric,
                      y=map_perc_lost,
                      label = label),
                  color='grey30',
                  segment.color='grey30',
                  size = 2.0,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +

  # axis labels
  xlab("Literature records of wetland loss (%)") + 
  ylab("Mapped wetland loss (%)") +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100)) +  #, expand=c(0,0)
  scale_y_continuous(limits=c(0, 100)) +  #, expand=c(0,0)
  scale_shape_manual(values=c(21)) +
  scale_size(range=c(1.0, 18)) +
  # scale_fill_brewer(palette = "pal8")+
  guides(fill=guide_legend(ncol=2))+
  
  # fixed axis ratio
  # coord_fixed() +
  line_plot_theme +
  theme(panel.background = element_rect(color="black", size=0.5, fill=NA),
        legend.position = c(0 , 1.3), # c(0.05, 0.9),#"top",
        legend.direction = "horizontal",
        plot.margin=unit(c(40, 1, 3, 1), 'mm'))


wetloss_scatterplot


### save plot    -----------------------------------------------------
ggsave("../output/figures/hist_cases/scatterplot_wetloss_serial_mcmcfit_continentcolor_v5.png",
       width=90, height=150, dpi=900, units='mm', type = "cairo-png")

ggsave("../output/figures/hist_cases/scatterplot_wetloss_serial_mcmcfit_continentcolor_v5.pdf",
       width=90, height=150, dpi=900, units='mm')

dev.off()





# points
# geom_point(data=cs_joined,
#            aes(x=perc_change_numeric, y=best, color=period, shape=wet_categ), 
#            size=2.75) +


# error bars
# geom_errorbar(data=cs_joined,
#               aes(x= perc_change_numeric,
#                   ymin= `5%`,
#                   ymax= `95%`,
#                   color=period),
#               width=0, size=0.15) +


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
