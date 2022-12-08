
### get extracted wetloss from overlay ==========================================

f<-"./output/results/histcase_remwet_extracted_v2.csv"
cs_extract <- read.csv(f, stringsAsFactors = F)


cs_extract <- cs_extract %>%

    # exclude hiscases starting before 1700
    filter(hcase_start_year >= 1700) %>%

    # calculate % difference
    mutate(map_wetloss_prc = (remwet_start-remwet_end)/remwet_start*100) %>%

    # group by columns to keep
    group_by(src_id, country, region, hcase_start_year, 
             hyde_start_yr, hcase_end_year, hyde_end_yr) %>%
  
    summarise_at(.vars=c("map_wetloss_prc"), .funs=c("min","mean","max")) %>%
  
    # rename columns
    rename(map_wetloss_prc_min  = min,
           map_wetloss_prc_mean = mean,
           map_wetloss_prc_max  = max) %>%
  
    ungroup() %>%
    
    mutate(label = ifelse(is.na(region),
                          paste0(country,"\n", hyde_start_yr,"-", hyde_end_yr),
                          paste0(region, "\n", hyde_start_yr,"-", hyde_end_yr)))



# # read hitorical cases data ==================================================

# get file
f <- './output/results/histcases_mod/historic_cases_wetland_loss_mod.csv'

# 
histcases <- read.csv(f, stringsAsFactors = F, na.strings=c("", " ","NA")) %>%
  
  # select columns
  dplyr::select(-one_of("X","loc_id","Comment","Source","nb_yrs","perc_change_original",
                        "ef_comment","full.citation","period","lat_pt2","long_pt2")) %>%
  
  #dplyr::select(rec_id, rate_loss_km2_yr, perc_change_numeric, perc_change_per_yr) %>%
  # convert field
  mutate(perc_change_numeric = as.numeric(perc_change_numeric)*-1)




# join mapped and histcase data together =======================================
cs_joined <- left_join(cs_extract, histcases, by=c("src_id"='rec_id')) %>%
  filter(perc_change_numeric > 0) %>%
  filter(compiler == "Dahl 1990")



# make scatterplot =============================================================
ggplot(cs_joined) +
  
  # make 1:1 line
  geom_abline(slope=1, intercept=0, color='grey65') +
  
  # points
  geom_point(aes(x=perc_change_numeric, y=map_wetloss_prc_mean), color='blue', size=0.6) +
  
  # error bars
  geom_errorbar(aes(x= perc_change_numeric,
                    ymin= map_wetloss_prc_min, 
                    ymax= map_wetloss_prc_max), 
                color='blue', width=0, size=0.3) +
  
  # axis labels
  xlab("Historical record wetland loss (%)") + ylab("Mapped wetland loss (%)") +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(limits=c(0, 100)) +
  
  # fixed axis ratio
  coord_fixed() +
  line_plot_theme +
  
  
  # histcase label
  geom_text_repel(data = cs_joined,
                  aes(x=perc_change_numeric, y=map_wetloss_prc_mean,
                      label = label),
                  color='grey30',
                  segment.color='grey30',
                  size = 1.7,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm'))




### save plot
ggsave("./output/figures/scatterplot_compare_percloss_histcases_vs_mapped_onlyUSA_nolabels.pdf",
       width=87, height=80, dpi=600, units='mm')

dev.off()



### Calculate the agreement
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
#                  dplyr::select(rec_id, hyde_start_yr, hcase_end_year) %>%
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
