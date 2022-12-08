
# write formatted histcase output 
histcases <- read.csv('./output/results/histcases_mod/historic_cases_wetland_loss_mod.csv')





# plot wetland loss duration ===================================
ggplot(histcases) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x=nb_yrs, y=perc_change_numeric, color=continent), 
             size=1, alpha=0.3) +
  xlab('Record Length (years)') +
  ylab("Wetland area change (%)") +
  ylim(-100, 150) + 
  line_plot_theme


### save plot 
ggsave("./output/figures/scatter_plot_hist_cases_percrate_length.png",
       width=98, height=90, dpi=600, units='mm', type = "cairo-png")

dev.off()




# PLOT - lines of histcases wetloss ==============================================

# subset database that has both start and end
histcases_compcases <- histcases %>%

  # select data columns
  dplyr::select(rec_id, continent, yr_start, yr_end, perc_change_numeric) %>%
  # keep cases
  filter(complete.cases(.)) %>%
  gather(year_name, year, yr_start:yr_end) %>%
  mutate(perc_change_numeric= ifelse(year_name=='yr_start', 
                                     100, 100+perc_change_numeric))


# get package that does ggzoom
library(ggforce)

# makeplot
ggplot(histcases_compcases) +
  
  # points
  geom_point(aes(x=year, y=perc_change_numeric, group=rec_id, color=continent),
             size=0.2, alpha=0.8) +
  
  # plot lines between pairs of pts
  geom_line(aes(x=year, y=perc_change_numeric, group=rec_id, color=continent), 
            alpha=0.3, size=0.45) +
  
  # axes labels
  xlab('Year') + ylab("Wetland area remaining (%)") +
  
  line_plot_theme +
  theme(legend.direction = "vertical",
        legend.position = "right") +
  
  # add a zoomed-in panel
  facet_zoom(x = year>=1900)


### save plot
ggsave("./output/figures/scatter_plot_hist_cases_perc_rate_lines.png",
       width=178, height=120, dpi=600, units='mm', type = "cairo-png")

dev.off()

