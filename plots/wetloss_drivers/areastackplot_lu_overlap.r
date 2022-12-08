library(lemon)

# write summed area of wetloss, remaining, etc... to output
sum_overlap <- read.csv( './output/results/wetloss/sum/sum_wet_overlap_3subgrid.csv') 



# /----------------------------------------------------------------------------#
#/  RemWet AREA Mkm^2 plot                                               -------

ggplot(sum_overlap) +
  
  # plot nat wet cover line
  geom_area(aes(x=year, y= sum_overlap/10^6, fill=lu,  group=lu), color="white", size=0.4, position = "stack") +
  # axis lables
  xlab("Year") +  ylab(expression(paste("Wetland area (10"^{6},' km'^{2},")"))) +

  facet_rep_wrap(.~ overlap, ncol=1) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +

  line_plot_theme +
  theme(#legend.position = c(0.2, 0.3), #"none",
        plot.margin = margin(1,1,1,1,"mm")) #+ ggtitle("Cropland")



# /-----------------------------------------------------------------------------
#/  Line plot of overlap

# prep data - inverting sum area from pres wetland cover;
# this makes the uncertainty band expand further back in time
sum_overlap_mod <- sum_overlap %>%
  
  # sum overlap of each LU
  group_by(year, overlap) %>%
  summarise(sum_overlap = sum(sum_overlap)/ 10^6) %>%
  ungroup() %>%
  
  # group by overlap
  group_by(overlap) %>%
  arrange(overlap, year) %>%
  
  # calculate the cumulative departure from rdm scenario, for avoid & pref
  mutate(lu_exp = sum_overlap - lag(sum_overlap)) %>%
  mutate(lu_exp = ifelse(is.na(lu_exp), 0, lu_exp)) %>%
  
  mutate(lu_exp_cum = cumsum(lu_exp)) %>%
  mutate(lu_exp_cum = max(lu_exp_cum) - lu_exp_cum) %>%
  
  # combine rdm overlap, with the deviation of pref & avoid in a single column
  mutate(comb = ifelse(overlap=="rdm", max(sum_overlap) - sum_overlap + 12, lu_exp_cum)) %>%
  ungroup() %>%
  
  dplyr::select(-lu_exp, -sum_overlap, -lu_exp_cum) %>%
  #mutate(comb= comb + 12) %>%
  spread(overlap, comb) %>%
  
  mutate(pref_inv_overlap = rdm + pref,
         avoid_inv_overlap = rdm - avoid)


# /----------------------------------------------------------------------------#
#/   
ggplot(sum_overlap_mod) +
  
  # plot nat wet cover line
  geom_line(aes(x=year, y= rdm), size=0.4) +
  geom_ribbon(aes(x=year, ymin= avoid_inv_overlap, ymax=pref_inv_overlap), size=0.4, alpha=0.2) +
  
  # axis labels
  xlab("Year") +  ylab(expression(paste("Wetland area (10"^{6},' km'^{2},")"))) +
  
  #facet_rep_wrap(.~ overlap, ncol=1) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 20)) +
  
  line_plot_theme +
  theme(#legend.position = c(0.2, 0.3), #"none",
    plot.margin = margin(1,3,1,1,"mm")) #+ ggtitle("Cropland")


# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(#plot=wetchimplot, 
  "./output/figures/lineplot_overlap_timeline_wribbon_flipped.png",
  dpi=300, width=90, height=90, units='mm' , type = "cairo-png")

dev.off()
