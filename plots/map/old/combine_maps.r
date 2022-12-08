
# function of lpx mask
source('./scripts/r/plots/fcn/make_lpx_mask_overalltimes.r')

# set legend position in maps
l_pos <- c(0.18, 0.4)


# map baseline wetland cover
source('./scripts/r/plots/map_natwet_in6000bc.r')
source('./scripts/r/plots/map_natwet_in1700.r')


# map wetloss rate (km^2 year^-1)
# source('./scripts/r/plots/map_remloss_rate_since6000bc.r')
# source('./scripts/r/plots/map_remloss_rate_since1700.r')


#  revised by using wetland % change
source('./scripts/r/plots/map_perc_remwet_since1700.r')
source('./scripts/r/plots/map_perc_remwet_since6000bc.r')


# MAP period of max wetloss
source('./scripts/r/plots/map_period_max_wetloss_rate_since_6000bc.r')
source('./scripts/r/plots/map_period_max_wetloss_rate_since_1700.r')




c <-  plot_grid(map_natwet_in6000bc, map_natwet_in1700ad,
                map_remloss_rate_since6000bc, map_remloss_rate_since1700,
                map_max_wetlossrate_since6000bc, map_max_wetlossrate_since1700,
                ncol=2, nrow=3, align="hv",
                labels=c("A","B","C","D","E","F"))



### Save figure to file --------------------------------------------------------
ggsave('./output/figures/maps_combined_aligned_v10.pdf', c,
       width=178, height=150, dpi=600, units="mm")#, type = "cairo-png")
dev.off()

### Save figure to file --------------------------------------------------------
ggsave('./output/figures/maps_combined_aligned_v10.png', c,
       width=178, height=150, dpi=600, units="mm")#, type = "cairo-png")

dev.off()
