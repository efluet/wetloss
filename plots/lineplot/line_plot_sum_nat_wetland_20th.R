

i <- "./output/results/natwet/global_sum_nat_wetland_20th.csv"
sum_nat_wet_20th <- read.csv(i, stringsAsFactors = F) %>%
                    gather(year_type, year, year_end:year_start) %>%
                    dplyr::select(-one_of('year_type'))


# plot =========================================================================

# declare breakpoints
mybreaks <- c(1901, 1932, 1993, 2004, 2010)

ggplot(sum_nat_wet_20th) +
  geom_line(aes(x=year, wet_Mkm2, color=name), size=2) +
  scale_x_continuous(breaks=mybreaks, labels=mybreaks) +
  xlab("Year") +
  ylab("Natural wetland area (10^6 km2)")




### save plot ------------------------------------------------------------------
ggsave("../../output/figures/line_plot_sum_nat_wet_20th.png",
       width=178, height=120, dpi=600, units='mm', type = "cairo-png")

dev.off()
