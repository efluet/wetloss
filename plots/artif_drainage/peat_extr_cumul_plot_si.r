## First run data reading;  don't run all
source('./data_proc/artif_drainage/process_peat_extr_weight.r')


# /----------------------------------------------------------------------------#
#/   Country plot of annual peat extraction rates
p <- 
  ggplot() +
  # ggplot(drained_peatex) +
  geom_path(data=subset(drained_peatex, !is.na(country_name) & !is.na(drained_weight)),
            aes(x=decade, y=drained_weight/1000, group=country_name, color=country_name), na.rm=T, size=0.4) + 
  # Data points
  geom_point(data=subset(drained_peatex_dat,!is.na(country_name) & !is.na(drained_weight) & unit != 'subnational'), 
             aes(x=year, y=drained_weight, group=country_name, color=country_name),
             size=0.3, alpha=0.5) + 
  scale_x_continuous(limits=c(1700, 2020)) +
  ylab("Annual peat volume extracted (×1000 tonnes per year)") + xlab('') +
  facet_wrap(~country_name, scales='free', ncol=7) +
  line_plot_theme +
  theme(legend.position = 'none')

ggsave("../output/figures/artif_drainage/sigmoid/peat_extract/peat_extract_tonsyear_v6.png", p, 
       width=310, height=200, dpi=500, units='mm')

dev.off()


# /----------------------------------------------------------------------------#
#/    County plot of cumulative peat extraction
pc <- 
  ggplot(subset(drained_peatex_int, !is.na(country_name))) +
  geom_path(aes(x=decade, y=drained_weight_cumsum/1000, group=country_name, color=country_name), size=0.4) + 
  geom_point(aes(x=decade, y=drained_weight_cumsum/1000, group=country_name, color=country_name), size=0.3) + 
  scale_x_continuous(limits=c(1700, 2020)) +
  scale_y_continuous(breaks=pretty_breaks(n=4)) +
  ylab("Cumulative peat volume extracted (×1000 tonnes)") + xlab('') +
  facet_wrap(~country_name, scales='free', ncol=7) +
  line_plot_theme +
  theme(legend.position = 'none')

ggsave("../output/figures/artif_drainage/sigmoid/peat_extract/peat_extract_cumulvol_v6.png", pc, 
       width=310, height=200, dpi=500, units='mm')

dev.off()
