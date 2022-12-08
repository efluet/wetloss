# Get datapoints for drainage (highly processed)
d <- read.csv('../output/results/artif_drainage/drained_data_fullproc_forfig1.csv', stringsAsFactors=F)
# Filter to only keep national data
# d_nat <- d %>% filter(region=='')
d_wc_forplot  # Use this df from the figure 1.5 (now fig S1)


d_wc_forplot <- read.csv('../output/results/final_data/drainage_db/drainage_db_v10.csv') %>% 

  mutate(country_name = countrycode(iso_a3, "iso3c", "country.name")) %>%
  mutate(continent = countrycode(iso_a3, "iso3c", "region")) %>%
  
  # mutate(continent = countrycode(country_name, "country.name", "region")) %>%
  mutate(continent = ifelse(continent %in% c("Central America","South America", "Caribbean"),"Central & South America",continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Europe","Southern Europe", "Northern Europe"), "Western Europe", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Asia","Central Asia"), "Western & Central Asia", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Northern America"), "North America", continent)) %>%
  mutate(continent = ifelse(continent %in% c("Western Africa", "Southern Africa","Eastern Africa","Middle Africa"), "Africa", continent)) %>%
  # lump australia & NZ to same as Europe
  mutate(continent = ifelse(country_name %in% c('Australia','New Zealand'), "Europe & Central Asia", continent)) %>% 
  filter(!is.na(continent))




#  /---------------------------------------------------------------------------#
# /  NATIONAL TIME

# subset to national
d_wc_forplot_nat <- d_wc_forplot %>% 
                    filter(unit=='National')

# Make plot
ts_nat <- 
  ggplot(d_wc_forplot_nat) +
  geom_histogram(aes(x=year, fill=type), color='white', size=0.01, binwidth = 20) +
  
  geom_histogram(data = transform(d_wc_forplot_nat, continent = "All"), 
                 aes(x=year, fill=type), color='white', size=0.01, binwidth = 20) +
  
  facet_wrap(.~continent, scales="free", ncol=3) +
  
  scale_x_continuous(limits=c(1700, 2040), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks()) +
  line_plot_theme +
  # expand_limits(y=0, x=0) +
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#CC3311',
        'Wetland Cultiv.' = '#33BBEE',
        'Forestry'   = '#228833',
        'Peat Extraction' = '#EE7733',
        'Rice'     = '#AA4499',
        'Pasture'  = '#DDCC77',
        'Urban'    = '#332288'),
    name="Driver of\nwetland loss") +
  
  xlab('')+
  ylab('Number of national drainage record') +
  theme(legend.position = c(0.8, 0.2),#'none',
        legend.direction = 'vertical',
        axis.ticks = element_line(colour='black'),
        plot.margin=unit(c(1, 9, -1, 1), "mm"),
        panel.spacing = unit(6, "mm"),
        strip.text = element_text(hjust= 0, vjust = -1),
        legend.key = element_rect(fill='black'))
  
ts_nat

# /-----------------------------------------------------------------------------
#/ save plot 
ggsave(plot = ts_nat,  "../output/figures/artif_drainage/timedistribut_natdrainstat_v2.pdf",
       width=190, height=100, dpi=600, units='mm')

ggsave(plot = ts_nat,  "../output/figures/artif_drainage/timedistribut_natdrainstat_v2.png",
       width=190, height=100, dpi=600, units='mm')

dev.off()



#  /---------------------------------------------------------------------------#
# /  SUBNATIONAL TIME

# Subset to subnational
d_wc_forplot_subnat <- 
  d_wc_forplot %>% 
  filter(unit!='National') %>% 
  mutate(year = ifelse(is.na(year), 2000, year))
  

# Make plot
ts_subnat <-
  ggplot(d_wc_forplot_subnat) +
  geom_histogram(aes(x=year, fill=type), color='white', size=0.01, binwidth = 20) +
  
  geom_histogram(data = transform(d_wc_forplot_subnat, continent = "All"), 
                 aes(x=year, fill=type), color='white', size=0.01, binwidth = 20) +
  
  facet_wrap(.~continent, scales="free", ncol=3) +
  
  scale_x_continuous(limits=c(1700, 2040), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks()) +  
  line_plot_theme +
  
  scale_fill_manual(#labels = driver_names,
    values =
      c('Cropland' = '#CC3311',
        'Wetland Cultiv.' = '#33BBEE',
        'Forestry'   = '#228833',
        'Peat Extraction' = '#EE7733',
        'Rice'     = '#AA4499',
        'Pasture'  = '#DDCC77',
        'Urban'    = '#332288'),
    name="Driver of\nwetland loss") +
  
  
  xlab('')+
  ylab('Number of subnational drainage record') +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.ticks = element_line(colour='black'),
        plot.margin=unit(c(1, 9, -1, 1), "mm"),
        panel.spacing = unit(6, "mm"),
        strip.text = element_text(hjust= 0, vjust = -1),
        legend.key = element_rect(fill='black'))

ts_subnat

# /-----------------------------------------------------------------------------
#/ save plot 
ggsave(plot = ts_subnat,  "../output/figures/artif_drainage/time_distribution/timedistribut_subnatdrainstat_v3.pdf",
       width=190, height=95, dpi=600, units='mm')

ggsave(plot = ts_subnat,  "../output/figures/artif_drainage/time_distribution/timedistribut_subnatdrainstat_v3.png",
       width=190, height=90, dpi=600, units='mm')


dev.off()



# /----------------------------------------------------------------------------#
#/  Print vals for paper's methods section

mean(d_wc_forplot_nat[,'year'], na.rm=T)
sd(d_wc_forplot_nat[,'year'], na.rm=T)


mean(d_wc_forplot_subnat[,'year'], na.rm=T)
sd(d_wc_forplot_subnat[,'year'], na.rm=T)


