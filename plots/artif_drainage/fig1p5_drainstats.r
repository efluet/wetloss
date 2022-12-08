# /-----------------------------------------------------------------------------
#/  Get wetcult data & reconstruction

drainage_wetcult <- 
  read.csv('../output/results/artif_drainage/drained_wetcult_sigmoid_interp_comb_march2021.csv') %>% as_tibble() %>% 
  group_by(country_name, type) %>% 
  mutate(max_pred_drained = max(pred_drained, na.rm=T)) %>%
  ungroup() %>% 
  mutate(type = str_to_title(type)) %>% 
  filter((  type == "Cropland" & max_pred_drained > 60000) |
           (type == "Forestry" & max_pred_drained > 5000) |
           (type == "Peat Extraction" & max_pred_drained > 750) |
           (type == "Wetland Cultiv." & max_pred_drained > 3500))



# Get data points for Wetland cultiv 
wc <- read.csv("../output/results/artif_drainage/drained_wetcult_km2_onlydata.csv") %>% as_tibble() %>% 
  rename(drained_area_tot = drained_area_irrig) %>% 
  mutate(type = "Wetland Cultiv.") %>% 
  mutate(type = str_to_title(type))


# Get datapoints for drainage (highly processed)
d <- read.csv('../output/results/artif_drainage/drained_data_fullproc_forfig1.csv', stringsAsFactors=F)
# Filter to only keep national data
d_nat <- d %>% filter(region=='')


# /----------------------------------------------------------------------------#
#/  Bind point and line data 
# Bind data points from drainage data and wetcultiv
d_wc_forplot <- bind_rows(d_nat, wc)

# Bind data points to time-series df
d_wc_forplot <- semi_join(d_wc_forplot, drainage_wetcult, by=c("country_name"="country_name", "type"="type"))
d_wc_forplot <- as_tibble(d_wc_forplot)

###Add
d_wc_forplot <- d_wc_forplot %>% 
  mutate(type_label=ifelse(type=='Cropland', 'Cropland\nSigmoid fit\n(11 countries >50 10^3 km^2)' , NA), 
         type_label=ifelse(type=='Forestry', 'Forestry\nSigmoid fit\n(shown: 5 countries >5 10^3 km^2)' , type_label),
         type_label=ifelse(type=='Peat Extraction', 'Peat Extraction\nSigmoid fit \n(8 countries >0.5 10^3 km^2)' , type_label),
         type_label=ifelse(type=='Wetland Cultiv.', 'Wetland Cultivation\nConstant fraction\n(8 countries >2.5 10^3 km^2)' , type_label))


# /----------------------------------------------------------------------------#
#/  Make lineplot stacked of multiple countries
m <- 
  ggplot() +
  
  # Interpolated line
  geom_line(data=drainage_wetcult, aes(x= year, y= pred_drained/1000, color=country_name,  group=country_name)) +
  
  # Data points
  geom_point(data=d_wc_forplot, aes(x= year, y= drained_area_tot/1000, color=country_name), size=.5) +  #  shape=21, fill='white') +
  
  # Country labels
  geom_text(data=subset(drainage_wetcult, year==2020), 
            aes(x=2030, y=pred_drained/1000, label = country_name, color=country_name), 
            size= 2.2, vjust=0.5, hjust=0) +
  
  facet_wrap(~type, scales="free", ncol=2) +
  
  line_plot_theme +
  
  expand_limits(y=0) +
  scale_x_continuous(limits = c(1700, 2080), expand=c(0,1)) +
  scale_y_continuous(expand=c(0.01,0), breaks=pretty_breaks()) +
  # scale_color_brewer(palette='Set2') +
    
  xlab("") + ylab(expression(paste("Cumulative area drained or converted (10"^{3},' km'^{2},")")))  +
  
  
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        axis.ticks = element_line(colour='black'),
        plot.margin=unit(c(1, 9, -1, 1), "mm"),
        panel.spacing = unit(6, "mm"),
        strip.text = element_text(hjust= 0, vjust = -1),
        legend.key = element_rect(fill='black'))

m


# /-----------------------------------------------------------------------------
#/ save plot 
ggsave(plot = m,  "../output/figures/artif_drainage/sigmoid/stacked/drainstats_datainterpol_fig1_july2021.pdf",
       width=187, height=100, dpi=600, units='mm')

ggsave(plot = m,  "../output/figures/artif_drainage/sigmoid/stacked/drainstats_datainterpol_fig1_july2021.png",
       width=187, height=100, dpi=600, units='mm')

dev.off()



## get counts
counts <- d_wc_forplot %>%  
          # drainage_wetcult %>% 
          group_by(type) %>%
          summarise(n=n_distinct(country_name))

counts

# label
# geom_text_repel(data = subset(drainage_wetcult, year==2000),
#                 aes(x=year+25, y=pred_drained/1000, label = country_name, color=country_name),
#                 segment.color='grey25',
#                 size = 3.0,
#                 nudge_x = 10,
#                 segment.size = 0.25,
#                 box.padding = unit(0.1, 'mm'),
#                 point.padding = unit(0.1, 'mm')) +