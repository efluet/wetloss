# Plo comparison vs paudel


# Read csv
df <- read.csv('../docs/wetlossperc_vspaudel2016.csv')



# /----------------------------------------------------------------------------#
#/ Make plot
paudel_scatter <- ggplot() +
  
  # make 1:1 line
  geom_abline(slope=1, intercept=0, color='grey85', size=0.3) +
  
  # points labeled by continent/period
  geom_point(data=df,
             aes(x=Paulel2016_perclosses, 
                 y=Percentage.loss.in.each.country....,
                 size=Wetland.area.in.2020..Mkm2.), 
                 # fill=continent, 
                 # size=areapoly_mkm2), #remwet_end/1000),  # areapoly_mkm2
             shape=21, fill='dodgerblue2', color='black', stroke=0.35, alpha=0.65) +
  
  # geom_text(aes(x=0, y=95), size=2.5, hjust=0, label= agg_label) +
  # 
  # histcase label
  geom_text_repel(data = df, #subset(cs_joined, label %in% labels_ls),
                  aes(x=Paulel2016_perclosses, 
                      y=Percentage.loss.in.each.country....,
                      # color=continent,
                      label = Country),
                  color='grey30',
                  segment.color='grey30',
                  size = 2.0,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(1, 'mm'),
                  point.padding = unit(1, 'mm')) +
  
  # axis labels
  xlab('Wetland loss percentage (%) since pre-industrial \n from Paudel et al. 2016') + 
  ylab('Reconstructed wetland loss percentage (%) since 1700\nfrom Fluet-Chouinard et al.') +
  
  # axis limits
  scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(limits=c(0, 100)) + 
  
  scale_shape_manual(values=c(21)) +
  scale_fill_brewer(palette = 'Set1', name = 'Continent') +
  scale_color_brewer(palette = 'Set1', guide='none') +
  
  scale_size_continuous(name = expression(paste('Wetland area from WAD2M (', 10^{6}, ' ', km^{2}, ')')),
                        breaks = c(0.05, 0.1, 0.5, 5),
                        # limits = c(0.1, 18),
                        labels = c('0.05', '0.01', '0.5', '5'),
                        range = c(0.3, 14)) +
  
  guides(fill=guide_legend(ncol=2, override.aes = list(size=6), title.position='top'),
         size=guide_legend(ncol=1, title.position='top')) +
  
  # fixed axis ratio
  coord_fixed() +
  line_plot_theme +
  theme(panel.background = element_rect(color='black', size=0.5, fill=NA),
        legend.position = 'top',#c(0 , 1.3), # c(0.05, 0.9),#'top',
        legend.title=element_text(),
        legend.direction = 'horizontal',
        plot.margin=unit(c(10, 1, 3, 1), 'mm'))


# paudel_scatter


ggsave('../output/figures/scatterplot_vspaudel2016.pdf', paudel_scatter,
       width=130, height=150, dpi=700, units='mm')
# 
# dev.off()

