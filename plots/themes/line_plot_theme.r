
line_plot_theme <- 
  theme_bw() +
  theme(
    
      ### ALL TEXT (inherited everywhere)
      text = element_text(size=9, colour='black'),
      
      ### FACET STRIP
      strip.text = element_text(size=9, face='bold',hjust= 0), #, vjust = -0.5),
      strip.background = element_blank(),
      
      ### LEGEND
      legend.text = element_text(size = 9),
      legend.background = element_blank(),
      legend.key.size = unit(4, "mm"),
      legend.title=element_blank(),
      #legend.position = 'top',
      legend.direction = 'vertical',
      legend.justification = "left",
      
      
      ### AXES
      axis.line  = element_line(colour = "black", size=0.3),
      axis.text  = element_text(size=7, colour='black'),
      axis.ticks = element_line(colour='black', size=0.3), 
      
      
      ### PANEL
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      # panel.background = element_rect(fill=NA, colour = "black", size=0.1),
      panel.spacing = unit(.05, "lines"),
      panel.border = element_blank()) #rect(color = "black", fill = NA, size = 0.1))
