

# /----------------------------------------------------------------------------#
#/     Get file                                                     ------------

wetchimp_area_sum <- read.csv("./output/results/natwet/sum_wetchimp_grids.csv")


wetchimp_area_sum$experiment <- factor(wetchimp_area_sum$experiment, levels = c(1, 2, 3))


# ADD EMPTY COLUMN
# o <- as.data.frame(table(wetchimp_area_sum$experiment, 
#                          wetchimp_area_sum$model,
#                          wetchimp_area_sum$extension))
# o$Freq <- NULL
# names(o) <- c("experiment", "model", "extension")
# wetchimp_area_sum <- bind_rows(wetchimp_area_sum, o)


# /----------------------------------------------------------------------------#
#/     Make figure of Wetchimp SUMs                                   ------------

#wetchimplot <- 
ggplot(wetchimp_area_sum) +
  geom_bar(aes(x=extension, y= wet_Mkm2, fill= experiment), 
           color='black', size=0.2, width= 0.8, position="dodge", stat="identity") +
  
  facet_wrap(~ model, scales="free") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_discrete(labels = c(" Exp.1-Equilibrium (1901-1931)", 
                                 " Exp.2-Transient (1993-2004)",
                                 " Exp.3-Optimal (1993-2004)")) +
  coord_flip() +
  
  xlab("") +
  ylab(expression(paste("Global wetland area (10"^{6},' km'^{2},")"))) +
  line_plot_theme +
  theme(panel.grid.major.x = element_line(color="grey80", size=0.2),
        legend.position = c(0.6, 0.2))


#wetchimplot

# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(#plot=wetchimplot, 
  "./output/figures/barplot_wetchimplot_wetarea_modelfacet.png",
  dpi=300, width=180, height=100, units='mm' , type = "cairo-png")

dev.off()
