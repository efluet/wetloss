library(tidyverse)
library(ggplot2)
library(here)
here()


df <- read.csv("../docs/wetloss_estimates.csv") %>%
  filter(Method != 'Satellite') %>% 
  mutate(Method = str_wrap(Method, width = 6, indent = 0, exdent = 0)) %>%
  mutate(Method = factor(Method, levels=c("Our\nreconstruction", "Geospatial\noverlay", "Meta-\nanalysis" ))) %>%
  mutate(plot_group = paste(Source, Period)) %>% 
  # mutate(Period = factor(Period, levels=c("Since 1700", "Since 1900", "Since 1970", "Cumulative to present"))) %>%
  # mutate(Period = factor(Period, levels=c("Since 1970", "Since 1900",  "Since 1700", "Cumulative to present"))) %>%
  
  # arrange(order) %>%
  arrange(desc(order)) %>%
  as_tibble() %>% 
  mutate(upper_bound = ifelse((!is.na(Wetland.Loss) & is.na(upper_bound)), Wetland.Loss, upper_bound),
         lower_bound = ifelse((!is.na(Wetland.Loss) & is.na(lower_bound)), Wetland.Loss, lower_bound))



# /----------------------------------------------------------------------------#
#/  Make barplot

ggplot(df) +
  # bars
  geom_bar(aes(x=Method, y=Wetland.Loss, fill=Method, group=order),
           width=0.4, position = position_dodge(width=0.5), stat="identity") +

  # Higher error bar in same color as bar
  geom_errorbar(aes(x=Method, ymin=upper_bound, ymax=lower_bound, 
                    color=Method, group=order), 
                position=position_dodge(.5), width=0, size=0.35) +
  
  # Lower error bar in white
  geom_errorbar(aes(x=Method, ymin=Wetland.Loss, ymax=lower_bound, group=order), color='white',
                position=position_dodge(.5), width=0, size=0.35) +
  
  # Add 'since when?' labels
  geom_text(aes(x=Method, y=upper_bound+15, group=order,label=Period), 
           position = position_dodge(width=0.5),
           size=2,
           stat="identity", 
           vjust="center",
           hjust="left") +
  

  scale_fill_hue(c = 100) +
  scale_color_hue(c = 100) +
  # scale_fill_brewer(palette='Set1') +
  # scale_color_brewer(palette='Set1') +
  scale_x_discrete(position = "top") +
  scale_y_reverse(position = "right", expand=c(0,0), limits=c(119, 0), breaks=seq(-0, 100, 25)) +
  coord_flip() +
  
  ylab("Global wetland loss (%)") +
  xlab("") +
  
  line_plot_theme +
  theme(legend.position = "none",
        axis.text = element_text(color='black', size=7),
        axis.line.x = element_blank(),
        axis.line.y = element_line(color='black', size=0.2),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line(size=0.1, color='grey85')) 

# /-----------------------------------------------------------------------#
#/ Save 
ggsave('../output/figures/lit_wetloss/lit_wetloss_comparison_werrorbars_v2.pdf',
       width=90, height=55, dpi=800, units="mm") #type = "cairo-png")
dev.off()

