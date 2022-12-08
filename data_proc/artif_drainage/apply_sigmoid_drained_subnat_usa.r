# /----------------------------------------------------------------------------#
#/     Apply the fit to countries with 3+ pts                               ----

# Get function that applies sigmoid
source('./data_proc/artif_drainage/fcn/fcn_apply_sigmoid_drainage.r')

# make list of years
y = seq(1700, 2020, 10) 

ifrm(predall)

# get unique cases, to then loop through
# ucases <- unique(d[,c('country_name','continent', 'type')])

d_subnat <- d %>% filter(region != '' & !is.na(year) & country_name=='United States')
ucases <- unique(d_subnat[,c('country_name','continent', 'region', 'type')])


# loop unique cases
for (i in 1:nrow(ucases)){
  
  c = as.character(ucases[i,'country_name'])
  t = as.character(ucases[i,'type'])
  r = as.character(ucases[i,'region'])
  o = as.character(ucases[i,'continent'])
  
  
  # subset data to unique case
  di = d_subnat %>% filter(country_name == c & type == t & region == r)
  
  # apply the sigmoid fit 
  pred <-  applysigmoid(di, fitall)
  pred$region <- r
  
  # Cap the drained area to the maximum data point
  pred[pred$pred_drained > max(di$drained_area_tot),'pred_drained'] <- max(di$drained_area_tot)
  
  # append to output
  if (!exist(predall)){predall <- pred} else{ predall <- bind_rows(predall, pred)}
}



# /----------------------------------------------------------------------------#
#/     Save predicted drained area

write.csv(predall, "../output/results/artif_drainage/drained_wetcult_ha_sigmoidpred_us_subnat.csv")




# /----------------------------------------------------------------------------#
#/     Plot subnational sigmoid curves and data points; facets lineplot per US state 

predall <- read.csv("../output/results/artif_drainage/drained_wetcult_ha_sigmoidpred_us_subnat.csv")


m <- ggplot() +
  geom_line(data = predall, aes(x=year, y= pred_drained, color=continent), size=0.4) +
  geom_point(data= d_subnat,  aes(x= year, y= drained_area_tot/10, color=continent), size=0.4) +
  expand_limits(y=0) +
  facet_wrap(~region, scales="free") +
  scale_y_continuous(breaks=pretty_breaks()) +
  line_plot_theme +
  ylab("Area drained (km^2)") + 
  xlab("") +
  theme(legend.position = 'none',
        strip.text = element_text(size = 7))
  



## save plot
ggsave(plot=m, "../output/figures/artif_drainage/sigmoid/template/drain_sigmoidtemplates_subnat_v2.png",
       width=280, height=185, dpi=400, units='mm' , type = "cairo-png")

dev.off()


########################################################
usa_crop_subnat_perc <- predall %>% 
                        group_by(year) %>% 
                        mutate(state_tot = sum(pred_drained)) %>% 
                        mutate(pred_drained_perc = pred_drained / state_tot *100) 
    

ggplot(usa_crop_subnat_perc) +
  geom_area(aes(x= year, y= pred_drained_perc, fill=region), position='stack', color='white', size=0.01)




# # /----------------------------------------------------------------------------#
# #/   Make plot that stacks all curves           ------
# predall <- predall %>%
#             group_by(country_name, type) %>%
#             mutate(max_pred_drained = max(pred_drained)) %>% 
#             mutate(perc_drained_ofmax = pred_drained/max_pred_drained) %>%
#             ungroup()
# 
# 
# # /----------------------------------------------------------------------------#
# #/     Plot facets
# m <- ggplot() +
#   geom_line(data = predall, aes(x= year, y= perc_drained_ofmax, group=country_name, color=continent)) +
#   #geom_point(data= d,      aes(x= year, y= drained_area_tot, color=type)) +
#   
#   #ggtitle(pred$country_name) +
#   expand_limits(y=0) +
#   facet_grid(continent~type, scales="free", space="free") +
#   #facet_rep_grid(continent~type, scales="free", label=label_both) +
#   
#   
#   line_plot_theme +
#   theme(legend.position = "none") + # c(0.9, 0.03)
#   ylab("Area drained (km^2)") + xlab("")
# 
# m
