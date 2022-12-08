# Description:  Plot wetloss per watershed

# /------------------------------------------
#/  Get river basins

# get river basin IDs
rivbasin <- raster("../data/ISLSCP_RIVER_ROUTING_1005/data/stn_basin_id_1d.asc")
rivbasin[rivbasin > 35] <- NA
plot(rivbasin)

rivbasindat <- read.csv("../data/ISLSCP_RIVER_ROUTING_1005/data/stn_basin_attribute_1d.csv")



### Zonal stat of potential wetland per basin
zstatout <- as.data.frame(zonal(potwet, rivbasin, 'sum'))
names(zstatout)<-c("zone","sum_potwet")
#zstatout <- data.frame(ID = c(-88, seq(1,1033)))

# Loop through variables and calculate overlap
for(v in var_ls_lu){
  
  assign(eval(paste0(v, "_loss_perbasin")),
         as.data.frame(zonal(get(paste0(v, "_wet_rdm")), rivbasin, 'sum')))
  
  # rename the basin;
  assign(eval(paste0(v, "_loss_perbasin")), 
         get(paste0(v, "_loss_perbasin")) %>% setNames(c("zone",paste0(v, "_loss"))) )
  
  # Append to ; Only appends the 2nd column (to avoid dupl zone col)
  zstatout <- bind_cols(zstatout, get(paste0(v, "_loss_perbasin"))[2])
}


glimpse(zstatout)
# # Zonal stat of loss per basin
# loss_perbasin   <- as.data.frame(zonal(sum_lu_rdm, rivbasin, 'sum')) 
# names(loss_perbasin)<-c("zone","sum_loss")
# 
# # Zonal stat of potential wetland per basin
# potwet_perbasin <- as.data.frame(zonal(potwet, rivbasin, 'sum')) 
# names(potwet_perbasin)<-c("zone","sum_potwet")


# Join Zonal stat and calculate % loss
perbasin <- zstatout %>%
  mutate(sum_loss = rowSums(.[3:7])) %>%
  gather(key = "lu", value = "loss", cropland_loss:uopp__loss) %>%
  mutate(perc_loss= loss/sum_potwet * 100) %>%
  
  left_join(., rivbasindat, by=c("zone"="ID")) %>%
  group_by(Name) %>%
  filter(sum_potwet > 10^5) %>%
  top_n(10, sum_loss)


glimpse(perbasin)


# Plot
ggplot(perbasin) +
  geom_bar(aes(x=reorder(Name, perc_loss), y=perc_loss, fill=lu), width=0.7, stat="identity") +
  coord_flip() + 
  line_plot_theme +
  
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(expand= c(0,0), limits = c(0,100)) +
  
  xlab("") +
  ylab(expression(paste("Wetland area lost (10"^{6},' km'^{2},")"))) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color="grey80", size=0.4),
        legend.position = c(0.5, 0.1 ))
# labs(title = "Top 10 Wetland Loss Per Watershed",
#      subtitle = "",
#      caption = "Only basins > 10,000 km^2 original wetland area")


# /-----------------------------------------------------------------------------
#/    Save plot 
ggsave(
  "./output/figures/per_basin/barplot_lu_loss_perbasin.png",
  width=90, height=90, dpi=500, units='mm' , type = "cairo-png")

dev.off()

