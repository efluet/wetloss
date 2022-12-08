# /----------------------------------------------------------------------------#
#/ Barplot of value types in the time-series           ------

#  Read in data
die <- read.csv('../output/results/artif_drainage/drained_wetcult_gridded_sum_fullserial_dec2019.csv') %>%
# die <- read.csv("../output/results/artif_drainage/drained_wetcult_gridded_sum.csv") %>% 
#die <- read.csv("./output/results/artif_drainage/drained_wetcult_gridded_sum_serial.csv") %>% 
  mutate(lu_type= stri_trans_totitle(lu_type)) %>% 
  mutate(lu_type= ifelse(lu_type=='Ir_rice', 'Rice', lu_type),
         lu_type= ifelse(lu_type=='Uopp_'  , 'Urban', lu_type),
         lu_type= ifelse(lu_type=='Forest_harv' , 'Forest', lu_type)) 

               # group_by(lu_type, year) %>%
               # dplyr::summarize(pred_drained = sum(pred_drained)) %>%
               # ungroup() %>%
  
die$lu_type_fac <- factor(die$lu_type, levels=c('Cropland',
                                                    'Forestry',
                                                    'Pasture',
                                                    'Peatland',
                                                    'Urban',
                                                    'Rice',
                                                    'Wetland Cultiv.'))

# Calculate loss rate
die_loss_rate<- die %>% 
                group_by(year) %>% 
                summarise(cumul_drain_km2 = sum(cumul_drain_km2)) %>% 
                ungroup() %>% 
                mutate(lossrate = (cumul_drain_km2 - lag(cumul_drain_km2)) / 10)



# /----------------------------------------------------------------------------#
#/     Bar plot of LU drain or conversion over time       ----------
barplot_lutype <- 
  
  ggplot(die) +
  
  geom_area(aes(x=year, y=cumul_drain_km2/10^6, fill=die$lu_type_fac), color='white', size=0.05) +
  
  scale_y_continuous(expand=c(0,0),
                     # limits= c(0, 3000), 
                     labels = c(0, 1, 2, 3), 
                     breaks = c(0, 1, 2, 3)) +
  
  scale_x_continuous(expand=c(0,0), 
                     labels = seq(1700,2000,100), 
                     breaks= seq(1700,2000,100)) +
  
  scale_fill_manual(#labels = driver_names,
                    values =
                      c('Cropland' = '#ff5b4f',  # Cropland
                        'Wetland Cultiv.'  = '#507dc7',  # Wetcultiv - blue
                        'Forestry'   = '#8df0a9',  # Forestry
                        'Peatland' = 'brown',    # Peatland
                        'Rice'     = '#a177e0',  # Irrig Rice
                        'Pasture'  = '#95f5f0',  # Pasture
                        'Urban'    = '#e0dd6e'), # Urban
                    name="Driver of\nwetland loss") +
  
  ylab(expression(paste("Drained or converted area (10"^{6},' km'^{2}," )" ))) +
  xlab("") +
  
  line_plot_theme +
  
  theme(legend.position = c(0.02, 0.65)) 

barplot_lutype



# /----------------------------------------------------------------------------#
#/  Line plot of LU-potwet overlap                                         -----

library(lemon)

# write summed area of wetloss, remaining, etc... to output
sum_overlap <- read.csv( '../output/results/wetloss/sum/sum_wet_overlap_3subgrid.csv') 


# prep data - inverting sum area from pres wetland cover;
# this makes the uncertainty band expand further back in time
sum_overlap_mod <- sum_overlap %>%
  
  # sum overlap of each LU
  group_by(year, overlap) %>%
  summarise(sum_overlap = sum(sum_overlap)/ 10^6) %>%
  ungroup() %>%
  
  # group by overlap
  group_by(overlap) %>%
  arrange(overlap, year) %>%
  
  # calculate the cumulative departure from rdm scenario, for avoid & pref
  mutate(lu_exp = sum_overlap - lag(sum_overlap)) %>%
  mutate(lu_exp = ifelse(is.na(lu_exp), 0, lu_exp)) %>%
  
  mutate(lu_exp_cum = cumsum(lu_exp)) %>%
  mutate(lu_exp_cum = max(lu_exp_cum) - lu_exp_cum) %>%
  
  # combine rdm overlap, with the deviation of pref & avoid in a single column
  mutate(comb = ifelse(overlap=="rdm", max(sum_overlap) - sum_overlap + 12, lu_exp_cum)) %>%
  ungroup() %>%
  
  dplyr::select(-lu_exp, -sum_overlap, -lu_exp_cum) %>%
  
  spread(overlap, comb) %>%
  
  mutate(pref_inv_overlap = rdm + pref,
         avoid_inv_overlap = rdm - avoid)



# /----------------------------------------------------------------------------#
#/   Prepare data from drainagee per LU type for plotting                 -----
sum_die <- die %>%
          group_by(year) %>%
          dplyr::summarize(sum_area_1000km2 = sum(cumul_drain_km2)/10^6) %>%
          ungroup() %>%
          mutate(wetarea = max(sum_area_1000km2) - sum_area_1000km2 + 12)


# /----------------------------------------------------------------------------#
#/   Make lineplot                             ------
wetarea_lineplot <- 
  
  ggplot(sum_overlap_mod) +
  
  # plot nat wet cover line
  geom_line(aes(x=year, y= rdm), size=0.4) +
  geom_ribbon(aes(x=year, ymin= avoid_inv_overlap, ymax=pref_inv_overlap), size=0.4, alpha=0.2) +
  
  geom_line(data= sum_die,
            aes(x=year, y= wetarea), size=0.4, color="red") +
  
  
  # axis labels
  xlab("Year") +  ylab(expression(paste("Natural wetland area (10"^{6},' km'^{2},")"))) +
  
  #facet_rep_wrap(.~ overlap, ncol=1) +
  
  # axis scales
  scale_x_continuous(expand=c(0,0), labels = seq(1700,2000,100), breaks= seq(1700,2000,100)) +
  scale_y_continuous(expand=c(0,0), limits=c(8, 18), labels = seq(10, 20, 2), breaks= seq(10,20,2)) +
  
  line_plot_theme +
  theme(#legend.position = c(0.2, 0.3), #"none",
    plot.margin = margin(1,3,1,1,"mm")) #+ ggtitle("Cropland")

wetarea_lineplot

# /----------------------------------------------------------------------------#
#/
ggsave('../output/figures/fig1B_drain_wetarea_forJPL.png',
       width=90, height=80, dpi=800, units="mm") #type = "cairo-png")
dev.off()



# /----------------------------------------------------------------------------#
#/    Make multipanel plot                                               ------

# set tight margins so plots are close side-by-side
barplot_lutype   <- barplot_lutype   + theme(plot.margin=unit(c(3, 3, 1, 3), "mm"))
wetarea_lineplot <- wetarea_lineplot + theme(plot.margin=unit(c(3, 3, 1, 3), "mm"))


# arrange plots grob into layout 
library(ggpubr)
p <- ggarrange(barplot_lutype, wetarea_lineplot,
               ncol=1, labels = c("A", "B"),
               align="hv")


# /----------------------------------------------------------------------------#
#/    Save figure to file          --------

ggsave('./output/figures/fig1AB_drain_wetarea_plots_v4.png', p,
       width=90, height=130, dpi=800, units="mm") #type = "cairo-png")
dev.off()






# /----------------------------------------------------------------------------#
# #/  RemWet AREA Mkm^2 plot                                               -------
# 
# ggplot(sum_overlap) +
#   
#   # plot nat wet cover line
#   geom_area(aes(x=year, y= sum_overlap/10^6, fill=lu,  group=lu), color="white", size=0.4, position = "stack") +
#   # axis lables
#   xlab("Year") +  ylab(expression(paste("Wetland area (10"^{6},' km'^{2},")"))) +
#   
#   facet_rep_wrap(.~ overlap, ncol=1) +
#   
#   # axis scales
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   
#   line_plot_theme +
#   theme(#legend.position = c(0.2, 0.3), #"none",
#     plot.margin = margin(1,1,1,1,"mm")) #+ ggtitle("Cropland")
