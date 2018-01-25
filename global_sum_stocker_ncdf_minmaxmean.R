rm(list=ls()) # clear memory
library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyr)

# set wd
setwd('C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/scripts/r')


# open connection to netcdf file
t <- '../../data/nat_wetland_map/trace21_129.cdf'
nc <- nc_open(t)

# get time labels
year <-  c(unique(ncvar_get( nc, attributes(nc$dim)$names[4]))) 
year_sub <- seq(1, length(year), 10) # subsample the timeseries

# subset specific variables
nc_inund <- ncvar_get(nc, 'inund') #subset to inund var
gridarea <- ncvar_get(nc, 'area') #subset to area var
nc_luarea <- ncvar_get(nc, 'lu_area') #subset to area var


# create empty df for output
output_df <- data.frame(year=numeric(),
                        sum_inund_mean_Mkm2=numeric(),
                        sum_inund_max_Mkm2=numeric(),
                        sum_inund_min_Mkm2=numeric(),
                        sum_peatland_Mkm2=numeric())


# loop through subset of years
for (y in year_sub){
  
  # get peatland area
  peatland_f <- nc_luarea[ , ,2,y]  # 3rd dimension is lu type: k=2 peatlands
  peatland_a <- gridarea * peatland_f # get inundated area 
  sum_peatland_Mkm2 <- sum(peatland_a, na.rm = TRUE) * 10^-6 * 10^-6  # sum to Mkm2
  
  # get inund area

  inund_mean_f <- apply(nc_inund[ , , , y], c(1,2), mean) # get elementwise mean value
  inund_max_f <- apply(nc_inund[ , , , y], c(1,2), max) # get elementwise max value
  inund_min_f <- apply(nc_inund[ , , , y], c(1,2), min) # get elementwise min value
  
  inund_mean_a <- gridarea * inund_mean_f # get inundated area 
  inund_max_a <- gridarea * inund_max_f # get inundated area 
  inund_min_a <- gridarea * inund_min_f # get inundated area 
  
  sum_inund_mean_Mkm2 <- sum(inund_mean_a, na.rm = TRUE)  * 10^-6 * 10^-6  # sum to Mkm2
  sum_inund_max_Mkm2 <- sum(inund_max_a, na.rm = TRUE)  * 10^-6 * 10^-6  # sum to Mkm2
  sum_inund_min_Mkm2 <- sum(inund_min_a, na.rm = TRUE)  * 10^-6 * 10^-6  # sum to Mkm2

  
  # append sums to df
  output_df <- rbind(output_df, 
                     data.frame(year = year[y],
                                sum_inund_mean_Mkm2 = sum_inund_mean_Mkm2,
                                sum_inund_min_Mkm2 = sum_inund_min_Mkm2,
                                sum_inund_max_Mkm2 = sum_inund_max_Mkm2,
                                sum_peatland_Mkm2= sum_peatland_Mkm2))
  }

# convert to long format
output_df2 <- output_df %>%
             gather(type, value, 
                    c(sum_inund_mean_Mkm2, sum_peatland_Mkm2)) %>%

             mutate(sum_inund_min_Mkm2= ifelse(type=='sum_peatland_Mkm2', NA, sum_inund_min_Mkm2)) %>%
             mutate(sum_inund_max_Mkm2= ifelse(type=='sum_peatland_Mkm2', NA, sum_inund_max_Mkm2)) %>%
             mutate(type= ifelse(type=='sum_peatland_Mkm2', 'Peatland', 'Inundated'))


# plot =========================================================================
ggplot(output_df2) +

  geom_ribbon(data=subset(output_df2, type!='Peatland'), 
              aes(x=year, ymin= sum_inund_min_Mkm2, ymax= sum_inund_max_Mkm2), fill= '#F8766D', alpha=0.3) +
  
  geom_line(aes(x=year, y=value, color=type), size=0.25) +
  
  
  ylab('Global area (km2 10^6)') +
  scale_x_continuous(limits=c(-10000, 2000), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,12), expand=c(0,0)) +

  theme_bw() +    # declare theme elements
  theme(legend.position = 'top',
        text = element_text(size=8, colour='black'),
        legend.background = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 7),
        legend.title=element_blank(),
        legend.justification = "right",
        axis.line = element_line(colour = "black", size=0.25),
        axis.text = element_text(size=7, colour='black'),
        axis.ticks = element_line(colour='black', size=0.2), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour='grey85', size=0.2), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        panel.background = element_blank())


# save figure to file
ggsave('../../output/figs/lpxdytop_global_inund_peatland_holocene_ribbon.png',  
       width=87, height=70, dpi=600, units="mm", type = "cairo-png")
dev.off()





# Qs
# how to deal with areas that are bot inundated and peatland?