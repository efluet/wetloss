


library(gridExtra)
library(raster) 
library(rasterVis) 
library(latticeExtra) 
library(grid) 

# rename years
if(yr<0){
  yr_label <- paste0(yr*-1, 'BC')
} else {
  yr_label <- paste0(yr, 'AD')
}



# plot wetland extent =========================================
# 
# # plot glacier/submerged land
# wet_plt<- levelplot(glacier,
#                 margin = F, 
#                 col.regions='grey90',
#                 pretty=TRUE, 
#                 par.settings = mapTheme,
#                 main=paste0("Inundated percentage \n ", month.abb[m], "  ", yr_label),
#                 par.strip.text=list(cex=0.5, lines=10, fontface='bold'),
#                 axes=FALSE, box=FALSE,
#                 scales=list(draw=FALSE),
#                 xlab=NULL, ylab=NULL,
#                 colorkey=F)#list(space="bottom", draw=FALSE))


# wetland mapping theme
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))


cutpts <- seq(0, 1, 0.1) # set symbol cutoffs



wet_plt<- levelplot(wet, 
                       margin = list(draw=F), 
                       at=cutpts,
                       cuts=10, # nb of colors 
                       pretty=TRUE, 
                       #main=paste0("Natural wetland cover"),
                       par.strip.text= list(cex=0.5, lines=10, fontface='bold'),
                       axes=FALSE, box=FALSE,
                       scales=list(draw=FALSE),
                       xlab=NULL, ylab=NULL,
                       par.settings = mapTheme,
                       colorkey=list(space="right"),
                       xlim=c(-180, 180),
                       ylim=c(-70, 90)) +
  latticeExtra::layer(grid.text("Natural wetland %", x=0, y=.35, just='left'))


# plot cropland ================================================================
# wetland mapping theme
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdYlGn")),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))


cutpts <- seq(0, 10^-6, 10^-7) # set symbol cutoffs


cropland_plt<- levelplot(c/a, 
                       margin = F, 
                       at=cutpts,
                       cuts=10, # nb of colors 
                       pretty=TRUE, 
                       #main=paste0("Cropland fraction"),
                       par.strip.text= list(cex=0.5, lines=10, fontface='bold'),
                       axes=FALSE, box=FALSE,
                       scales=list(draw=FALSE),
                       xlab=NULL, ylab=NULL,
                       par.settings = mapTheme,
                       colorkey=list(space="right"),
                       xlim=c(-180, 180),
                       ylim=c(-70, 90))  +
  latticeExtra::layer(grid.text("Cropland %", x=0, y=.35, just='left'))


# plot wetloss  ===============================================================
mapTheme <- rasterTheme(region = brewer.pal(10, "OrRd"),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))

cutpts <- seq(0, 10^-6, 10^-7) # set symbol cutoffs

# plot inundated %
wetloss_plt<- levelplot(wetloss, 
                margin = F, 
                at=cutpts,
                cuts=10, # nb of colors 
                pretty=TRUE, 
                #main=paste0("Wetland loss as % of natural wetland cover"),
                par.strip.text= list(cex=0.5, lines=10, fontface='bold'),
                axes=FALSE, box=FALSE,
                scales=list(draw=FALSE),
                xlab=NULL, ylab=NULL,
                par.settings = mapTheme,
                colorkey=list(space="right"),
                xlim=c(-180, 180),
                ylim=c(-70, 90)) +
  latticeExtra::layer(grid.text("Wetland loss %", x=0, y=.35, just='left'))



# timeline plot ================================================================
library(ggplot2)

timeline <- ggplot() +
  
  geom_point(aes(x=c(hyde_yrs[hyde_yrs<yr])), y=0, shape = 124, size=10, color='grey45') +
  geom_point(aes(x=yr, y=0), shape = 124, size=15, color='red') +
  
  geom_hline(yintercept = 0, color='black', size=1) +
  geom_text(aes(x=yr, y=0.7, label=yr), color='red',size=8) +
  
  scale_y_continuous(limits=c(0,1), expand = c(0, 0)) +
  scale_x_continuous(limits=c(-10000, 2000), expand = c(0.05,0.05), 
                     breaks=seq(-10000, 2000, 1000), labels=seq(-10000, 2000, 1000)) +
  
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x =  element_line(color='black'),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.margin = margin(-60, 10, 2, 10, "pt"))
  

#library(cowplot)
#plot_grid(wet_plt, cropland_plt, wetloss_plt, timeline, ncol=1, rel_heights = c(0.3, 0.3, 0.3, 0.1))

# arrange into a grid ==========================================================
grid.arrange(wet_plt, cropland_plt, wetloss_plt, timeline, ncol=1, heights = c(1,1,1,0.1))

# print plot so it show in GIF
# print(plt)  
# plt


