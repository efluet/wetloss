globsums <- read.csv("./output/results/wetloss/sum/wetloss_ensemble_prc.csv") 

library(riverplot)



globsums <- globsums %>%
            filter(year %in% c(-6000, 1700, 1980)) %>%
            dplyr::select("year","tot_wet_Mkm2_mean","tot_wetloss_Mkm2_mean",
                          "tot_convtorice_Mkm2_mean","tot_remwet_Mkm2_mean",
                          "tot_cropland_Mkm2_mean","tot_irrice_Mkm2_mean") %>%
            gather(val, type, tot_wet_Mkm2_mean:tot_irrice_Mkm2_mean) 
  
  
            # mutate(N1=ifelse(type==tot_convtorice_Mkm2_mean,"rice",NA),
            #        N2=ifelse(type==tot_convtorice_Mkm2_mean,"rice",NA))
            # mutate(N1=ifelse())
  


head(globsums)



library(data.table)

nodes <- transpose(data.frame(c("wetland.1",1,0,"purple","wetland"),
                              c("wetland.2",2,0,"purple","wetland"),
                              c("wetland.3",3,0,"purple","wetland"),
                              c("upland.1", 1,1,"green","upland"),
                              c("upland.2", 2,1,"green","upland"),
                              c("upland.3", 3,1,"green",""),
                              c("cropland.1", 1,2,"green",""),
                              c("cropland.2", 2,2,"green","cropland"),
                              c("cropland.3", 3,2,"green","cropland"),
                              c("rice.1", 1,3,"green",""),
                              c("rice.2", 2,3,"green","rice"),
                              c("rice.3", 3,3,"green","rice")))

names(nodes) <- c("ID","x","y","col","labels")
nodes$x <- as.numeric(nodes$x)
nodes$y <- as.numeric(nodes$y)

rownames(nodes) = nodes$ID


edges <- transpose(data.frame(c("wetland.1","wetland.2",8),
                              c("upland.1","wetland.2",3.8),
                              
                              c("wetland.1","cropland.2",0.6),
                              c("upland.1","cropland.2",2.5),
                              c("cropland.1","cropland.2",0.01),
                              
                              c("wetland.1","rice.2",0.001),
                              
                                                            
                              c("wetland.2","wetland.3",9.2),
                              c("wetland.2","cropland.3",2.25),
                              c("wetland.2","rice.3",0.25),
                              c("wetland.2","upland.3",0.06),

                              c("upland.2","cropland.3",11),
                              c("cropland.2","cropland.3",4),                              
                              
                              
                              c("rice.2","rice.3",0.03),
                              c("upland.2","rice.3",0.3)
                              
                              ))

names(edges) <- c("N1","N2","Value")
edges$Value <- as.numeric(edges$Value)


# prep the style: colors, widths 
palette = paste0(brewer.pal(4, "Set2"), "20")  # "60"
palette = brewer.pal(4, "Set2")  # "60"

styles = lapply(nodes$y, function(n) {
  list(col = palette[n+1], textcol = "black",  lty = 0)})
names(styles) = nodes$ID



# make plot --------------------------------------------------------------------

plot.new()
#pdf("./output/figures/riverplot_wetland_conv2.pdf", width=7, height=3)

# make riverplot objest
x <- makeRiver(nodes, edges, node_styles = styles)   # node_labels = nodes$labels, 


par(lty=0)
par(mar=c(0,0,2,0)+0.1)

# plot the object
riv_plot <- riverplot(x, plot_area = 0.95, yscale=0.08, fix.pdf=TRUE)  #  node_ypos=1:2)


dev.copy2pdf(file="./output/figures/riverplot_wetland_conv2.pdf", width=6, height=3)

dev.off()




# fix for riverplot?

# curveseg.new = function (x0, x1, y0, y1, width = 1, nsteps = 50, col = "#ffcc0066", 
#                          grad = NULL, lty = 1, form = c("sin", "line")) 
# {
#   w <- width
#   if (!is.null(grad)) {
#     grad <- colorRampPaletteAlpha(grad)(nsteps)
#   }
#   else {
#     grad <- rep(col, nsteps)
#   }
#   form <- match.arg(form, c("sin", "line"))
#   if (form == "sin") {
#     xx <- seq(-pi/2, pi/2, length.out = nsteps)
#     yy <- y0 + (y1 - y0) * (sin(xx) + 1)/2
#     xx <- seq(x0, x1, length.out = nsteps)
#   }
#   if (form == "line") {
#     xx <- seq(x0, x1, length.out = nsteps)
#     yy <- seq(y0, y1, length.out = nsteps)
#   }
#   for (i in 1:(nsteps - 1)) {
#     polygon(c(xx[i], xx[i + 1], xx[i + 1], xx[i]), 
#             c(yy[i], yy[i + 1], yy[i + 1] + w, yy[i] + w), 
#             col = grad[i], border = grad[i], lty=0)
#     lines(c(xx[i], xx[i + 1]), c(yy[i], yy[i + 1]), lty = lty)
#     lines(c(xx[i], xx[i + 1]), c(yy[i] + w, yy[i + 1] + w), lty = lty)
#   }
# }
# 
# assignInNamespace('curveseg', curveseg.new, 'riverplot', pos = -1, envir = as.environment(pos))


