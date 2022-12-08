

# wetland mapping theme
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))



wet_plt<- levelplot(wet, 
                    margin = list(draw=F), 
                    #at=cutpts,
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
  latticeExtra::layer(grid.text("Natural wetland %", 
                                x=0, y=.35, just='left'))

print(wet_plt)
