

# set mapping theme
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))


cutpts <- seq(0, 1, 0.05) # set symbol cutoffs


if(yrs[t]<0){
  yr_label <- paste0(yrs[t]*-1, 'BC')
} else {
  yr_label <- paste0(yrs[t], 'AD')
}




# plot glacier/submerged land
plt<- levelplot(glacier,
                margin = F, 
                col.regions='grey90',
                pretty=TRUE, 
                par.settings = mapTheme,
                main=paste0("Inundated percentage \n ", month.abb[m], "  ", yr_label),
                par.strip.text=list(cex=0.5, lines=10, fontface='bold'),
                axes=FALSE, box=FALSE,
                scales=list(draw=FALSE),
                xlab=NULL, ylab=NULL,
                colorkey=F)#list(space="bottom", draw=FALSE))


# plot inundated %
plt<- plt + levelplot(lu, 
                      #margin = F, 
                      at=cutpts,
                      cuts=10, # nb of colors 
                      pretty=TRUE, 
                      par.settings = mapTheme,
                      colorkey=list(space="bottom"))


print(plt)




