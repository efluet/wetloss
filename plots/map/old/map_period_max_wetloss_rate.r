


hyde_yrs<- readRDS('./output/results/hyde_yrs_all.rds')


wetloss_Mk2_stack



names(wetloss_Mk2_stack)




# get the raster number of max wetloss 
which.max.na <- function(x, ...) ifelse(length(x) == sum(is.na(x)), 0, which.max(x))

max_wetloss_r <- calc(wetloss_Mk2_stack, which.max.na)
values(max_wetloss_r)[values(max_wetloss_r)== 0] = NA


# 
for (i in seq(1, nlayers(wetloss_Mk2_stack), 1)){
  
  yr <- hyde_yrs[i] 
  
  values(max_wetloss_r)[values(max_wetloss_r)== i] = yr
  
  }
plot(max_wetloss_r)


# PLOT the map =================================================================

# set mapping theme
mapTheme <- rasterTheme(region = rev(brewer.pal(5, "RdYlGn")),
                        axis.line = list(col = "transparent"),
                        scales = list(x = list(draw = FALSE)))


cutpts <- c(-10000, -4000, -2000, 0, 1700, 1850, 2000)



png("../../output/figures/map_period_max_wetloss.png", width=1800, height=1000, res=300)

# plot glacier/submerged land
plt<- levelplot(max_wetloss_r,
                margin = F, 
                #col.regions='grey90',
                pretty=TRUE, 
                at=cutpts,
                par.settings = mapTheme,
                #main=paste0("Inundated percentage \n ", month.abb[m], "  ", yr_label),
                par.strip.text=list(cex=0.5, lines=10, fontface='bold'),
                axes=FALSE, box=FALSE,
                scales=list(draw=FALSE),
                xlab=NULL, ylab=NULL) #list(space="bottom", draw=FALSE))

print(plt)
dev.off()
