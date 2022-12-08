

# read hyde
h <- './data/lucc/hyde32_beta/zip/hyde32.nc'
hyde <- nc_open(h)
hyde_yrs <- sort(hyde$dim$time$vals) # get years
rm(h, hyde)


# read remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_2.5deg.rds')


r_start   <-remwet_Mkm2_stack[[grep(pattern="6000",   names(remwet_Mkm2_stack))]]
r_end   <-remwet_Mkm2_stack[[grep(pattern="1700",   names(remwet_Mkm2_stack))]]


r_start <- mean(r_start)
r_end <- mean(r_end)


perc_remwet_in1980from2000bc <- (remwet_Mkm2_stack$yr1980*10^6) / (remwet_Mkm2_stack$yr.2000*10^6) * 100
perc_remwet_in1980from1700 <- (remwet_Mkm2_stack$yr1980*10^6) / (remwet_Mkm2_stack$yr1700*10^6) * 100


# PLOT the map =================================================================


perc_remwet_in1980from2000bc <- as(perc_remwet_in1980from2000bc, "SpatialPixelsDataFrame")
perc_remwet_in1980from2000bc <- as.data.frame(perc_remwet_in1980from2000bc)

cutpts <- c(0, 25, 50, 75, 100, 500, 1000)
perc_remwet_in1980from2000bc$layercut <- cut(perc_remwet_in1980from2000bc$layer, breaks=cutpts)


# replace the categories stings to make them nicer in the legend
perc_remwet_in1980from2000bc$layercut <- gsub("\\(|\\]", "", perc_remwet_in1980from2000bc$layercut)
perc_remwet_in1980from2000bc$layercut <- gsub("\\,", " to ", perc_remwet_in1980from2000bc$layercut)
perc_remwet_in1980from2000bc <- perc_remwet_in1980from2000bc %>% mutate(layercut=ifelse(layercut=="100 to 500",
                                                                                        ">100",layercut))


# set order (from last to first )
lengend_order <- rev(c(">100",  "75 to 100", "50 to 75", "25 to 50",  "0 to 25"))      

perc_remwet_in1980from2000bc$layercut <- factor(perc_remwet_in1980from2000bc$layercut, levels = lengend_order)
levels(perc_remwet_in1980from2000bc$layercut)


# map wetland  
#wet_plt <- 
  ggplot() +
            #geom_tile(data=glacier, aes(x=x, y=y), fill='grey80') +
            # add background mask
            geom_tile(data=perc_remwet_in1980from2000bc, aes(x=x, y=y, fill=layercut)) +
            
            coord_equal() +
            theme_minimal() +
            scale_fill_brewer(palette='YlOrRd',direction=-1, 
                              name= "Remaining % \nof 2000BC wetland \n cover in 1980") +
            theme(legend.title = element_text(size=6),
                  legend.text = element_text(size=6),
                  legend.position=c(0.1, 0.4),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())


### save plot ------------------------------------------------------------------
ggsave("../../output/figures/map_percremwetland_since_baseline.png",
       dpi=1000, width=87, height=70, units='mm' , type = "cairo-png")

dev.off()
