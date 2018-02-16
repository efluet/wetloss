# import libraries
library(RColorBrewer)
library(cowplot)
library(stringi)
library(stringr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)
library(ggplot2)
#library(mc2d) 
library(countrycode)
library(ggrepel)
library(grid)
library(ncdf4)
library(dplyr)
library(animation)
library(raster)
library(rasterVis)
library(maptools)
library(maps)

rm(list=ls()) # clear memory
library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)

# monte carlo
# fitdistrplus package is convenient for assessing a parametric distribution of data
options("scipen"=100, "digits"=6)