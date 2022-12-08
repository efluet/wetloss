
# get Davidson histcase polygon data ====================================================

# save the spatial data data
#histcases_poly <- readRDS("./output/results/histcases_poly.rds")
histcases_poly <- readOGR("../data/hist_records/davidson_sites_gis", "davidson_sites")

# project shapefile to Robinson for plotting
histcases_poly_robin <- spTransform(histcases_poly, CRS("+proj=robin"))

# add id column
histcases_poly_robin@data$id <- as.numeric(rownames(histcases_poly_robin@data))

# convert to df
histcases_poly_df <- fortify(histcases_poly_robin, region = 'id')

# merge the attribute table back to spatial df
histcases_poly_df <- merge(histcases_poly_df, histcases_poly_robin@data, by="id")



