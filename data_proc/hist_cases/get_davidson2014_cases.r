#/ GET DAVIDSON2014 CASES     -------------------------------------------------


# read data histcases Shapefile
# hist_poly <- readOGR("../output/results/histcases/davidson_sites_manmod.shp")
hist_poly <- readOGR("../data/hist_records/davidson_sites_gis/davidson_sites_wdata.shp")

hist_poly <- subset(hist_poly, src_id != 0)  # remove records without a paired source_id

# read the manually curated datatable
# "./data/hist_records/wetland_loss_cases_combined_v2_manmod.csv", 
hist_data <- 
  read.csv('../output/results/histcases/histcases_loss_v2_manmod_p.csv') %>%
  select(-c(full.citation, Comment, ef_comment))

# join poly and data on id
histcasesdata <- merge(hist_poly, hist_data, by=c("src_id","rec_id"))

# Get data table of joined df (why?)
h = histcasesdata@data  

# save as RDS again
saveRDS(histcasesdata, "../data/hist_records/davidson_sites_gis/davidson_sites_wdata_manmod.rds")





#### 
# FIX FUCK UP 

hist_poly <- readOGR("../data/hist_records/davidson_sites_gis/davidson_sites_wdata.shp")
# hist_polymod <- readOGR("../output/results/histcases/davidson_sites_manmod.shp")@data


hist_poly <- merge(hist_poly, hist_polymod, by='name')
