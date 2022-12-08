
# get function that gets polygons
source("./scripts/data_proc/fcn/fcn_get_polygons_for_histcases.r")


# Read formatted histcases from data file
histcases <- read.csv(#'./output/results/histcases_mod/historic_cases_wetland_loss_mod.csv', 
                      './output/results/histcases_loss_v2_manmod_p.csv',
                      stringsAsFactors = F) %>%
  # add field of adminitrative code
  mutate(adm0_a3 = country_code)


# split histcases into national / subnational
histcases_nat <- histcases %>% filter(is.na(region)) 
histcases_subnat <- histcases %>% filter(!is.na(region)) 


# get country shpfiles
countries_shp <- readOGR("./data/nat_earth", "ne_110m_admin_0_countries")
# get subnational unit shpfiles 
subnat_shp <- readOGR("./data/nat_earth", "ne_10m_admin_1_states_provinces") 
# remove region column to avoid conflict with histcase's region column 
subnat_shp <- subset(subnat_shp, select = -c(region) )


# select the polygons for each
h_c_shp <- get_histcase_shapefile(histcases_nat, countries_shp, "adm0_a3", "adm0_a3", "rec_id")
h_s_shp <- get_histcase_shapefile(histcases_subnat, subnat_shp, "name", "region", "rec_id")

# rename some columns, so the colnames are same in country and subnat
names(h_s_shp)[85] <- 'adm0_a3'
names(h_s_shp)[1] <- 'region'




# set list of columns to keep
cols <- c('rec_id','adm0_a3','country','region','yr_start',
          'yr_end',"rate_loss_km2_yr","perc_change_numeric", "perc_change_per_yr")

# select columns to keep from the polygons
h_c_shp <- h_c_shp[c(cols)]
h_s_shp <- h_s_shp[c(cols)]

# combine country and sub_nat objects
histcases_shp <- rbind(h_c_shp, h_s_shp, makeUniqueIDs = TRUE)



# SAVE OUTPUT POLYGONS =========================================================

# save as shapefile
writeOGR(histcases_shp,  dsn="./output/results/histcases/histcases_poly_mod_sep2019.shp",
         layer = 'historical_cases_poly', driver="ESRI Shapefile")

# save as rds
saveRDS(histcases_shp, "./output/results/histcases/histcases_poly_mod_sep2019.rds")


# delete objects
rm(histcases, histcases_nat, histcases_subnat, countries_shp, 
   subnat_shp, h_c_shp, h_s_shp, histcases_shp)


