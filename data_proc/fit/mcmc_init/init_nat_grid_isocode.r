# /----------------------------------------------------------------------------#
#/   MAKE GRID OF COUNTRY ISO CODES   FROM GADM0 - SO ITS HARMONIZED WITH SUBNAT


gadm0_st <- st_read("../data/gadm36", "gadm36_0")  %>%
  filter(!NAME_0 %in% c('Antarctica', 'Kiribati', 'Micronesia', 'Samoa', 'Fiji')) %>% 
  mutate(nat_id = as.numeric(as.factor(GID_0)))

# make df of gadm0 data
gadm0_st_df <- gadm0_st %>%  st_drop_geometry()  # data.frame(gadm0_st[,c('GID_0','NAME_0','nat_id')])




# /----------------------------------------------------------------------------#
#/  Convert GADM0 to raster 0.25deg raster then aggregate to 0.5deg, to prevent cutting-off coastlines
gadm0_st_nat_id_r <- fasterize(gadm0_st, template_025deg, field = "nat_id", fun="first")
gadm0_st_nat_id_r <- raster::aggregate(gadm0_st_nat_id_r, fact=2, fun=modal, ties='first', na.rm=T)

# then to df
gadm0_st_nat_id_df <- raster2df(gadm0_st_nat_id_r)

names(gadm0_st_nat_id_df) <- c('nat_id', 'x', 'y')

# Join data to df pixels
gadm0_st_nat_id_df <- left_join(gadm0_st_nat_id_df, gadm0_st_df , by='nat_id')

# join 
# use old name for df
ciso_df <- left_join(maxlncr_df_xy, gadm0_st_nat_id_df, by=c('x','y'))

names(ciso_df) <- c('x','y','nat_id','iso_a3','country_name')

