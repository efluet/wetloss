# /-----------------------------------------------------------------------------
#/  MAKE GRID OF COUNTRY ISO CODES   FROM GADM0 - SO ITS HARMONIZED WITH SUBNAT
#   Make df grids of geographic units with drainage stats 


# /-----------------------------------------------------------------------------
#/  Read polygons 

# Read nat polygons
gadm0_st <- st_read("../data/gadm36", "gadm36_0")

# Read subnat polygons
gadm1_st <- st_read("../data/gadm36", "gadm36_1") 

# Get HASC_1 fix for Poland 
source('./data_proc/mcmc_init/fix_poland_subnat.r', local=TRUE) 
gadm1_st <- gadm1_st %>% 
            left_join(., poland_subnat_fix, by=c('GID_1'='GID_1')) %>% 
            mutate(HASC_1 = ifelse(NAME_0=='Poland', HASC_1fix, HASC_1)) %>% 
            dplyr::select(-HASC_1fix)


# /----------------------------------------------------------------------------#
#/  LOOP THROUGH LU TYPES 

# Create empty dataframe
geo_units_alltypes <- data.frame()

# List of lu types with subnat data
types= c('cropland', 'forestry', 'peatextr', 'wetcultiv')

# Loop through LUtypes
for (t in types){
  
  print(t)
  
  # filter to national units, i.e.have pred_drained_subnat == NA
  drainage_nat <- drainage %>% 
                  filter(type==t, is.na(HASC_1)) %>%  #pred_drained_subnat)) %>% 
                  dplyr::select(type, iso_a3, pred_drained) %>% 
                  distinct()
  
  # Filter to subnat units 
  drainage_subnat <- drainage %>% 
                    filter(type==t, !is.na(HASC_1)) %>%  # pred_drained_subnat 
                    dplyr::select(type, iso_a3, HASC_1, pred_drained) %>%  # pred_drained_subnat
                    distinct() 
  
  
  # /--------------------------------------------------------------------------#
  #/ Filter national polygons
  gadm0_st_f <- gadm0_st %>% filter(GID_0 %in% drainage_nat$iso_a3)
  # Filter SUBnational polygons
  gadm1_st_f <- gadm1_st %>% filter(GID_0 %in% drainage_subnat$iso_a3 & HASC_1 %in% drainage_subnat$HASC_1)
  
  
  # If there are SUBNAT 
  if (nrow(gadm1_st_f) > 0){
    # Combine nat & subnat polygons
    gadm_comb <- data.table::rbindlist(list(gadm0_st_f, gadm1_st_f), use.names = TRUE, fill = TRUE, idcol = NULL)
    gadm_comb <- st_as_sf(gadm_comb)
  } else {
    gadm_comb <- gadm0_st_f
  }
  
  # Create a new numeric id
  gadm_comb <- gadm_comb %>% mutate(unit_id = c(1:nrow(gadm_comb)))
  
  # If there are SUBNAT
  if (nrow(gadm1_st_f) > 0){
    #  Make df (no geo) of columns to keep
    gadm_comb_df <- gadm_comb %>% st_drop_geometry() %>% dplyr::select(GID_0, NAME_0, GID_1, NAME_1, HASC_1, unit_id)
  } else {
    gadm_comb_df <- gadm_comb %>% st_drop_geometry() %>% dplyr::select(GID_0, NAME_0, unit_id)
  }
  
  
  
  # /---------------------------------------------------------------------------
  #/  FASTERIZE PERCENTAGE DRAINAGE
  #   Convert to raster 0.25deg raster then aggregate to 0.5deg, to prevent cutting-off coastlines
  # gadm_comb_id <- fasterize(gadm_comb, template, field = "unit_id", fun="first")
  gadm_comb_id <- fasterize(gadm_comb, template_025deg, field = "unit_id", fun="first")
  gadm_comb_id <- raster::aggregate(gadm_comb_id, fact=2, fun=modal, ties='first', na.rm=T)
  
  
  # /---------------------------------------------------------------------------
  #/  
  
  # Convert to dataframe
  gadm_comb_id_df <- raster2df(gadm_comb_id)
  # Round coordinates to 2nd decimal, for joining
  gadm_comb_id_df$x = round(gadm_comb_id_df$x, 2)
  gadm_comb_id_df$y = round(gadm_comb_id_df$y, 2)
  
  # # Join subnat data to template grid with country codes
  gadm_comb_id_df <- left_join(maxlncr_df_xy, gadm_comb_id_df, by=c('x','y'))  %>% 
                     rename(unit_id = layer) %>% 
                     # Join the drainage data to the df-grid
                     left_join(., gadm_comb_df, by='unit_id') %>% 
                     # Make column for LUtype
                     mutate(type = t)
  
  # Append to output df combining all types
  geo_units_alltypes <- bind_rows(geo_units_alltypes, gadm_comb_id_df)
  
  }


# Save as CSV (30 Nov )
f <- paste0('../output/results/geo_units/alltypes_df.csv', row.names=F)
write.csv(geo_units_alltypes, f)


# /---------------------------------------------------------------------------#
#/ USE THIS TO ENTER GID_1 CODES IN DATABASE  - not needed for Dec 2020 runs
# make df of gadm0 data; lookup table, not grid.
gadm1_st_nogeo <- gadm1_st %>%  st_drop_geometry() %>% dplyr::select(c('GID_0', 'NAME_0', 'GID_1', 'NAME_1', 'HASC_1')) #, 'HASC_1fix'))
# gadm_comb_nogeo <- gadm_comb %>%  st_drop_geometry() %>% dplyr::select(c('GID_0', 'NAME_0', 'GID_0'))


