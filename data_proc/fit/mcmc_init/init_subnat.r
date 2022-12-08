# Init subnat:  Prep a df of perc drained in each countries
# wit happropriated infill elswehre

# Function 
conv_subnat_df <- function(subnat_stack) { #subnat_perc_r, subnat_id_r){

  # subnat_stack <- stack(subnat_cropland_perc_r, subnat_cropland_id_r)
  
  # Convert rasters to df
  subnat_stack_df  <- raster2df(subnat_stack)
  names(subnat_stack_df) <- c('nat_id', 'subnat_id', 'subnat_perc', 'x', 'y')
  subnat_stack_df<- subnat_stack_df %>% dplyr::select(-nat_id)
  
  # Round coordinates to 2nd decimal, for joining
  subnat_stack_df$x = round(subnat_stack_df$x, 2)
  subnat_stack_df$y = round(subnat_stack_df$y, 2)
  
  # # Join subnat data to template grid with country codes
  subnat_stack_df <- left_join(ciso_df, subnat_stack_df, by=c('x','y'))  # maxlncr_df_xy
  
  names(subnat_stack_df) <- c('x', 'y', 'nat_id', 'iso_a3', 'country_name', 'subnat_id', 'subnat_perc') # 
  
  # This isnt true...  Fill in 0 over countreis with other subnat data, but 1s everywhere else
  subnat_stack_df <-  subnat_stack_df %>% 
                      group_by(nat_id) %>% #iso_a3 
                      # Fill NA pixels in  countries with subnat data with 0s 
                      mutate(subnat_perc = ifelse((is.na(subnat_perc) & sum(!is.na(subnat_perc))>0), 0, subnat_perc)) %>% 
                      # Fill NA pixels in countries without subnat data with 1s
                      mutate(subnat_perc = ifelse((is.na(subnat_perc) & sum(!is.na(subnat_perc))==0), 1, subnat_perc)) %>% 
                      # NOTE: There are pixels on coastline that are not part of any countries 
                      #       because the landmask is bigger than the GADM polygons
                      #   Set those pixels to NA for peace of mind
                      mutate(subnat_perc = ifelse(is.na(iso_a3), NA, subnat_perc)) %>% 
                      ungroup()

  return(subnat_stack_df)
  }


# /----------------------------------------------------------------------------#
#/  READ SUBNAT RASTER

# read raster of subnat percentage of nat drain
subnat_cropland_perc_r <- stack('../output/results/artif_drainage/subnational/raster/subnat_perc_Cropland.tif')
subnat_forestry_perc_r <- stack('../output/results/artif_drainage/subnational/raster/subnat_perc_Forestry.tif')
subnat_peatextr_perc_r <- stack('../output/results/artif_drainage/subnational/raster/subnat_perc_Peat Extraction.tif')


# /----------------------------------------------------------------------------#
#/ Run function  &  Add LUtype column 
subnat_cropland_df <- conv_subnat_df(subnat_cropland_perc_r)
subnat_cropland_df$type <- 'cropland' 

subnat_forestry_df <- conv_subnat_df(subnat_forestry_perc_r)
subnat_forestry_df$type <- 'forestry' 


subnat_peatextr_df <- conv_subnat_df(subnat_peatextr_perc_r)
subnat_peatextr_df$type <- 'peatextr'


# stack all df outputs 
subnat_df_alltypes <- subnat_cropland_df %>% 
                      bind_rows(., subnat_forestry_df) %>% 
                      bind_rows(., subnat_peatextr_df)


