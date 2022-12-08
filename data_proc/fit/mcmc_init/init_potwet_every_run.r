# Initialize the preswet and potwet dfgrids

# the indices for simwet and preswet
if (test_potwet) { s_i = 4 ; p_i = 1 }


# /------------------------------------------------------------------
#/  Get simwet
simwet  <- simwet_stack[[s_i]]   # 1 - 4 
simwet_df <- left_join(maxlncr_df_xy, raster2df(simwet), by=c('x','y'))

# /------------------------------------------------------------------
#/  Get preswet - mamax

preswet <- preswet_stack[[p_i]]  # 1-3
# Convert preswet to grid df
preswet_df <- left_join(maxlncr_df_xy, raster2df(preswet), by=c('x','y'))
# Select 3rd column; the one containing data
preswet_df <- preswet_df[[3]]


# /------------------------------------------------------------------
#/  Get preswet - mamax
  
if(preswet_max == 1){
  
  preswet <- preswet_max_stack[[p_i]]  # 1-3
  # Convert preswet to grid df
  preswet_df <- left_join(maxlncr_df_xy, raster2df(preswet), by=c('x','y'))
  # Select 3rd column; the one containing data
  preswet_df <- preswet_df[[3]]
  }


# /------------------------------------------------------------------
#/  Calculate potwet:  Used to distribute drainage
potwet <- simwet - preswet
# 0 negative values
potwet[potwet<0] <- 0.0001    # 0
# ensure potwet cannot be greater than pixarea
potwet <- min(potwet, maxlncr) 
# Convert to standard df grid
potwet_df <- left_join(maxlncr_df_xy, raster2df(potwet), by=c('x','y'))
# Select column
potwet_df <- potwet_df$layer


#####
# Diagnosis maps
if(0){
ggplot() + geom_tile(data=preswet_df, aes(x,y,fill=layer)) 

ggplot() + geom_tile(data=simwet_df, aes(x,y,fill=SDGVM2_km2)) 

ggplot() +
  geom_tile(data=potwet_df, aes(x,y,fill=layer)) +
  geom_tile(data=subset(potwet_df, layer==0), aes(x,y), fill='red')

}

# /----------------------------------------------------------------------------#
#/ Make empty output df                                                   ------
total_drain_perlu_peryear <- 
  data.frame(lu_type     = character(),
             year             = numeric(),
             new_drain_km2    = numeric(),
             cumul_drain_km2  = numeric(),
             rem_potwet_km2   = numeric())


total_drain_perlu_peryear <- data.frame()
