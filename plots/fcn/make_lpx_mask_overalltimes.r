
# read remaining wetland
remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_2.5deg.rds')



# make a mask of the full extent in LPX =========================================
r_start <-max(remwet_Mkm2_stack[[grep(pattern="6000", names(remwet_Mkm2_stack))]])
r_end <- max(remwet_Mkm2_stack[[grep(pattern="1980", names(remwet_Mkm2_stack))]])

# make function
# introduce na in rst1 for all locations that are non-na in rst2
r_mask <- overlay(r_start, r_end, fun = function(x, y) {
  x[!is.na(y[])] <- 99
  x[!is.na(x[])] <- 1
  return(x) })

r_mask_robin <- prep_raster_into_robin_map(r_mask)
