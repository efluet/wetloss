remwet_Mkm2_stack <- readRDS('./output/results/wetloss/grid/remwet_Mkm2_stack_0.5deg.rds')



## this is probably not the right one to use
remwet_Mkm2_stack <- sel.by.pattern(remwet_Mkm2_stack, "1700")
remwet_Mkm2_stack <- sel.by.pattern(remwet_Mkm2_stack, "rdm")

remwet_Mkm2_stack_rng <- max(remwet_Mkm2_stack) - min(remwet_Mkm2_stack)

