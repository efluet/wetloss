
# read in the national drainage statistics database
source('data_proc/artif_drainage/read_nat_artif_drainage_v2.r')

# area of agriculture in wetland (valley bottom, spate irrig)
# append it to drainage table
source('data_proc/artif_drainage/aquastat_wetland_cultiv.r')


# combine databases of drainag and wetland cultivation.
drainstat <- bind_rows(drained, cultwet)


### TO DO:
# - combine the spate irrig in the total drain

# write out the data
write.csv(drainstat, "../output/results/artif_drainage/drained_wetcult_ha_v3.csv")
