# read file on cultivated wetlands
f <- '../data/cultivated_wetland/aquastat_cultivated_wetland.csv'

# read the AQUASTAT wetland-ag national drainage database
cultwet <-  read.csv(f, stringsAsFactors = F)

var_name_sel <- c("Area equipped for irrigation: spate irrigation",
                  "Cultivated wetlands and inland valley bottoms non-equipped",
                  "Flood recession cropping area non-equipped",
                  "Area equipped for irrigation: equipped lowland areas")

# keep only a few types 
cultwet <- cultwet[cultwet$Variable.Name %in% var_name_sel, c("Area", "Variable.Name", "Year", "Value")]
#
cultwet <- cultwet %>%
           mutate(Variable.Name=ifelse(Variable.Name=="Area equipped for irrigation: spate irrigation", "Spate irrig.", Variable.Name),
                  Variable.Name=ifelse(Variable.Name=="Cultivated wetlands and inland valley bottoms non-equipped", "Cultiv.not-equip.", Variable.Name),
                  Variable.Name=ifelse(Variable.Name=="Flood recession cropping area non-equipped", "Flood recession", Variable.Name),
                  Variable.Name=ifelse(Variable.Name=="Area equipped for irrigation: equipped lowland areas", "Cultiv.equip.", Variable.Name))

# rename columns
names(cultwet) <- c("country_name", "type", "year", "drained_area_irrig")


write.csv(cultwet, "../output/results/artif_drainage/cultwet_v3.csv")

# plot the area per country
#source('./scripts/r/plots/artif_drainage/cultiv_wetland_only_percountryarea.r')






################################################################################

### Area equipped for irrigation: Equipped lowland areas (ha)

# The land equipped for irrigation in lowland areas includes: 
# (i) Cultivated wetland and inland valley bottoms (IVB), 
# which have been equipped with water control structures for irrigation and drainage (intake, canals, etc.); 
# (ii) Areas along rivers, where cultivation occurs making use of water from
# receding floods and where structures have been built to retain the receding water; 
# (iii) Developed mangroves and equipped delta areas.


### Area equipped for irrigation: Spate irrigation (ha)

# Spate irrigation can also be referred to as floodwater harvesting. It is a method of random irrigation
# using the floodwaters of a normally dry watercourse or riverbed (wadi). 
# These systems are in general characterized by a very large catchment upstream (200 ha - 50 km2) with a “catchment area: cultivated area” ratio of 100:1 to 10 000:1. 
# There are two types of floodwater harvesting or spate irrigation: 
#   1)  floodwater harvesting within streambeds, where turbulent channel flow is collected and spread
# through the wadi in which the crops are planted; cross-wadi dams are constructed with stones, earth, or
# both, often reinforced with gabions; 
#   2) floodwater diversion, where the floods - or spates - from the seasonal rivers 
# are diverted into adjacent embanked fields for direct application. A stone or concrete
# structure raises the water level within the wadi to be diverted to the nearby cropping areas. 


### Flood recession cropping area non-equipped

# Areas along rivers where cultivation occurs in the areas exposed as floods recedes 
# and where nothing is undertaken to retain the receding water. The special case of 
# floating rice is included in this category.


### Cultivated wetlands and inland valley bottoms non-equipped
# Wetland and inland valley bottoms (IVB) that have not been equipped with 
# water control structures but are used for cropping. They are often found in Africa. 
# They will have limited (mostly traditional) arrangements to regulate water and control drainage.