

# read drainage data

# ICID national data
icid <- read.csv('./data/artif_drained/nat/icid_2018_drainage.csv',stringsAsFactors = F) %>%
        gather(Variable.Name, Value, Arable.land.and.permanent.crops.Mha:percent_drained_area) %>%
        mutate(Value = as.numeric(Value))

# Siebert national & subnational data
siebert2005 <- read.csv('./data/artif_drained/nat/siebert2005_sources.csv',stringsAsFactors = F) %>%
               gather(Variable.Name, Value, drainage_area_ha:drainage_in_rainfed.area_ha) %>% 
               dplyr::select(-one_of("ID")) %>%
               mutate(Value = Value / 10^6)

### PREP AQUASTAT
# aquastat
aquastat <- read.csv('./data/artif_drained/nat/aquastat/aquastat_drainage.csv',stringsAsFactors = F) %>%
            filter(Variable.Id %in% c(4300, 4303, 4304)) %>%
            dplyr::select(Country, Variable.Name, Year, Value) %>%
            group_by(Country, Year) %>%
            spread(Variable.Name, Value)

names(aquastat) <- c("Country","Year","drained_area_irrig", "drained_area_rainfed", "drained_area_tot")
# aquastat <- aquastat %>%
#             mutate_all(funs(replace(., is.na(.), 0))) %>%
#             mutate(drained_area_tot = ifelse(drained_area_tot==0, drained_area_irrig+drained_area_rainfed,drained_area_tot))

write.csv(aquastat, './data/artif_drained/nat/aquastat/aquastat_drainage_spreaded.csv')




# combine all three datasources
nat_drainage <- bind_rows(icid, siebert2005, aquastat) %>%
                filter(!Variable.Name %in% c("% of area equipped for irrigation drained", 
                                             "% of total cultivated area drained",
                                             "percent_drained_area")) %>%
                filter(Country != "" & !is.na(Value)) %>%
                # add country code column
                mutate(country_code = countrycode(Country,'country.name','iso3c',warn=F)) %>%
                # combine variables
                mutate(Variable.Name.Comb = ifelse(Variable.Name %in% 
                                                     c("Total.drained.area.Mha",
                                                       "drainage_area_ha",
                                                       "Total cultivated area drained"), 
                                                   "tot.drained.area.Mha", Variable.Name)) %>%
  
                mutate(Variable.Name.Comb = ifelse(Variable.Name %in% 
                                                     c("drainage_in_rainfed.area_ha",
                                                       "Non-irrigated cultivated area drained"), 
                                                   "rainfed.drained.area.Mha", Variable.Name.Comb)) %>%
  
                mutate(Variable.Name.Comb = ifelse(Variable.Name %in% 
                                                     c("drainage_in_irrigated_ha",
                                                       "Area equipped for irrigation drained"), 
                                                   "irrigated.drained.area.Mha", Variable.Name.Comb))
 

s#rm(icid, siebert2005, aquastat) 





ggplot(nat_drainage) +
  geom_line(aes(x=Year, y=Value, color=country_code)) +
  geom_point(aes(x=Year, y=Value, color=country_code)) +
  facet_wrap(~Variable.Name.Comb, scales = "free_y") +
  theme(legend.position = "none")







### US CENSUS - IMPROVED FARMLAND 

test <- readGDAL('./data/artif_drained/grids/cty2mc/cty2mc.e00')

acr_file = "./data/artif_drained/grids/cty2mc/cty2mc.e00"
dr = ogr.GetDriverByName("AVCE00")
f = dr.Open(arc_file)

library(RArcInfo)

#Number of polygons
nmuni<-length(palmuni[[1]][[1]])
e00toavc('./data/artif_drained/grids/cty2mc/cty2mc.e00', "valencia")




# this gets tehe data
palmuni<-get.paldata(".", "valencia")



patmuni<-get.tabledata("./info", "VALENCIA.PAT")

patmuni <- as.data.frame(patmuni)


plotpal(arc=arcsmuni, palmuni)



arcsmuni<-get.arcdata(".", "valencia")
bnd.muni<-get.bnddata("info/", "VALENCIA.BND")
l <- get.labdata(datadir, coverage, filename="lab.adf")


#Number of polygons
nmuni<-length(patmuni[[1]][[1]])
municipios<-data.frame(1:nmuni, patmuni$"VALENCIA-ID")
names(municipios)<-c("INDEX", "CODMUNICI")
#Datafiles to be used
unemp<-read.table(file="data_valencia.csv", sep=";",
                  dec = ",",skip=1)


