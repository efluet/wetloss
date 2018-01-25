
### Prep for scripts -----

# ~~~ set working directory ----
setwd('C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/scripts/r')

# ~~~ import libraries ----
source('./import_libraries.r')



#
glob_zhang <- read.csv('../../output/global_wetarea_zhang_hyde31.csv')
glob_zhang$source <- 'zhang et al 2016'

glob_dlem <- read.csv('../../output/global_wetchimp_1_DLEM_hyde31.csv')
glob_dlem$source <- 'WETCHIMP-DLEM'

# glob_sdgvm <- read.csv('../../output/global_wetchimp_1_SDGVM_wetarea_hyde31.csv')
# glob_sdgvm$source <- 'SDGVM'

glob <- rbind(glob_zhang, glob_dlem)


# usa_zhang <- read.csv('../../output/usa_wetarea_zhang_hyde31.csv')
# usa_dlem <- read.csv('../../output/usa_wetchimp_1_DLEM_hyde31.csv')
# usa <- rbind(usa_zhang, usa_dlem)


rm(glob_zhang, glob_dlem, usa_zhang, usa_dlem)

library(stringr)

glob <- glob %>%
        mutate(year = as.numeric(str_sub(glob$year, 1, -3))) %>%
        group_by(year, source) %>%
        summarize(a_wet_rem_km2 = sum(a_wet_rem_km2)) %>%
        ungroup() %>%
        mutate(a_wet_rem_Mkm2 = a_wet_rem_km2 / 10^6)

dahl = read.csv('../../data/hist_records/source_specific/dahl_1990/dahl1990_wetland_loss_v3.csv')


# plot ----------------------------------
ggplot(glob)+
  geom_line(data=glob, aes(x=year, y=a_wet_rem_Mkm2, color=source)) 




