
# poland 
# 
# gadm1_st <- st_read("../data/gadm36", "gadm36_1")
# 
# poland1 <-  gadm1_st %>% 
#             filter(GID_0=='POL') %>% 
#             mutate()
# 


GID_1 <- c('POL.1_1', 'POL.2_1', 'POL.3_1', 'POL.4_1', 'POL.5_1', 'POL.6_1',
           'POL.7_1', 'POL.8_1', 'POL.9_1', 'POL.10_1','POL.11_1','POL.12_1',
           'POL.13_1', 'POL.14_1', 'POL.15_1', 'POL.16_1')

HASC_1fix <- c('PL.DS', 'PL.KP', 'PL.LD', 'PL.LU', 'PL.LB', 'PL.MA', 
            'PL.MZ', 'PL.OP', 'PL.PK', 'PL.PD', 'PL.PM', 'PL.SL',
            'PL.SK', 'PL.WM', 'PL.WP', 'PL.ZP')


poland_subnat_fix <- data.frame(GID_1, HASC_1fix)
# names(poland_subnat_fix) <- c('GID_1', 'HASC_1fix')