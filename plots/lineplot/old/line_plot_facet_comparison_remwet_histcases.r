# compare histcases to wetloss % from gis data




remwet <- read.csv("../../output/results/histcase_remwet_extracted.csv") %>%
          # calc % loss
          mutate(perc_change_numeric = (remwet_end-remwet_start)/remwet_start*100) %>%
          mutate(yr_start = hyde_yr_start, yr_end   = hyde_yr_end) %>%
          select(rec_id, yr_start, yr_end, perc_change_numeric) %>%
          mutate(name = "remwet")


histcases <-  read.csv('../../output/results/historic_cases_wetland_loss_mod.csv', stringsAsFactors=F) %>%
              select(rec_id, country, yr_start, yr_end, perc_change_numeric) %>%
              mutate(name = "histcase")




comb <- inner_join(remwet, histcases, by="rec_id") %>%
        mutate(remwet_perc_loss = -(remwet_start-remwet_end)/remwet_start*100)


ggplot(comb) +
  geom_point(data=comb, aes(x=remwet_perc_loss, y=perc_change_numeric))




#comb <- bind_rows(remwet, histcases) # %>% gather(a, b, yr_start, yr_end)


# comb <- inner_join(remwet, histcases, by="rec_id") %>%
#         # select some columns
#         select(rec_id, year_start, year_end, perc_change_numeric,
#                hyde_year_start, hyde_year_end, remwet_perc_loss)
#   gather(a, b,
#          year_start, year_end, hyde_year_start, hyde_year_end) %>%
#   mutate(c = ifelse(grepl("hyde",comb$a), remwet_perc_loss, perc_change_numeric))



# # should the polygond be rasterized?
# 
# map <- ggplot() +    # bbox_robin_df, aes(long, lat)
#   
#   # add countries with data
#   geom_polygon(data=histcases_poly_sub, 
#                aes(long, lat, group=group, 
#                    fill= perc_change_numeric), alpha=1) +
#   
#   coord_equal() +  #theme_fig() +
#   #scale_fill_distiller(palette = 3) +
#   theme(legend.position="top") +
#   theme(plot.margin = unit(c(-2,-3,-2,-10), "mm"))
# 
# map
