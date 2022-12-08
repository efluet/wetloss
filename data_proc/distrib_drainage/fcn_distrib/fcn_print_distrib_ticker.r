
# PRINT AREA TICKER

sum_drain_distrib <- round(sum(df$drain_distrib, na.rm=T), 1)  # Area to distributed
sum_excess_drain  <- round(sum(df$excess, na.rm=T), 1)         # Area exceeding pixel-wise limit
pred_remain_drain  <- round(sum_drain_stat - sum_drain_distrib, 1)  # Area remaining to distrib

print(paste0('  n:', nb_redist,'   stat:', sum_drain_stat, ',  distrib: ', 
             sum_drain_distrib, ',  excess:', sum_excess_drain, ',  remain:', pred_remain_drain))
