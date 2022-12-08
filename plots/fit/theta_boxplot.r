# /----------------------------------------------------------------------------#
#/ Plot theta values histogram per member                               --------

# get parameters
best_pars_df <- read.csv('../output/results/fit/lh/parameters/best_pars_lh_2021.csv')


p_names=c('GIEMSv2','GLWD3','WAD2M')
s_names=c('DLEM','ORCHIDEE','SDGVM','LPJ-wsl')

# convert to long format
pars_df_long <- best_pars_df %>% 
  pivot_longer(theta_rice:theta_urban, names_to='theta_name', values_to='theta_val') %>% 
  mutate(s_name=s_names[s_i], p_name=p_names[p_i])




#TODO: Make violin plot
ggplot(pars_df_long) +
  # geom_histogram(aes(x=theta_val, fill=theta_name), bins=20) +
  # geom_violin(aes(x=theta_name, y=theta_val, fill=theta_name), bins=20) +
  geom_boxplot(aes(x=theta_name, y=theta_val, fill=theta_name, color=theta_name), alpha=0.6, width=0.2) + #, bins=20) +
  # facet_grid(s_name~p_name, scales='free') +
  facet_rep_grid(s_name~p_name) +
  line_plot_theme +
  xlab('') + ylab('Theta parameter value') +
  theme(legend.position = 'none')


ggsave('../output/figures/theta_values_facet_v2.png',
       width=190, height=220, dpi=600, units='mm' )
