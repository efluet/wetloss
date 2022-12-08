
# /----------------------------------------------------------------------------#
#/    SI PLOTS & TABLES                                                   ------

# FIG 1.5  - sigmoid data points
source('./plots/artif_drainage/fig1p5_drainstats.r')
source('./plots/artif_drainage/sigmoid_template_facet.r')


# table; Combine run r^2 and drained area  for SI  table
source('data_proc/fit/finalize_run_df_forSI.r')

#/  Fig 3 C - cumul lineplot of regional& WETindex data & KS test
source('plots/fig3abc/fig3c_2021_forSI.r')


# SI plot - maps of drainage database country & subnat  units - 2021
source('./plots/artif_drainage/map_drain_data_nat.r')
source('./plots/artif_drainage/subnat/map_subnat_units.r')

# SI plot - lineplot interpolated drainage data country - 2021
source('./plots/artif_drainage/si_lineplot_sigmoid_reconstruct.r')

#/ Figure with time distribution histogram of drainage stats
source('plots/artif_drainage/histogram_timedistribution_drainstats.r')

# SI plot - map of generalized regions for sigmoid parameters
source('./plots/artif_drainage/map_sigmoid_regions.r')

# Map of preswet, potwet grid
# source('./plots/figSI_map_matrix.r')
source('./plots/nat_wet/map_potwet_grid.r')

# Maps of present-day wetlands (one map per input data)
source('./plots/nat_wet/si_preswet_stack_map.r')
source('./plots/nat_wet/si_simwet_stack_map.r')

# SI plot - lineplots of mapped wetloss per lu (extracted from grid_drain_perlu)
source('./plots/lineplot/facet_country_grid_drain_perlu.r')

# Map of max drainage rate time period 
source('./plots/map/max_loss_rate_map_forSI.r')

# Map LU drivers in separate maps
source('./plots/map/map_ludrivers_facet.r')

# Map peatland regions
source('./plots/per_basin/map_peat_regions.r')

#/ Fig.2A lineplot but with area - with all reconstructions
source('plots/fig2abcd/fig2a_wetarea_lineplot.r')

#/ Panel of wetloss map figure 2B
source('plots/fig2abcd/panel_fig2b_wetloss_forsi.r')

#/ SI - Boxplot of theta parameters 
source('plots/fit/theta_boxplot.r')

# SI - Peat extraction rates and cumul
source('plots/artif_drainage/peat_extr_cumul_plot_si.r')

