# /----------------------------------------------------------------------------#
#/    Title: Historical wetland loss analysis
#     Author: Etienne Fluet-Chouinard
#     Affiliation: Stanford University, USA; University of Wisconsin-Madison, USA
#     Last update: 8 December 2022

# /----------------------------------------------------------------------------#
#/        SET WD                                                         -------
# Set working directory to the location of the project
library(here); setwd(here())
# Prep functions, themes, etc.
source('./data_proc/fcn/prep_init.r') 

# /----------------------------------------------------------------------------#
#/          PREPARE GRIDDED DATA                                            -------

# Resample HYDE 3.2 to 0.5deg grid; Only needed once
source('./data_proc/prep_data/hyde/resample_hyde.r')

# Prep peatmap
source('./data_proc/wettype/grid_PEATMAP.r')

# Prepare simulated wetland covers (ORCHIDEE, SDGVM, DLEM, LPJ-wsl)
source('./data_proc/natwet/simwet/prep_simwet.r')

# Prepare present wetland covers (WAD2M, GLWDM, GIEMS2)
source('./data_proc/natwet/preswet/prep_preswet.r')

# Calculate POTWET as difference between simwet and pres.wet (only positive values)
source('./data_proc/natwet/potwet/prep_potwet_v2.r')


# /----------------------------------------------------------------------------#
#/       PREPARE DRAINAGE STATISTICS                                     -------

# Convert peat extraction time series to area;  Split it off from other data, then rebind rows
source('./data_proc/artif_drainage/process_peat_extr_weight.r')

# Fit sigmoid to data rich  - FOR CROPLAND, FORESTRY, PEAT EXTRACTION
#  NOV2020 - applies to nat & subnat (if >=4 pts);  subnational fits not used any more
source('./data_proc/artif_drainage/sigmoid_fit_ts_drained_nat.r')

# Apply sigmoid interpolation to national data
source('./data_proc/artif_drainage/apply_sigmoid_drained_nat.r')

# Prep wetland cultivation statistics separately
source('./data_proc/artif_drainage/wetcult_stat_process_v2.r')

# Combine the interpolated drainage and wetland cultivation
source('./data_proc/artif_drainage/comb_interp_drainage_wetcult.r')


#### DISAGGREGATE DRAINAGE TIEMSERIES TO SUBNATIONAL UNITS
# Prepares subnat unit processing before the init_preprocess script
# Calculate % of drainage per subnational unit
source('./data_proc/artif_drainage/subnat_drainage_v3.r')


# /----------------------------------------------------------------------------#
#/    PREP DAVIDSON 2014 DATA                                         ----------

# This is the starting point file: wetland_loss_cases_combined_v2.csv
# This one is manually curated: wetland_loss_cases_combined_v2_manmod

### not needed after manual modification of Hist Cases
# #.. DATA PROC - Point coordinate hist cases
# # plots the cases with lat/long coordinates as points
source('./data_proc/hist_cases/format_historical_cases.r')

# #.. DATA PROC - POLY hist cases (still needs some manual attention)
#  This is replaced by manual polygon drawing
source('./data_proc/hist_cases/make_polygon_histcases_nat_subnat.r')


# Process WET index into % loss per record
source('./data_proc/hist_cases/wetindex_calc_loss.r')

# Make poly/points from WET index
# source('./data_proc/hist_cases/prep_wet_index.r')

# This subset the polygons from Davidson2014
source('./data_proc/hist_cases/prep_hist_cases_2021.r')


# /----------------------------------------------------------------------------#
#/   RUN RECONSTRCTION MODEL & FIT THETA PARAMETERS                     --------

# Initialize inputs for theta fitting
source('./data_proc/overlay/initialize_prefitting.r')

#  Run Latin Hypercube of Theta parameters
source('./data_proc/fit/run_lh_fit_2021.r')

# Combine parameters (pars) from each of 12 runs
source('data_proc/fit/compile_lh_pars.r')


# /----------------------------------------------------------------------------#
#/   RUN FINAL FINAL MODELS                                             --------

# Run model for 12; generate outputs
source('data_proc/mcmc_fit/run_finalmap_lh_pars_range.r')

#/    Run XVAL fit (i.e., Cross validation)
source('./data_proc/fit/run_lh_fit_xval.r')

# /----------------------------------------------------------------------------#
#/  SAVE OUTPUTS                                                        --------

# Save NetCDF of grids for 12 reconstructions
source('data_proc/save_grid_ncdf_2021.r')

# Save drainage stats database (only ones actually used)
source('data_proc/save_final_drain_db.r')

# Save regional drainage data
source('data_proc/save_final_regional_data.r')


# /----------------------------------------------------------------------------#
#/   SUMMARY STATS                                                      --------
# get summary stats of the results
source('./data_proc/postprocess/stats_for_paper.r') 
# source('./data_proc/summary_stats_wetloss.r')

# % peat losses
source('./data_proc/wettype/perc_peat_losses.r') 

# /----------------------------------------------------------------------------#
#/    FIGURES                                                       --------

# FIG 1  -  lineplot, maps, etc
source('./plots/fig2abcd/fig1_2022_revround2.r')

#  FIG 2 AB -  Map of regional data, and scatterplot
source('./plots/fig3abc/fig3_2021_v2.r')

# FIG 3 # PLOT:  line plot: wetland type timeline per region & wettype
source('./plots/per_basin/lineplot_lu_loss_perbasin_v6.r') 

# Fig 1 Wetloss lit bargraph
source('./plots/wetloss_lit_bargraph_v2.r')


# /----------------------------------------------------------------------------#
#/    SI PLOTS & TABLES                                                   ------

source('./plots/si_figures/si_figures.r')

# /----------------------------------------------------------------------------#
#/     GIF Animation maps                                                -------

# GIF animation of wetland loss
source('./plots/gif/gif_wetloss_since1700.r')

# GIF animation of remaining wetland area
source('./plots/gif/gif_remwet_map.r')
