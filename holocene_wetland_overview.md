Rates and hotspots of global long-term historic wetland loss
================
Etienne Fluet-Chouinard
March 19, 2018

### Background

-   Wetland conversion rates during early historical periods with waterworks and agricultural expansion are not well known and rely on extrapolations from more recent periods (Davidson 2014).
-   The geographic and temporal records of wetland loss are likely unrepresentative of global conversion patterns, possibly distorting the extrapolated baseline wetland cover (Hu et al. 2017).
-   Natural wetland cover and their methane emissions simulated over millennia timescales by earth system models offer a long-term perspective on the natural baseline wetland cover.
-   Land use reconstructions have been used to assess forest cover (Hurtt et al. 2006; Kaplan et al. 2009), carbon fluxes (Kaplan et al. 2011), water consumption (Kummu et al. 2010), novel ecosystems (Ellis et al. 2010; 2013), and could represent reclamation pressure to natural wetlands.
-   In this paper, we combine simulations of wetland cover with land use reconstructions to estimate long-term change in global wetland extent.

### Method/Approach

-   We intersect land use reconstruction and an ensemble of simulated wetland extent to estimate wetland conversion rates and remaining fraction of cover between 6000BC and 2000AD. The start of the period matches with the apparition of levees and breaks in Mesopotamia around 6000BC (Bowden et al. 1981).
-   We produce estimates of wetland extent and loss over the worlds surface outside of sampled regions of the world.
-   These estimates help contextualize the present-day wetland cover within a long-term trend of degradation.

#### Natural wetland cover

We use and ensemble of natural wetland cover simulations time periods over a range of time periods (Late Holocene & 20th century) given the divergence among simulated present-day wetland extents (Melton et al. 2013). We use models based on prognostic approach to wetland cover but calibrated to the global wetland cover from satellite imagery (Prigent et al. 2007; Papa et al. 2010). These models do not include drainage and water abstraction processes from their simulations, effectively representing natural wetland cover in the absence of human action.

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/line_plot_sum_nat_wet_20th.png" alt="Figure 1: Global wetland area over years 1700-2010 from different models and simulations." width="50%" />
<p class="caption">
Figure 1: Global wetland area over years 1700-2010 from different models and simulations.
</p>

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/lpxdytop_global_inund_peatland.png" alt="Figure 2: Global wetland area from LPX-DYTOP over the late holocene. Wetland area is defined as the maximum monthly fraction over the course of that year." width="50%" />
<p class="caption">
Figure 2: Global wetland area from LPX-DYTOP over the late holocene. Wetland area is defined as the maximum monthly fraction over the course of that year.
</p>

#### Land use reconstructions

Two land cover reconstructions are used to quantify reclamation of wetland: the HYDE (Goldewijk et al. 2010) and KK10 (Kaplan et al. 2011). Both reconstructions rely on hindcasted population to estimate land use requirements that are spatially allocated based on land cover suitability and satellite information on current distribution, but are based on different sets of assumptions (Klein Goldewijk and Verburg 2013).

### Estimating wetland loss

**So far, I have been using the overlay of DYTOP & HYDE3.2 as a testbed for the other combinations of the ensemble.**
Wetland conversion in each gridcell is estimated with the overlay of the wetland extent and LUCC from reconstructions. Conversion is defined as wetland area transitioning from a ‘natural’ cover to cropland or urban covers in the reconstructions (Figure 3 - bottom). The wetland cover will be overlaid with each land cover class to quantify the conversion attributed to each land cover class. We evaluate conversion to irrigated rice culture separately from other land cover classes as the flooding of wet rice culture maintains some hydrological functions of natural wetlands.

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/wetloss.gif" alt="Figure 3: (top) Animation of natural wetland cover (LPXDYTOP-Stocker et al. 2017), (middle) cropland cover reconstruction, and (bottom) wetland loss." width="40%" />
<p class="caption">
Figure 3: (top) Animation of natural wetland cover (LPXDYTOP-Stocker et al. 2017), (middle) cropland cover reconstruction, and (bottom) wetland loss.
</p>

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/area_plot_sum_wetloss.png" alt="Figure 4: Timeline of wetland coversion, as represented in the animation of figure 3." width="40%" />
<p class="caption">
Figure 4: Timeline of wetland coversion, as represented in the animation of figure 3.
</p>

### Analysis of wetland loss patterns

**We evaluate the geography of wetland loss with regards to:** \* Remaining percentage of wetland cover against a reference-baseline \* Identify the period of maximum wetland loss

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/map_perc_remwet_from_natwetlarea.png" alt="Figure 5: Global map of the percentage remaining wetland cover relative to the year 2000BC." width="40%" />
<p class="caption">
Figure 5: Global map of the percentage remaining wetland cover relative to the year 2000BC.
</p>

<img src="https://s3.us-east-2.amazonaws.com/holocenewetlandloss/map_period_max_wetloss.png" alt="Figure 6: Global map of time period of maximum wetland conversion rate." width="40%" />
<p class="caption">
Figure 6: Global map of time period of maximum wetland conversion rate.
</p>
