# /----------------------------------------------------------------------------#
#/     Read correction layers                                     --------------

# JRC static
JRC <- stack('../data/natwet/wad2m_corr_layers/Global_JRC_025deg_WGS84_fraction.nc')
JRC <- JRC * area(JRC)

# Mirca Rice 12 months
MIRCA <- stack('../data/natwet/wad2m_corr_layers/MIRCA_monthly_irrigated_rice_area_025deg_frac.nc')
MIRCA <- calc(MIRCA, fun=max, na.rm=T)
MIRCA <- MIRCA * area(MIRCA)

# Coastal mask (Static)
COAST <- stack('../data/natwet/wad2m_corr_layers/MODIS_coastal_mask_025deg.nc')
COAST <- COAST * area(COAST)

# CIFOR wetland map (Static)
CIFOR <- stack('../data/natwet/wad2m_corr_layers/cifor_wetlands_area_025deg_frac.nc')
CIFOR <- CIFOR * area(CIFOR)

# NCSCD peatland map (static)
NCSCD <- stack('../data/natwet/wad2m_corr_layers/NCSCD_fraction_025deg.nc')
NCSCD <- NCSCD * area(NCSCD)

# NCSCD peatland map (static)
GLWD <- stack('../data/natwet/wad2m_corr_layers/GLWD_wetlands_025deg_frac.nc')
GLWD <- GLWD * area(GLWD)

