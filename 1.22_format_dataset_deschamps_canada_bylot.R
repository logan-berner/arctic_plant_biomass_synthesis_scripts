# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Laurent J. Lamarque (University of Quebec at Three Rivers)
# Script Author: Logan Berner
# Script Date: 2023-05-30
# Dataset Notes: 
# - need to updated citation once paper/dataset is available

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
require(utils)
require(dplyr)
require(sf); sf_use_s2(FALSE)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_22_deschamps_canada_bylot/Arctic biomass data intake form v2_Deschamps_Maire_Levesque_Mesic.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 2))

# STANDARDIZE DATASET ======================================================================================

# add metadata
harv.dt[, ':='(dataset_id = as.character(dataset_id), 
               contributor = as.character(contributor),
               citation = as.character(citation),
               citation_short = as.character(contributor))]

harv.dt[, ":="(dataset_id = 'ds22',
               contributor = 'Deschamps L, Levesque E, Maire V, Morneault A',
               citation = 'Deschamps et al. 2023. Forthcoming.',
               citation_short = 'Deschamps et al. 2023')]
  
# create site and plot codes
harv.dt[, site_code := paste(country, 'BylotIsland', capitalize(toCamelCase(harv.dt$site_id)), sep='.')]
harv.dt[, plot_code := paste(country, 'BylotIsland', capitalize(toCamelCase(harv.dt$site_id)), plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)

# set notes to none
harv.dt[, notes := as.character(notes)]
harv.dt[, notes := 'none']


# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# check plant functional types 
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)
harv.dt[pft == 'deciduous shrub', pft := 'shrub']
harv.dt[pft == 'evergreen shrub', pft := 'shrub']
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'moss', pft := 'bryophyte']
unique(harv.dt$pft)
check_pfts(harv.dt)

# compute total AGB for each PFT by summing over 'Vegetation_Description' column 
harv.dt <- harv.dt[, biomass_density_gm2 := NULL] # # remove biomass density column beacuse it messes up calculation below

value.cols <- paste(c('vegetation_description','biomass_dry_weight_g'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      vegetation_description = first(vegetation_description)), 
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# compute biomass density 
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]
dim(pft.dt)

pft.dt[plot_code == 'Canada.BylotIsland.MesicPlateau.D1']

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set tree biomass to zero on all plots (treeless tundra)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey')]

# lichen biomass measured on all plots, but absent on atleast one plot so set to zero instead of NA 
pft.dt[pft == 'lichen' & is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'harvest')]

fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')
fill_missing_metadata(pft.dt, fill.cols = 'site_description', by = 'plot_code')
pft.dt[is.na(notes), notes := 'none']

pft.dt
dim(pft.dt)


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_code']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_code)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_code),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_22_deschamps_canada_bylot_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================