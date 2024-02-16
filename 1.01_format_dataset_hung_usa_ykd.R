# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Jacqueline Hung (WCRC)
# Script Author: Logan Berner
# Script Date: 2022-04-27
# Dataset Notes: 
# - Each site has multiple quadrats
# - Coordinates available for each quadrat 
# - To get AGB for each PFT, need to sum over the Vegetation_Descirption column 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
require(leaflet)
require(sf)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_01_hung_usa_ykdelta/Hung_WCRC_USA_YKDelta_biomass_data_v2.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 2))

# STANDARDIZE DATASET ======================================================================================

# add dataset ID
harv.dt[, dataset_id := 'ds01']

# add contributors
harv.dt[, contributor := 'Hung J, Ludwid S, Natali S']

# adjust citation
first(harv.dt$citation)
harv.dt[year == 2018, citation := 'Hung et al. 2022. Polaris Project 2018: Vegetation biomass, plot characterization, point intercept, and thaw depth, Yukon-Kuskokwim Delta. Alaska, Arctic Data Center, doi:10.18739/A2F18SG68.']
harv.dt[year == 2018, citation_short := 'Hung et al. 2022']

harv.dt[year == 2019, citation := 'Hung el al. 2022. Polaris Project 2019: Vegetation biomass, point intercept, and thaw depth, Yukon-Kuskokwim Delta, Alaska. Arctic Data Center, doi:10.18739/A2JS9H89M.']
harv.dt[year == 2019, citation_short := 'Hung et al. 2022']

# set notes to none
harv.dt[, notes := 'none']

# adjust country and locale
harv.dt[, country := 'USA']
harv.dt[, locale := 'Yukon Kuskokwim Delta']

# create site and plot codes
harv.dt[, site_code := paste(country, 'YukonKuskokwimDelta', site_id, sep='.')]
harv.dt[, plot_code := paste(country, 'YukonKuskokwimDelta', site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# specifcy coornidate type (site or plot) 
harv.dt[, coord_type := as.character(coord_type)]
harv.dt[year == 2018, coord_type := 'site']
harv.dt[year != 2018, coord_type := 'plot']

# set method to harvest
harv.dt[, method := 'harvest']

# set notes to none
harv.dt[, notes := as.character(notes)]
harv.dt[, notes := 'none']

# check plant functional types 
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'moss', pft := 'bryophyte']
harv.dt <- harv.dt[pft != 'unknown']
check_pfts(harv.dt)

# remove biomass density column beacuse it messes up calculation below
harv.dt <- harv.dt[, biomass_density_gm2 := NULL]

# compute total AGB for each PFT by summing over 'Vegetation_Description' column 
value.cols <- paste(c('vegetation_description','biomass_dry_weight_g'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      vegetation_description = concat_rows(vegetation_description)), 
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# compute biomass density 
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# expand data table so every pft occurs in every plot and set missing rows to zero
pft.dt <- expand_missing_pfts(pft.dt)

# set tree biomass to zero on all plots (treeless tundra)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey')]

# set any missing data to zero 
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]

pft.dt[is.na(vegetation_description), vegetation_description := 'none']
pft.dt[is.na(notes), notes := 'none']
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

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_01_hung_usa_ykd_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================