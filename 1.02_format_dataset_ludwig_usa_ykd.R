# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Sarah Ludwig (Columbia Univ)
# Script Author: Logan Berner
# Script Date: 2022-04-27, 2023-03-24
# Dataset Notes: 
# - Each site has multiple quadrats
# - Coordinates available for each quadrat 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_02_ludwig_usa_ykdelta/Ludwig_WCRC_USA_YKDelta_biomass_data_v2.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 2))

# STANDARDIZE DATASET ======================================================================================

# add dataset ID
harv.dt[, dataset_id := 'ds02']

# add contributors
harv.dt[, contributor := 'Ludwig S, Hung J, Natali S']

# updated citation 
first(harv.dt$citation)
harv.dt[year == 2016, citation := 'Ludwig et al. 2018. Yukon-Kuskokwim Delta fire: vegetation biomass, Yukon-Kuskokwim Delta Alaska, 2016. Arctic Data Center, doi:10.18739/A29S1KK6T.']
harv.dt[year == 2016, citation_short := 'Ludwig et al. 2018']

harv.dt[year == 2017, citation := 'Ludwig et al. 2018. Polaris Project 2017: Vegetation biomass, carbon, and nitrogen, Yukon-Kuskokwim Delta, Alaska. Arctic Data Center, doi:10.18739/A2FJ29D12.']
harv.dt[year == 2017, citation_short := 'Ludwig et al. 2018']

# group the three burned and three unburned plots into two sites 
unique(harv.dt$site_id)
harv.dt[site_id == 'B1', ':='(site_id = 'B', plot_id = 1)]
harv.dt[site_id == 'B2', ':='(site_id = 'B', plot_id = 2)]
harv.dt[site_id == 'B3', ':='(site_id = 'B', plot_id = 3)]
harv.dt[site_id == 'U1', ':='(site_id = 'U', plot_id = 1)]
harv.dt[site_id == 'U2', ':='(site_id = 'U', plot_id = 2)]
harv.dt[site_id == 'U3', ':='(site_id = 'U', plot_id = 3)]
unique(harv.dt$site_id)

# drop measurement from a secondary (duplicated) plot that was partially harvested on the transect
harv.dt <- harv.dt[longitude != -163.229449]

# adjust plot code (remove trailing "_0" from each)
harv.dt[, plot_id := substr(plot_id, 1,1)]

# change site/plot 72B2.2 to 72B2.1 because the Hung et al. data set already has a 72B2.2
harv.dt[site_id == '72B2', plot_id := 1]

# 


# update locale
harv.dt[, locale := 'Yukon Kuskokwim Delta']

# create site and plot codes
harv.dt[, site_code := paste(country, 'YukonKuskokwimDelta', site_id, sep='.')]
harv.dt[, plot_code := paste(country, 'YukonKuskokwimDelta', site_id, plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# set method to harvest
harv.dt[, method := 'harvest']

# set notes to none
harv.dt[, notes := as.character(notes)]
harv.dt[, notes := 'none']

# specify coordinate type (site or plot) 
harv.dt[, coord_type := as.character(coord_type)]
harv.dt[, coord_type := 'plot']
        
# check plant functional types 
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'moss', pft := 'bryophyte']
check_pfts(harv.dt)

# remove biomass density column because it messes up calculation below
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

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)

# set tree biomass to zero on all plots (treeless tundra)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey')]

# set any missing data to zero 
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]

pft.dt[is.na(vegetation_description), vegetation_description := 'none']

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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_02_ludwig_usa_ykd_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================