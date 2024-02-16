# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Matthias Siewert (Umea University)
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-05-13, 2023-03-10
  # Dataset Notes: 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
require(sf)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_06_siewert_russia_kytalyk/Siewert_Arctic biomass data intake form v2_Kytalyk.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 2))

# STANDARDIZE DATASET ======================================================================================

# add dataset ID
harv.dt <- harv.dt[, dataset_id := NULL] 
harv.dt[, dataset_id := 'ds06']

# add contributors
harv.dt[, contributor := 'Siewert M']

# set locale
harv.dt[, locale := 'Kytalyk']

# standardize site id and plot id
str(harv.dt$site_id)
harv.dt[, site_id := substr(site_code, 4,5)] # grab transect id and use for plot
harv.dt[, plot_id := substr(plot_id, 7, nchar(plot_id))]

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# specify coordinate type (site or plot) 
harv.dt[, coord_type := as.character(coord_type)]
harv.dt[, coord_type := 'plot']

# set vegetation and site descriptions to none
harv.dt <- harv.dt[, site_description := as.character(site_description)]
harv.dt <- harv.dt[, ':='(site_description = 'none', vegetation_description = 'none')]
harv.dt <- harv.dt[, ...24 := NULL] 

#specify harvest type
harv.dt[, method := 'harvest + survey'] 
harv.dt[, notes := 'Harvested 10x10 cm patch then scaled using PFT cover at 1x1m']

#check plant functional types 
unique(harv.dt$pft)
harv.dt[pft == 'Mosses (from Taimyr (Ramage 2012))', pft := 'Mosses'] # removing note from "mosses" pft

harv.dt[, pft := tolower(pft)]
harv.dt[pft == 'mosses', pft := 'bryophyte']
harv.dt[pft == 'graminoids', pft := 'herb']
harv.dt[pft == 'shrubs', pft := 'shrub']
harv.dt[pft == 'forbs', pft := 'herb']
harv.dt[pft == 'lichens', pft := 'lichen']

harv.dt <- harv.dt[pft != 'litter'] #removing litter pft from dataset

unique(harv.dt$pft)

check_pfts(harv.dt)

# remove biomass density column because it messes up calculation below
harv.dt <- harv.dt[, biomass_density_gm2 := NULL]

# compute total AGB for each PFT by combining multiple pfts at a given plot 
value.cols <- paste(c('vegetation_description','biomass_dry_weight_g'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(as.numeric(biomass_dry_weight_g)),
                      vegetation_description = first(vegetation_description)), 
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# compute biomass density 
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]
pft.dt <- fill_missing_metadata(pft.dt, 'vegetation_description', 'plot_code')
pft.dt <- fill_missing_metadata(pft.dt, 'notes', 'plot_code')

pft.dt[pft == 'tree', ':='(method = 'survey', notes = 'none')]

# add note that moss biomass is from 
pft.dt[pft == 'bryophyte', notes := paste0(notes, '. Bryophyte biomass from Taimyr (Ramage 2012)')]

# add citation
pft.dt[, citation := as.character(citation)]
pft.dt[, citation := 'Siewert et al. 2015. Comparing carbon storage of Siberian tundra and taiga permafrost ecosystems at very high spatial resolution. Journal of Geophysical Research: Biogeosciences 120:1973-1994.']

pft.dt[, citation_short := 'Siewert et al. 2015']


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

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# View(pft.dt)

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_06_siewert_russia_kytalyk_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================