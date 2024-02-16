# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: H.E. Greaves
# Script Author: Melissa Rose
# Script Date: 2022-06-17
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(tidyr)
require(dplyr)
require(leaflet)
require(sf)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_13_greaves_usa_toolik/Toolik_Lake_Shrub_Biomass.csv'))

# STANDARDIZE BIOMASS DATA ===================================================================================

# remove easting and northing 
harv.dt <- harv.dt[, c('easting', 'northing') := NULL]

# add year, month, and day
harv.dt <- separate(harv.dt, 'date', c('year', 'month', 'day'), sep = '-') %>% data.table()

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# change column names 
setnames(harv.dt, 'shrub_dry_mass','biomass_density_gm2')

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, method, citation, citation_short, and notes
harv.dt[, ':='(dataset_id = 'ds13',
               contributor = 'Greaves H',
               country = 'USA',
               locale = 'Toolik',
               plot_area_m2 = 0.64,
               coord_type = 'plot',
               pft = 'shrub',
               method = 'harvest',
               citation = 'Greaves et al. 2018. High-Resolution Shrub Biomass and Uncertainty Maps, Toolik Lake Area, Alaska, 2013. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1573.',
               citation_short = 'Greaves et al. 2018')]

# assign each plot a unique plot_id
harv.dt <- harv.dt[, plot_id := 1:nrow(harv.dt)]
harv.dt[, plot_id := as.character(plot_id)]
harv.dt[nchar(plot_id) == 1, plot_id := gsub(" ", "", paste('0',plot_id))]

# assign site_id according to plot_id 
site1 <- c('01', '02', '03', '04', '05', '06', '07', '08', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48')
site2 <- c('09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '49', '50', '51', '52', '53', '54', '55', '56', '57', '58')
site3 <- c('19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38')

setkey(harv.dt, plot_id)
harv.dt[site1, site_id := 'S1']
harv.dt[site2, site_id := 'S2']
harv.dt[site3, site_id := 'S3']

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# STANDARDIZE PFT DATA =======================================================================================

# standardize pfts
unique(harv.dt$pft)

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(harv.dt)
dim(harv.dt)
dim(pft.dt)

# specify methods
pft.dt[pft == 'tree', method := 'survey']
pft.dt[pft != 'tree' & pft != 'shrub', method := 'unmeasured']

pft.dt[method == 'unmeasured', plot_area_m2 := NA]

# no trees present, so set chance biomass density from NA to zero
pft.dt[pft == 'tree', biomass_density_gm2 := 0]

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# set vegetation_description, site_description, and notes values to 'none'
pft.dt[, site_description := 'Tundra landscape underlain by continuous permafrost with expansive rolling hills of tussock tundra']
pft.dt[, vegetation_description := 'Tussock cottongrass and Carex sedges with Sphagnum mosses and low deciduous shrubs mainly Betula nana and Salix spp']

pft.dt[, notes := 'none']
pft.dt[pft == 'shrub', notes := 'Only harvested shrubs taller than 5 cm.']


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_id']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_id)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_id),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_13_greaves_usa_toolik_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================