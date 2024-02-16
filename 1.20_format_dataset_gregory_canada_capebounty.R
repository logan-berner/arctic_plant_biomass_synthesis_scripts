# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Fiona Gregory
# Script Author: Logan Berner
# Script Date: 2023-03-30
# Dataset Notes:
# - Biomass was not partitioned by PFT, so only have total live AGB

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(tidyr)
require(dplyr)
require(sf); sf_use_s2(FALSE)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
colnames(database.tmplt.dt)
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_20_gregory_canada_capebounty/Fiona-biomass.xls', sheet = 3))
meta.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_20_gregory_canada_capebounty/Fiona-biomass.xls', sheet = 8))


# STANDARDIZE SITE DATA ======================================================================================

# adjust metadata column names
names(meta.dt) <- tolower(names(meta.dt))
names(meta.dt)
setnames(meta.dt, 
         c('site','lat','long','type','vegetation'), 
         c('site_id','latitude','longitude','site_description','vegetation_description'))

# convert coordinates from wgs72 to wgs84
pts.wgs72.sf <- st_as_sf(meta.dt, coords = c("longitude", "latitude"), crs = 4322, agr = "constant") %>% st_cast('POINT')
pft.wgs84.sf <- st_transform(pts.wgs72.sf, crs = 4326)

meta.dt[, ':='(latitude = st_coordinates(pft.wgs84.sf)[,2],
               longitude = st_coordinates(pft.wgs84.sf)[,1])]

# adjust site description
meta.dt[site_description == 'PD', site_description := 'Polar desert']
meta.dt[site_description == 'WS', site_description := 'Wet sedge meadow']
meta.dt[site_description == 'MH', site_description := 'Mesic heath']

# subset and adjust harvest data column names
names(harv.dt) <- tolower(names(harv.dt))
col.names <- names(harv.dt); col.names
keep.cols <- col.names[c(1,3,13)]; keep.cols
harv.dt <- harv.dt[, keep.cols, with = F]

setnames(harv.dt, c('site','quadrant','weight (g) /m2'), c('site_id','plot_id','biomass_density_gm2'))

# get the plot-level biomass density (quadrats can have multiple sample bags [rows], but this column is the sum across bags)
harv.dt <- harv.dt[is.na(biomass_density_gm2) == F]

# add coordinates, harvest date, and descriptions
harv.dt <- meta.dt[harv.dt, on = 'site_id']

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds20',
               contributor = 'Gregory F, Treitz P, Scott N',
               country = 'Canada',
               locale = 'Cape Bounty',
               plot_area_m2 = 0.25,
               coord_type = 'site',
               year = 2008,
               month = 7,
               day = 27, # harvests occurred 26 - 28th but lacking data on when each plot was specifically harvested
               method = 'harvest',
               pft = 'total',
               citation = 'Gregory F. 2012. Biophysical remote sensing and terrestrial CO2 exchange at Cape Bounty, Melville Island, Masters Thesis, Queen’s University, Kingston, Canada.',
               citation_short = 'Gregory 2012',
               notes = 'none')]

# create site and plot codes
harv.dt[, site_code := paste(country, 'MelvilleIsland', site_id, sep='.')]
harv.dt[, plot_code := paste(country, 'MelvilleIsland', site_id, plot_id, sep='.')]

# calculate biomass dry weight
harv.dt[, biomass_dry_weight_g := biomass_density_gm2 / 4]

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(harv.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(harv.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- harv.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'site_id']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(site_id)) %>% 
  addLabelOnlyMarkers(label = ~as.character(site_id),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_20_gregory_canada_capebounty_standardized.csv'
fwrite(harv.dt, out.file)

# END SCRIPT =================================================================================================