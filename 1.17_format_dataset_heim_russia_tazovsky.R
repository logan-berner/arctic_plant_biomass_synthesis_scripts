# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Logan Berner (Northern Arizona University)
# Script Author: Logan Berner
# Script Date: 2023-02-03
# Dataset Notes: 
# 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(sf)
require(tidyr)
require(dplyr)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_17_heim_russia_tazovsky/Arctic biomass data intake form v2 Ramona Heim.xlsx', sheet = 2))
tree.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_17_heim_russia_tazovsky/Arctic biomass data intake form v2 Ramona Heim.xlsx', sheet = 4))

# ADD METEDATA TO HARVEST DATA ===================================================================================
harv.dt <- harv.dt[, c('dataset_id','method') := NULL]
names(harv.dt)

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, method, site_description, notes, citation, and citation_short
harv.dt[, ':='(dataset_id = 'ds17',
               contributor = 'Heim R, Holzel N',
               locale = 'Tazovsky',
               coord_type = 'plot',
               vegetation_description = 'Extensive Cladonia reindeer lichens, abundant Betula nana and other shrubs, occasional graminoids, bryophytes, and Larix sibirica trees',
               site_description = 'Relatively dry and well-drained upland area in transition zone between the forest tundra to the south and the shrub tundra to the north',
               citation = 'Heim et al. 2022. Fire in lichen-rich subarctic tundra changes carbon and nitrogen cycling between ecosystem compartments but has minor effects on stocks. Biogeosciences 19:2729-2740.', 
               citation_short = 'Heim et al. 2022',
               method = 'harvest',
               notes = paste0('Sample pooled from 5 subplots that were each ', subplot_area_m2, ' m2.'))]


# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# partition burned and unburned plots into separate "sites" 
harv.dt[, site_id := as.character(site_id)]
harv.dt[fire == 1, site_id := paste0('Burned',site_id)]
harv.dt[fire == 0, site_id := paste0('Unburned',site_id)]

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

dim(harv.dt)


# STANDARDIZE PFT DATA =======================================================================================
unique(harv.dt$pft)

# standardizing pft names
harv.dt[pft == 'bryophytes', pft := 'bryophyte']
harv.dt[pft == 'lichens', pft := 'lichen']
harv.dt[pft == 'grasses', pft := 'herb']
harv.dt[pft == 'herbs', pft := 'herb']
harv.dt[pft == 'shrub leaves' | pft == 'shrub stems', pft := 'shrub']

unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each pft  
harv.dt[, biomass_dry_weight_g := NULL]

harv.dt[biomass_density_gm2 == 'NA', biomass_density_gm2 := 0] # two shrub leaves are missing but stems are present. assume not partitioned
harv.dt[, biomass_density_gm2 := as.numeric(biomass_density_gm2)]

value.cols <- 'biomass_density_gm2'
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)), 
                        by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]
dim(pft.dt)


# deal with trees (not measured, but percent cover recorded, so flag as zero or NA accordingly)
pft.dt <- expand_missing_pfts(pft.dt)

setnames(tree.dt, 'plot','plot_id')
pft.dt <- pft.dt[tree.dt, on = 'plot_id']

pft.dt[pft == 'tree' & treecov_percent == 0, ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]
pft.dt[pft == 'tree' & treecov_percent != 0, ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA)]

pft.dt[pft == 'tree' & biomass_density_gm2 == 0, method := 'survey']
pft.dt[pft == 'tree' & is.na(biomass_density_gm2), method := 'unmeasured']

pft.dt[pft == 'tree', notes := paste0('Tree cover ', treecov_percent, '%.')]
pft.dt[pft == 'tree', plot_area_m2 := 100] # surveys conducted on 10 m x 10 m plots

pft.dt <- pft.dt[is.na(pft) == F]

pft.dt[method == 'unmeasured', plot_area_m2 := NA]

pft.dt <- fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')

# add notes about time since fire and remove those columns
pft.dt[fire == 1, notes := paste(notes, 'Burned', years_since_fire, 'years before sampling.', sep = ' ')]

# drop unused columns
pft.dt <- pft.dt[, c('fire','years_since_fire','no_subplots','subplot_area_m2','pairs', 'treecov_percent','treeheight_cm') := NULL]

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]
dim(pft.dt)

# check average biomass density for each pft across all sites
pft.dt[,.(sum = sum(biomass_dry_weight_g, na.rm = T), 
          mean = mean(biomass_density_gm2, na.rm = T)), by = 'pft']


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


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


# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_17_heim_russia_tazovsky_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================