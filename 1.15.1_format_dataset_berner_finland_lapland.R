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
harv.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_15_berner_finland_lapland/2_plot_biomass_summaries.csv')
plot.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_15_berner_finland_lapland/1_plot_metadata.csv')

# ADD METEDATA TO HARVEST DATA ===================================================================================
names(harv.dt)

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, method, site_description, notes, citation, and citation_short
harv.dt[, ':='(dataset_id = 'ds15',
               contributor = 'Berner L, Orndahl K, Burns P',
               country = 'Finland',
               coord_type = 'plot',
               citation = 'Berner et al. 2023. Plant aboveground biomass by functional group for alpine tundra and mountain birch woodlands in northern Finland, 2022. Arctic Data Center. doi:10.18739/A2QV3C526', 
               citation_short = 'Berner et al. 2023', 
               notes = 'none')]


# STANDARDIZE PLOT DATA =======================================================================================
names(plot.dt)

# round coordinates to 6 decimals
plot.dt[, latitude := round(latitude, 6)]
plot.dt[, longitude := round(longitude, 6)]

# change column names
names(plot.dt)[names(plot.dt) == 'site_vegetation']  <- 'vegetation_description'

# combine harv.dt and plot.dt
harv.dt <- harv.dt[plot.dt, on = c('site_id','plot_id')]

dim(harv.dt)


# STANDARDIZE PFT DATA =======================================================================================
unique(harv.dt$pft)

# standardizing pft names
harv.dt[pft == 'moss', pft := 'bryophyte']
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'graminoid', pft := 'herb']

unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each pft  
value.cols <- 'biomass_density_gm2'

grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)), 
                        by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# reformat plot area metadata, remove excess columns, make not about shrub plot areas
names(pft.dt)
pft.dt[, plot_area_m2 := numeric()]
pft.dt[pft == 'bryophyte' | pft == 'lichen', plot_area_m2 := nonvascular_plot_area_m2]
pft.dt[pft == 'herb', plot_area_m2 := vascular_plot_area_m2]
pft.dt[pft == 'shrub', plot_area_m2 := shrub_plot_area_m2]
pft.dt[pft == 'tree', plot_area_m2 := tree_plot_area_m2]

pft.dt[pft == 'shrub', notes := 'Tall shrub survey on 2 x 2 m plot and low shrub harvest on 0.5 x 0.5 m plot.']
pft.dt <- pft.dt[, c('nonvascular_plot_area_m2', 'vascular_plot_area_m2', 'shrub_plot_area_m2', 'tree_plot_area_m2') := NULL]

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]
dim(pft.dt)

# check average biomass density for each pft across all sites
pft.dt[,.(sum = sum(biomass_dry_weight_g), 
          mean = mean(biomass_density_gm2)), by = 'pft']

# specify method
pft.dt[pft == 'bryophyte' | pft == 'lichen' | pft == 'herb', method := 'harvest']
pft.dt[pft == 'shrub', method := 'harvest + survey']
pft.dt[pft == 'tree', method := 'survey']

# create site and plot codes
pft.dt[, site_code := paste(country, locale, site_id, sep='.')]
pft.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]


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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_15_berner_finland_lapland_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================