# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Matthias Siewert (Umea University)
# Script Author: Logan Berner
# Script Date: 2022-05-10, updated 2023-03-10
# Dataset Notes: 
# - Subdivided each site using information on vegetation type 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(tidyr)
require(dplyr)
require(stringr)
require(sf)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_05_siewert_sweden_absiko/Siewert_Arctic biomass data intake form v2_Abisko.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 2))

# STANDARDIZE DATASET ======================================================================================

# add meta data 
harv.dt[, dataset_id := 'ds05']
harv.dt[, contributor := 'Siewert M, Olofsson J']
harv.dt[, citation := 'Siewert M and Olofsson J. 2020. Scale-dependency of Arctic ecosystem properties revealed by UAV. Environmental Research Letters 15:094030.']
harv.dt[, citation_short := 'Siewert and Olofsson 2020']

# parse site_id and plot_id
str(harv.dt$site_id)
harv.dt[, site_id := substr(site_id, 1,2)]
harv.dt[, plot_id := substr(plot_id, 4, nchar(plot_id))]

# subdivide each site by vegetation type, but first adjust vegetation type names
unique(harv.dt$vegetation_description)
harv.dt[vegetation_description == 'Betula nana dom.', vegetation_description := 'Betula nana']
harv.dt[, plot_veg := vegetation_description]
harv.dt[plot_veg == 'Salix shrub (mesic)', plot_veg := 'Salix shrub']
harv.dt[plot_veg == 'Seasonally flooded areas', plot_veg := 'Seasonally flooded']
harv.dt[plot_veg == 'Betula nana', plot_veg := 'Birch shrub']
harv.dt[plot_veg == 'Sparsely vegetated', plot_veg := 'Sparse veg']
harv.dt[, plot_veg := str_to_title(plot_veg)]
harv.dt[, plot_veg := gsub(' ', '', plot_veg)]

harv.dt[, site_id := paste(site_id, plot_veg, sep='-')]
harv.dt <- harv.dt[, plot_veg := NULL]

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# specify coordinate type (site or plot) 
harv.dt[, coord_type := as.character(coord_type)]
harv.dt[, coord_type := 'plot']

# check plant functional types 
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]
harv.dt[pft == 'ferns', pft := 'herb']
harv.dt[pft == 'forbs', pft := 'herb']
harv.dt[pft == 'forbs comarum palustre', pft := 'herb']
harv.dt[pft == 'evergreen shrub dwarf', pft := 'shrub']
harv.dt[pft == 'decidiuous shrub dwarf', pft := 'shrub'] # spelled incorrectly in dataset
harv.dt[pft == 'decidiuous shrub erect', pft := 'shrub']
harv.dt[pft == 'rubus chamaemorus forbs', pft := 'shrub']
harv.dt[pft == 'decidiuous shrub tree', pft := 'tree'] # deal with this later b/c tree biomass not consistently measured
harv.dt[pft == 'evergreen shrub erect', pft := 'shrub']
harv.dt[pft == 'mosses', pft := 'bryophyte']
harv.dt[pft == 'lichens', pft := 'lichen']
harv.dt[pft == 'graminoids', pft := 'herb']
harv.dt[pft == 'graminoids grass', pft := 'herb']
harv.dt[pft == 'graminoids sedge', pft := 'herb']
harv.dt[pft == 'graminoids wood rush', pft := 'herb']
harv.dt[pft == 'graminoids common rush', pft := 'herb']

harv.dt <- harv.dt[pft != 'na']
harv.dt <- harv.dt[pft != 'litter']
harv.dt <- harv.dt[pft != 'soil']
unique(harv.dt$pft)

check_pfts(harv.dt)

# remove biomass density column beacuse it messes up calculation below
harv.dt <- harv.dt[, biomass_density_gm2 := NULL]

# compute total AGB for each PFT by summing over 'Vegetation_Description' column 
value.cols <- paste(c('vegetation_description','biomass_dry_weight_g'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      vegetation_description = first(vegetation_description)), 
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# compute biomass density 
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# expand data table so every pft occurs in every plot, populate with zeros, then fix trees
pft.dt <- expand_missing_pfts(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]
pft.dt <- fill_missing_metadata(pft.dt, 'vegetation_description', 'plot_id')
pft.dt <- fill_missing_metadata(pft.dt, 'notes', 'plot_id')

# adjust notes about "representativeness"  
pft.dt[, notes := paste0(str_to_sentence(notes), 'of a Landsat pixel.')]

# tree biomass not systematically measured despite a few plots have values. Set to unmeasured.
pft.dt[pft == 'tree' & biomass_dry_weight_g > 0, ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured')]
pft.dt[pft == 'tree' & vegetation_description == 'Birch forest', ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured')]

pft.dt[method == 'unmeasured', plot_area_m2 := NA]


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude), agb_gm2 = sum(biomass_density_gm2, na.rm = F)), 
                 by = c('plot_code','plot_id','vegetation_description','notes')]
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_code), ) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_code),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

# st_write(pts.sf, 'data/gis_data/tmp/siewert_sites.gpkg', append = F)

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# View(pft.dt)

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_05_siewert_sweden_abisko_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================
# 
# harv.dt[, notes := 'none']
# 
# pft.dt[pft == 'tree']
# 
# xx <- pft.dt[notes == 'low representativeness']
# 
# yy <- pft.dt[notes != 'low representativeness']
# 
# sort(unique(yy$vegetation_description))
# 
# xx <- pft.dt[vegetation_description == 'Birch forest' & pft == 'tree']
