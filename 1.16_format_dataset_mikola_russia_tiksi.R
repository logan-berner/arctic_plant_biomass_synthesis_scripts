# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Juha Mikola, LUKE, Finland
# Script Author: Logan Berner
# Script Date: 2023-01-11
# Dataset Notes:
# - bryophyte biomass was estimated from a 5x5 cm harvest and then scaled to the plot using visual estimates of percent cover
# - Create site_ids by grouping plots by land cover type? 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(tidyr)
require(dplyr)
require(sf)
require(leaflet)
require(stringr)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
colnames(database.tmplt.dt)
harv.dt <- data.table(read_xlsx('data/synthesis_database/2_contributed_data_raw/dataset_16_mikola_russia_tiksi/Mikola et al. 2018_Biogeosciences_with_coordinates.xlsx'))
meta.dt <-  data.table(read_xlsx('data/synthesis_database/2_contributed_data_raw/dataset_16_mikola_russia_tiksi/Mikola et al. 2018_Biogeosciences_harvest dates and lichen cover.xlsx'))

# STANDARDIZE SITE DATA ======================================================================================

# subset columns to keep
colnames(harv.dt) <- tolower(colnames(harv.dt))
col.names <- colnames(harv.dt); col.names
keep.cols <- col.names[c(1,4,5,6,15:21)]; keep.cols
harv.dt <- harv.dt[, keep.cols, with = F]

# adjust column names
colnames(harv.dt) <- gsub(pattern = ' dry mass (g m-2)', replacement = '', x = colnames(harv.dt), fixed = T)
colnames(harv.dt) <- gsub(pattern = ' in decimals', replacement = '', x = colnames(harv.dt), fixed = T)
setnames(harv.dt, c('study plot','land cover type'), c('plot_id','vegetation_description'))

setnames(meta.dt, c('Study plot','Harvest date','Lichen cover percent'), c('plot_id','date','lichen_cover'))

# melt dataset into long format 
harv.dt <- melt(harv.dt, id.vars = 1:4, variable.name = 'pft', value.name = 'biomass_density_gm2')

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds16',
               contributor = 'Mikola J, Virtanen T, Aurela M, Nyman J',
               country = 'Russia',
               locale = 'Tiksi',
               plot_area_m2 = 0.45*0.45,
               coord_type = 'plot',
               method = 'harvest',
               citation = 'Mikola et al. 2018. Data from: Spatial variation and linkages of soil and vegetation in the Siberian Arctic tundra coupling field observations with remote sensing data, Dryad, Dataset, https://doi.org/10.5061/dryad.8382j4r',
               citation_short = 'Mikola et al. 2018',
               site_description = 'Tundra lowlands and gently sloping hillslopes near Arctic Ocean',
               notes = 'none')]

# set site_id to vegetation type
harv.dt[, site_id := gsub(' ', '', str_to_title(harv.dt$vegetation_description))]

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# add harvest date and lichen cover from metadata 
harv.dt <- harv.dt[meta.dt, on = 'plot_id']
harv.dt[, ':='(day = substr(date, 1, 2),
               month = substr(date, 4, 4),
               year = substr(date, 6, 9)),]

harv.dt <- harv.dt[, c('date') := NULL]


# STANDARDIZE UNDERSTORY DATA =================================================================================
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]

# change pft names
harv.dt[pft == 'sphagnum', pft := 'bryophyte']
harv.dt[pft == 'other moss', pft := 'bryophyte']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'dwarf shrub', pft := 'shrub']
harv.dt[pft == 'betula nana', pft := 'shrub']
harv.dt[pft == 'salix', pft := 'shrub']
check_pfts(harv.dt)

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('vegetation_description','biomass_density_gm2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                  vegetation_description = unique(na.omit(vegetation_description))),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# change method to harvest + survey for bryophyte 
pft.dt[pft == 'bryophyte', method := 'harvest + survey']

# expand data table so every pft occurs in every plot and then specify whether
# the pft data are missing because the pft was not present, or it wasn't harvested
pft.dt <- expand_missing_pfts(pft.dt)

pft.dt[pft == 'lichen' & lichen_cover == 0, ':='(biomass_dry_weight_g = 0, 
                                                 biomass_density_gm2 = 0,
                                                 method = 'survey')]

pft.dt[pft == 'lichen' & lichen_cover != 0, ':='(biomass_dry_weight_g = NA, 
                                                 biomass_density_gm2 = NA, 
                                                 method = 'unmeasured')]

pft.dt[pft == 'lichen', notes := paste0('Lichen cover ', lichen_cover, '%.')]
pft.dt <- pft.dt[, lichen_cover := NULL]

pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, 
                           biomass_density_gm2 = 0,
                           method = 'survey')]

pft.dt[is.na(notes), notes := 'none']

pft.dt[method == 'unmeasured', plot_area_m2 := NA]

pft.dt <- fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

setcolorder(pft.dt, colnames(database.tmplt.dt)) 

View(pft.dt)

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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_16_mikola_russia_tiksi_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================