# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Fiona Gregory
# Script Author: Logan Berner
# Script Date: 2023-03-30
# Dataset Notes:
# - Experimental design includes six "blocks" each with 2 - 3 harvest samples.
# - Each harvest quadrat is small (10x10 cm for vascular and 5x3 cm for moss).
# - Therefore, we consider this to be one site with six plots, and pool samples w/in plots

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
vasc.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_21_gignac_canada_bylot/Biomass_Bylot_Gignac_2019_Control.xlsx', sheet = 1, skip = 8))
moss.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_21_gignac_canada_bylot/Biomass_Bylot_Gignac_2019_Control.xlsx', sheet = 2, skip = 8))
meta.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_21_gignac_canada_bylot/Biomass_Bylot_Gignac_2019_Control.xlsx', sheet = 3))


# STANDARDIZE SITE DATA ======================================================================================

# adjust metadata column names
names(meta.dt) <- tolower(names(meta.dt))
names(meta.dt)
setnames(meta.dt, 
         c('block','lat','long'), 
         c('plot_id','latitude','longitude'))

# convert coordinates from DMS to DD
meta.dt <- separate(meta.dt, col = 'latitude', into = c('lat.deg','lat.min','lat.sec'), sep = ' ')
meta.dt <- separate(meta.dt, col = 'longitude', into = c('lon.deg','lon.min','lon.sec'), sep = ' ')
meta.dt <- data.table(meta.dt)

meta.dt[, ':='(lat.deg = gsub('°','', lat.deg),
               lat.min = gsub("\'", '', lat.min),
               lat.sec = gsub('\"N', '', lat.sec),
               lon.deg = gsub('°','', lon.deg),
               lon.min = gsub("\'", '', lon.min),
               lon.sec = gsub('\"W', '', lon.sec))]

meta.dt[, c(names(meta.dt)) := lapply(.SD, as.numeric), .SDcols=names(meta.dt)]

meta.dt[, ':='(latitude = lat.deg + lat.min / 60 + lat.sec / 3600,
               longitude = (lon.deg + lon.min / 60 + lon.sec / 3600)*-1)]

meta.dt <- meta.dt[, c('lat.deg','lat.min','lat.sec','lon.deg','lon.min','lon.sec') := NULL]

# subset and adjust harvest data column names
names(vasc.dt) <- tolower(names(vasc.dt))
names(vasc.dt)
vasc.dt <- vasc.dt[, c(1,3,4), with = F]
setnames(vasc.dt, c('block','sample (0.01 m2)','biomass (g)'), c('plot_id','sample_id','biomass_dry_weight_g'))
vasc.dt[, pft := 'herb'] 

names(moss.dt) <- tolower(names(moss.dt))
names(moss.dt)
moss.dt <- moss.dt[, c(1,3,4), with = F]
setnames(moss.dt, c('block','sample (0.0015 m2)','biomass (g)'), c('plot_id','sample_id','biomass_dry_weight_g'))
moss.dt[, pft := 'bryophyte'] 

harv.dt <- rbind(vasc.dt, moss.dt)

# pool samples w/in plots, add plot size and note about pooling
harv.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g), 
                       n.samples = .N),
                   by = c('plot_id','pft')]


harv.dt[pft == 'herb', ":="(plot_area_m2 = 0.01 * n.samples,
                            notes = paste('Pooled', n.samples, 'samples from 10 cm x 10 cm subplots', sep = ' '))]

harv.dt[pft == 'bryophyte', ":="(plot_area_m2 = 0.0015 * n.samples,
                            notes = paste('Pooled', n.samples, 'samples from 5 cm x 3 cm subplots', sep = ' '))]

harv.dt <- harv.dt[, c('n.samples') := NULL]
  
# add coordinates, harvest date, and descriptions
harv.dt <- meta.dt[harv.dt, on = 'plot_id']

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds21',
               contributor = 'Gignac C, Gauthier G',
               country = 'Canada',
               locale = 'Bylot Island',
               site_id = 'Qarlikturvik Valley',
               coord_type = 'plot',
               year = 2019,
               month = 8,
               method = 'harvest',
               citation = 'Gignac et al. 2022. N/P addition is more likely than N addition alone to promote a transition from moss-dominated to graminoid-dominated tundra in the High-Arctic. Atmosphere 13:676.',
               citation_short = 'Gignac et al. 2022')]

# create site and plot codes
harv.dt[, site_code := paste(country, 'BylotIsland', 'QarlikturvikValley', sep='.')]
harv.dt[, plot_code := paste(country, 'BylotIsland', 'QarlikturvikValley', plot_id, sep='.')]

# calculate biomass dry weight
harv.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]


# STANDARDIZE HARVEST DATA =================================================================================
dim(harv.dt)
unique(harv.dt$pft)
harv.dt[, pft := as.character(pft)]
check_pfts(harv.dt)

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(harv.dt)
dim(pft.dt)

# set lichen, shrub and tree biomass to zero
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, 
                                         biomass_density_gm2 = 0,
                                         notes = 'none')]

# set methods for tree to survey
pft.dt[pft == 'tree', method := 'survey']

# specify day of harvests using midpoints: Aug 12-13 for non-vascular and Aug 3-5 for vascular
pft.dt[pft == 'herb' | pft == 'shrub' | pft == 'tree', day := 4]
pft.dt[pft == 'bryophyte' | pft == 'lichen', day := 12]

# set general veg and site descriptions
pft.dt[, ':='(vegetation_description = 'Arctic fen vegetation dominated by graminoids (e.g., Carex aquatilis, Eriophorum scheuchzeri, Dupontia fischeri) and brown mosses (e.g., Aulacomnium acuminatum, Aulacomnium turgidum, Scorpidium revolvens)',
              site_description = 'Productive freshwater wetlands composed of peat polygons, small lakes, and pond aggregations surrounded by upland, mesic vegetation')]


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_id']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant") %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_id)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_id),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textsize = '15px',
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_21_gignac_canada_bylot_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================