# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Elyn Humphreys (Carleton University)
# Script Author: Logan Berner
# Script Date: 2023-09-08
# Dataset Notes: 
# 
# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(R.utils)
require(data.table)
require(tidyr)
require(dplyr)
require(sf); sf_use_s2(FALSE)
require(leaflet)
require(stringr)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_26to29_humphreys_canada_daringlake/Arctic biomass data intake form v2 - Carleton_eh.xlsx', sheet = 2))

# ADD METADATA ======================================================================================

# convert names to lower case
names(harv.dt) <- tolower(names(harv.dt))
names(harv.dt)

harv.dt[, notes := 'none']

# add dasaset ID, contributor, and citation
harv.dt[dataset_id == "skaarup - CONSOLIDATEBIOMASS", ':='(dataset_id = 'ds26',
                                                           contributor = 'Humphreys E, Skaarup E',
                                                           citation = 'Skaarup E. 2017. The impacts of shrub abundance on microclimate and decomposition in the Canadian Low Arctic. Carleton University Department of Geography and Environmental Studies, Ottawa, Ontario, Canada.',
                                                           citation_short = 'Skaarup 2017')]

harv.dt[dataset_id == "Piquette - biomass", ':='(dataset_id = 'ds27',
                                                 contributor = 'Humphreys E, Piquette S, Hayne S',
                                                 citation = 'Hayne SL. 2010. Controls on atmospheric exchanges of carbon dioxide and methane for a variety of Arctic tundra types. Carleton University Department of Geography and Environmental Studies, Ottawa, Ontario, Canada.',
                                                 citation_short = 'Hayne 2010')]


harv.dt[dataset_id == "dl soil and veg eh" & year == 2006, ':='(dataset_id = 'ds28',
                                                                contributor = 'Humphreys E',
                                                                citation = 'Lafleur PM and Humphreys ER. 2008. Spring warming and carbon dioxide exchange over low Arctic tundra in central Canada. Global Change Biology 14:740-756.',
                                                                citation_short = 'Lafleur and Humphreys 2008')]

harv.dt[dataset_id == "dl soil and veg eh" & year > 2006, ':='(dataset_id = 'ds29',
                                                               contributor = 'Humphreys E',
                                                               citation = 'Lafleur PM and Humphreys ER. 2018. Tundra shrub effects on growing season energy and carbondioxide exchange. Environmental Research Letters 13:055001.',
                                                               citation_short = 'Lafleur and Humphreys 2018')]

# specify locale
harv.dt[, ":="(locale = 'Daring Lake')]


# convert month from character to numeric
unique(harv.dt$month)
harv.dt[month == 'July', month := '7']
harv.dt[month == 'August', month := '8']
harv.dt[month == 'July/ August', month := '8']
harv.dt[, month := as.numeric(month)]


# adjust site and plot IDs
harv.dt[, site_id := gsub('CA-DL','Tower', site_id)]

dim(harv.dt)

sort(unique(harv.dt$site_id))
sort(unique(harv.dt$plot_id))

harv.dt[site_id == 'Tower1' & plot_id %in% c('A','B','C','D'), site_id := 'Tower1-DwarfShrub']

harv.dt[site_id == 'Tower1' & plot_id %in% c(1:5, paste0('A',1:5)), site_id := 'Tower1-MixedTundra']

harv.dt[site_id == 'Tower1' & grepl('Heath', plot_id), ':='(site_id = 'Tower1-Heath',
                                                            plot_id = gsub('Heath-','',plot_id))]

harv.dt[site_id == 'Tower1' & grepl('Shrub', plot_id), ':='(site_id = 'Tower1-Shrub',
                                                            plot_id = gsub('Shrub-','',plot_id))]

harv.dt[site_id == 'Tower1' & grepl('SedgeHollow', plot_id), ':='(site_id = 'Tower1-SedgeHollow',
                                                                  plot_id = gsub('SedgeHollow-','',plot_id))]

harv.dt[site_id == 'Tower1' & grepl('SedgeTussock', plot_id), ':='(site_id = 'Tower1-SedgeTussock',
                                                                   plot_id = gsub('SedgeTussock-','',plot_id))]

harv.dt <- harv.dt[(site_id == 'Tower2' & plot_id %in% paste0('A',1:6)) == F] # drop some incomplete measurements
harv.dt[site_id == 'Tower2', site_id := 'Tower2-SedgeFen']
harv.dt[site_id == 'Tower2-SedgeFen', plot_id := substr(plot_id, nchar(plot_id), nchar(plot_id))]

harv.dt[site_id == 'Tower3' & plot_id %in% 1:10, site_id := 'Tower3-DwarfShrub-1']
harv.dt[site_id == 'Tower3' & plot_id %in% c('A','C','D'), site_id := 'Tower3-DwarfShrub-2']

harv.dt[site_id == 'Tower4' & plot_id %in% 1:5, site_id := 'Tower4-LowShrub-1']
harv.dt[site_id == 'Tower4' & plot_id %in% c('A','B','C'), site_id := 'Tower4-LowShrub-2']

# create site and plot codes
harv.dt[, site_code := paste(country, gsub('\\ ','', locale), site_id, sep='.')]
harv.dt[, plot_code := paste(country, gsub('\\ ','', locale), site_id, plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)


# subset coordinates in DMS, convert to DD, join back with other DD coordinates
harv.tmp.dt <- harv.dt[grep('°', latitude)]
harv.dt <- harv.dt[grep('°', latitude, invert = T)]

harv.tmp.dt <- separate(harv.tmp.dt, col = latitude, into = c('lat.deg','lat.min'), sep = '°', convert = T)
harv.tmp.dt <- separate(harv.tmp.dt, col = longitude, into = c('lon.deg','lon.min'), sep = '°', convert = T)
harv.tmp.dt <- as.data.table(harv.tmp.dt)

harv.tmp.dt[, ':='(latitude = lat.deg + lat.min/60, 
                   longitude = lon.deg - lon.min/60)] # lon is already negative, so subtract min

harv.tmp.dt <- harv.tmp.dt[, c('lat.deg','lat.min','lon.deg','lon.min') := NULL]

harv.dt[, ':='(latitude = as.numeric(latitude),
               longitude = as.numeric(longitude))]

harv.dt <- rbind(harv.dt, harv.tmp.dt)
rm(harv.tmp.dt)

harv.dt[, ':='(latitude = round(latitude, 6),
               longitude = round(longitude, 6))]

fivenum(harv.dt$longitude)

# adjust coordinates type
unique(harv.dt$coord_type)
harv.dt[coord_type == "site (+/- 100 m)", coord_type := 'site']

# in original dataset, the sites below around Tower 1 all had the same coordinates.
# Elyn then provided a land cover map to help specify more nuance representative locations 
harv.dt[site_id == 'Tower1-Heath', ':='(latitude = 64.86885, longitude = -111.57695, notes = 'Coordinates are for a representative location since specific site coordinates were not recorded.')]
harv.dt[site_id == 'Tower1-MixedTundra', ':='(latitude = 64.86890, longitude = -111.57414, notes = 'Coordinates are for a representative location since specific site coordinates were not recorded.')]
harv.dt[site_id == 'Tower1-SedgeHollow', ':='(latitude = 64.87081, longitude = -111.581289, notes = 'Coordinates are for a representative location since specific site coordinates were not recorded.')]
harv.dt[site_id == 'Tower1-SedgeTussock', ':='(latitude = 64.86611, longitude = -111.57093, notes = 'Coordinates are for a representative location since specific site coordinates were not recorded.')]
harv.dt[site_id == 'Tower1-Shrub', ':='(latitude = 64.86850, longitude = -111.57003, notes = 'Coordinates are for a representative location since specific site coordinates were not recorded.')]

# sort 
setorder(harv.dt, site_id, plot_id)


# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# drop dead vegetation
unique(harv.dt$vegetation_description)
harv.dt[, vegetation_description := tolower(as.character(vegetation_description))]
harv.dt <- harv.dt[grep('dead', vegetation_description, invert = T)] # drop dead veg
harv.dt <- harv.dt[, vegetation_description := gsub(' (live)','', vegetation_description, fixed = T)]
unique(harv.dt$vegetation_description)

harv.dt[vegetation_description == '?', vegetation_description := 'unknown']

# update species name per comment from Elyn 
harv.dt[vegetation_description %in% c('ledum spp.', 'ledum groenlandicum', 'rhododendron spp.'),   
        vegetation_description := 'Rhododendron tomentosum']

harv.dt[, vegetation_description := str_to_sentence(vegetation_description)]

# standardize plant functional types 
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)
harv.dt[pft == 'gramanoid' | pft == 'graminoid', pft := 'herb']

unique(harv.dt$pft)
check_pfts(harv.dt)


# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('biomass_dry_weight_g','biomass_density_gm2','vegetation_description'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      biomass_density_gm2 = sum(biomass_density_gm2),
                      vegetation_description = concat_rows(vegetation_description)),
                  by = grouping.cols]

dim(harv.dt)
dim(pft.dt)

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set any missing herb and shrub biomass to 0 b/c always measured 
pft.dt[pft %in% c('herb','shrub') & is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'harvest', plot_area_m2 = 0.25, vegetation_description = 'none')]

# set tree biomass to zero (treeless tundra)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey', plot_area_m2 = 0.25, vegetation_description = 'none')]

# set bryophyte and lichen biomass to unmeasured
pft.dt[pft %in% c('bryophyte','lichen'), ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured', plot_area_m2 = NA, vegetation_description = 'none')]

pft.dt[is.na(notes), notes := 'none']

pft.dt[method == 'unmeasured', plot_area_m2 := NA]


fill_missing_metadata(pft.dt, 'site_description', 'plot_code')
fill_missing_metadata(pft.dt, 'notes', 'plot_code')

pft.dt

dim(pft.dt)


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('plot_code')]
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

# st_write(pts.sf, 'data/gis_data/tmp/humphreys_plots.gpkg')

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# WRITE OUTPUT ===============================================================================================
unique(pft.dt$dataset_id)
fwrite(pft.dt[dataset_id == 'ds26'], 'data/synthesis_database/3_contributed_data_processed/dataset_26_humphreys_canada_darkinglake_standardized.csv')
fwrite(pft.dt[dataset_id == 'ds27'], 'data/synthesis_database/3_contributed_data_processed/dataset_27_humphreys_canada_darkinglake_standardized.csv')
fwrite(pft.dt[dataset_id == 'ds28'], 'data/synthesis_database/3_contributed_data_processed/dataset_28_humphreys_canada_darkinglake_standardized.csv')
fwrite(pft.dt[dataset_id == 'ds29'], 'data/synthesis_database/3_contributed_data_processed/dataset_29_humphreys_canada_darkinglake_standardized.csv')

# END SCRIPT =================================================================================================