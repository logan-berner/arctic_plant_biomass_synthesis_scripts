# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Howie Epstein
# Script Author: Logan Berner
# Script Date: 2023-03-22
# Dataset Notes:
# -

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
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_19_epstein_russia_yamal/ALL_RV_data20101001.xls', sheet = 3))
meta.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_19_epstein_russia_yamal/all_RV_DescriptionsPhotos_110601.xls', skip = 4))

# STANDARDIZE SITE DATA ======================================================================================

# subset columns to keep
col.names <- colnames(meta.dt); col.names
keep.cols <- col.names[c(1:5,8:9)]; keep.cols
meta.dt <- meta.dt[, keep.cols, with = F]

# adjust column names
names(meta.dt) <- tolower(names(meta.dt))
names(meta.dt)
setnames(meta.dt, 
         c('releve','location','study site','characteristic species','gps north','gps east'), 
         c('plot_id','site_id','site_description','vegetation_description','latitude','longitude'))

names(harv.dt)
setnames(harv.dt, c('location','revele'), c('site_id','plot_id'))

# adjust plot_id in harvest data
harv.dt[, plot_id := substr(plot_id, 7,8)]

# melt dataset into long format 
harv.dt <- melt(harv.dt, id.vars = 1:2, variable.name = 'pft', value.name = 'biomass_density_gm2')

# add coordinates, harvest date, and descriptions
harv.dt <- meta.dt[harv.dt, on = c('site_id','plot_id')]

# parse year / month / day from date 
harv.dt <- separate(harv.dt, date, c('year','month','day'), '-') %>% data.table()

# adjust coordinates from degrees decimal minutes to decimal degrees
harv.dt <- separate(harv.dt, latitude, c('lat.deg','lat.min'), ' ')
harv.dt <- separate(harv.dt, longitude, c('lon.deg','lon.min'), ' ') %>% data.table()

harv.dt[, ':='(latitude = as.numeric(lat.deg) + as.numeric(lat.min) / 60, 
               longitude = as.numeric(lon.deg) + as.numeric(lon.min) / 60)]

harv.dt <- harv.dt[, c('lat.deg','lat.min','lon.deg','lon.min') := NULL]

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds19',
               contributor = 'Epstein H, Forbes B, Walker D',
               plot_area_m2 = 0.1,
               coord_type = 'plot',
               method = 'harvest',
               citation = 'Walker et al. 2012. Environment, vegetation and greenness (NDVI) along the North America and Eurasia Arctic transects. Environmental Research Letters 7:015504.',
               citation_short = 'Walker et al. 2012',
               notes = 'none')]

# specify country and locale of each site
harv.dt[, country := 'Russia']
harv.dt[, locale  := 'Yamal']
harv.dt[site_id == 'Krenkel-1' | site_id == 'Krenkel-2', locale := 'Heiss Island']

# create site and plot codes
harv.dt[, site_code := paste(country, gsub(' ', '', locale), gsub(' ', '', site_id), sep='.')]
harv.dt[, plot_code := paste(country, gsub(' ', '', locale), gsub(' ', '', site_id), plot_id, sep='.')]


# STANDARDIZE HARVEST DATA =================================================================================

# set trace biomass to zero and cast column as numeric
harv.dt[biomass_density_gm2 == 'T', biomass_density_gm2 := 0]
harv.dt[, biomass_density_gm2 := as.numeric(biomass_density_gm2)]

# consolidate pfts
unique(harv.dt$pft)
harv.dt[pft == 'deciduous stem', pft := 'shrub']
harv.dt[pft == 'deciduous live foliar', pft := 'shrub']
harv.dt[pft == 'deciduous reproductive', pft := 'shrub']
harv.dt[pft == 'evergreen stem', pft := 'shrub']
harv.dt[pft == 'evergreen live foliar', pft := 'shrub']
harv.dt[pft == 'evergreen reproductive', pft := 'shrub']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'broadleaf deciduous trees', pft := 'tree']
harv.dt[pft == 'needleleaf deciduous trees', pft := 'tree']
harv.dt[pft == 'evergreen trees', pft := 'tree']
harv.dt[, pft := as.character(pft)]
unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('vegetation_description','biomass_density_gm2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                  vegetation_description = unique(na.omit(vegetation_description))),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# update methods and plot size for trees 
pft.dt[pft == 'tree', ':='(method = 'survey', plot_area_m2 = 100)]

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- harv.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_id']
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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_19_epstein_russia_yamal_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================
