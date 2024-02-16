# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: M.K Raynolds
# Script Author: Melissa Rose
# Script Date: 2022-06-14
# Dataset Notes: Missing eastern ATLAS-1 sites. Missing ATLAS-2 biomass for C and LAVA sites. 
# - 

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
A1_sites.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_12_raynolds_usa_alaska/ATLAS_Veg_Plots_1541/ATLAS_Veg_Plots_1541/data/atlas-1_environmental_data.csv'))
A1_biomass.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_12_raynolds_usa_alaska/ATLAS_Veg_Plots_1541/ATLAS_Veg_Plots_1541/data/atlas-1_biomass.csv'))
A2_sites.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_12_raynolds_usa_alaska/ATLAS_Veg_Plots_1541/ATLAS_Veg_Plots_1541/data/atlas-2_environmental_data.csv', fileEncoding = 'us-ascii'))
A2_biomass.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_12_raynolds_usa_alaska/ATLAS_Veg_Plots_1541/ATLAS_Veg_Plots_1541/data/atlas-2_biomass.csv', fileEncoding = 'us-ascii'))

# STANDARDIZE SITE DATA ======================================================================================

## ATLAS-1 SITES
# create new data tables with only the relevant columns
A1_sites.dt <- A1_sites.dt[, c('site', 'field_plot_number', 'date', 'latitude', 'longitude', 'plant_community_name', 'remarks')]

# add year, month, and day
A1_sites.dt[, year := substr(date, 1, 4)]
A1_sites.dt[, month := substr(date, 5, 6)]
A1_sites.dt[, day := substr(date, 7, 8)]
A1_sites.dt[, date := NULL]

# change site names to match biomass data
A1_sites.dt[, site := paste0(site, substr(field_plot_number, 3, 3))]
A1_sites.dt[site == 'Atqasuk1', site := 'Atqasuk'] 
A1_sites.dt[site == 'Barrow1', site := 'Barrow'] 
A1_sites.dt[, field_plot_number := NULL]

# collapse data table to show information for each site only once
veg_descrip <- A1_sites.dt[,.(vegetation_description=concat_rows(plant_community_name)), by=site]
A1_sites.dt <- A1_sites.dt[,.SD[c(1)],by=site]
A1_sites.dt[, plant_community_name := NULL]

# combine abridged site list with updated vegetation_description
A1_sites.dt <- A1_sites.dt %>%
  left_join(veg_descrip, by='site') %>%
  data.table()

# change remaining column names
names(A1_sites.dt)[names(A1_sites.dt) == 'site']  <- 'site_id'
names(A1_sites.dt)[names(A1_sites.dt) == 'remarks']  <- 'site_description'


## ATLAS-2 SITES
# create new data tables with only the relevant columns
A2_sites.dt <- A2_sites.dt[, c('releve', 'date', 'latitude', 'longitude', 'plant_community', 'site_location')]

# add year, month, and day
A2_sites.dt[, year := substr(date, 1, 4)]
A2_sites.dt[, month := substr(date, 5, 6)]
A2_sites.dt[, day := substr(date, 7, 8)]
A2_sites.dt[, date := NULL]

# remove any site data that does not overlap with biomass data 
A2_sites.dt <- A2_sites.dt[releve %in% c('QC-1', 'QC2-A', 'QC2-B', 'QC2-C', 'QC3-A', 'QC3-B')]

# create site_id
A2_sites.dt[, site_id := paste0('QuartzCreek', substr(releve, 3, 3))]
A2_sites.dt[site_id == 'QuartzCreek-', site_id := 'QuartzCreek1'] 

# fix incorrect lat/ long values 
A2_sites.dt[releve == 'QC3-A', latitude := 65.54790]
A2_sites.dt[releve == 'QC3-A', longitude := -163.4314]

# collapse data table to show information for each site only once
veg_descrip <- A2_sites.dt[,.(vegetation_description=concat_rows(plant_community)), by=site_id]
A2_sites.dt <- A2_sites.dt[,.SD[c(1)],by=site_id]
A2_sites.dt[, plant_community := NULL]
A2_sites.dt[, releve := NULL]

# combine abridged site list with updated vegetation_description
A2_sites.dt <- A2_sites.dt %>%
  left_join(veg_descrip, by='site_id') %>%
  data.table()

# change remaining column names
names(A2_sites.dt)[names(A2_sites.dt) == 'site_location']  <- 'site_description'

# combine A1 and A2 sites
sites.dt <- rbind(A1_sites.dt, A2_sites.dt)
dim(A1_sites.dt)
dim(A2_sites.dt)
dim(sites.dt)


# STANDARDIZE BIOMASS DATA ===================================================================================

## ATLAS-1 SITES
# create unique plot identifier
A1_biomass.dt[, plot := paste(site, grid_plot_number, sep='.')]

# keep only relevant columns 
A1_biomass.dt[, vegetation.type := NULL]
A1_biomass.dt[, site := NULL]
A1_biomass.dt[, grid_plot_number := NULL]

# convert litter from character to numeric 
A1_biomass.dt[, litter := as.numeric(litter)]

# move pft columns to rows 
A1_harv.dt <- pivot_longer(A1_biomass.dt, -plot, names_to = 'pft', values_to = 'biomass_density_gm2')
dim(A1_biomass.dt)
dim(A1_harv.dt)

# standardize pfts
unique(A1_harv.dt$pft)

# remove total and dead pft categories
A1_harv.dt <- data.table(A1_harv.dt)
A1_harv.dt <- A1_harv.dt[ pft != 'dead_deciduous_leaf']
A1_harv.dt <- A1_harv.dt[ pft != 'dead_evergreen_leaf']
A1_harv.dt <- A1_harv.dt[ pft != 'dead_graminoid']
A1_harv.dt <- A1_harv.dt[ pft != 'total']
A1_harv.dt <- A1_harv.dt[ pft != 'litter']
A1_harv.dt <- A1_harv.dt[ pft != 'total_evergreen_shrub']
A1_harv.dt <- A1_harv.dt[ pft != 'total_deciduous_shrub']
A1_harv.dt <- A1_harv.dt[ pft != 'total_graminoid']
A1_harv.dt <- A1_harv.dt[ pft != 'total_vascular']
A1_harv.dt <- A1_harv.dt[ pft != 'total_shrub']

# standardizing pft names
unique(A1_harv.dt$pft)
A1_harv.dt[pft == 'deciduous_shrub_stem', pft := 'shrub']
A1_harv.dt[pft == 'live_deciduous_leaf', pft := 'shrub']
A1_harv.dt[pft == 'deciduous_reproduction', pft := 'shrub']
A1_harv.dt[pft == 'evergreen_shrub_stem', pft := 'shrub']
A1_harv.dt[pft == 'live_evergreen_leaf', pft := 'shrub']
A1_harv.dt[pft == 'evergreen_reproduction', pft := 'shrub']
A1_harv.dt[pft == 'live_graminoid', pft := 'herb']
A1_harv.dt[pft == 'forb', pft := 'herb']
A1_harv.dt[pft == 'equisetum', pft := 'herb']  # horsetail
A1_harv.dt[pft == 'moss', pft := 'bryophyte']

unique(A1_harv.dt$pft)
check_pfts(A1_harv.dt)

# compute biomass density for each pft  
value.cols <- 'biomass_density_gm2'

grouping.cols <- grep(value.cols, colnames(A1_harv.dt), invert = T, value = T)

A1_pft.dt <- A1_harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)),
                  by = eval(grouping.cols)]
dim(A1_harv.dt)
dim(A1_pft.dt)


## ATLAS-2 SITES
# remove average and mean s.e. location rows
A2_biomass.dt <- A2_biomass.dt[ location != 'average']
A2_biomass.dt <- A2_biomass.dt[ location != 'mean s.e.']

# create unique plot identifier
A2_biomass.dt[, grid := paste0('QuartzCreek', substr(grid, 3, 3))]
A2_biomass.dt[, plot := paste(grid, location, sep='.')]

# keep only relevant columns 
A2_biomass.dt[, grid := NULL]
A2_biomass.dt[, location := NULL]

# convert columns from character to numeric 
A2_biomass.dt[, moss := as.numeric(moss)]
A2_biomass.dt[, lichen := as.numeric(lichen)]
A2_biomass.dt[, forb := as.numeric(forb)]
A2_biomass.dt[, horsetail := as.numeric(horsetail)]
A2_biomass.dt[, graminoid_live := as.numeric(graminoid_live)]
A2_biomass.dt[, graminoid_dead := as.numeric(graminoid_dead)]
A2_biomass.dt[, shrub_foliage_live := as.numeric(shrub_foliage_live)]
A2_biomass.dt[, shrub_foliage_dead := as.numeric(shrub_foliage_dead)]
A2_biomass.dt[, shrub_stem_live := as.numeric(shrub_stem_live)]
A2_biomass.dt[, shrub_stem_dead := as.numeric(shrub_stem_dead)]
A2_biomass.dt[, litter := as.numeric(litter)]
A2_biomass.dt[, total_no_litter := as.numeric(total_no_litter)]

# move pft columns to rows 
A2_harv.dt <- pivot_longer(A2_biomass.dt, -plot, names_to = 'pft', values_to = 'biomass_density_gm2')
dim(A2_biomass.dt)
dim(A2_harv.dt)

# standardize pfts
unique(A2_harv.dt$pft)

# remove total and dead pft categories
A2_harv.dt <- data.table(A2_harv.dt)
A2_harv.dt <- A2_harv.dt[ pft != 'graminoid_dead']
A2_harv.dt <- A2_harv.dt[ pft != 'shrub_foliage_dead']
A2_harv.dt <- A2_harv.dt[ pft != 'shrub_stem_dead']
A2_harv.dt <- A2_harv.dt[ pft != 'litter']
A2_harv.dt <- A2_harv.dt[ pft != 'total_no_litter']

# standardizing pft names
A2_harv.dt[pft == 'shrub_foliage_live', pft := 'shrub']
A2_harv.dt[pft == 'shrub_stem_live', pft := 'shrub']
A2_harv.dt[pft == 'graminoid_live', pft := 'herb']
A2_harv.dt[pft == 'forb', pft := 'herb']
A2_harv.dt[pft == 'horsetail', pft := 'herb']
A2_harv.dt[pft == 'moss', pft := 'bryophyte']

unique(A2_harv.dt$pft)
check_pfts(A2_harv.dt)

# compute biomass density for each pft  
value.cols <- 'biomass_density_gm2'

grouping.cols <- grep(value.cols, colnames(A2_harv.dt), invert = T, value = T)

A2_pft.dt <- A2_harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm=T)),
                        by = eval(grouping.cols)]
dim(A2_harv.dt)
dim(A2_pft.dt)

# combine A1 and A2 pfts
pft.dt <- rbind(A1_pft.dt, A2_pft.dt)
dim(A1_pft.dt)
dim(A2_pft.dt)
dim(pft.dt)

# create site_id and plot_id for pft.dt 
pft.dt[, site_id := word(plot, 1, sep = fixed('.'))]
pft.dt[, plot_id := word(plot, 2, sep = fixed('.'))]
pft.dt[, plot := NULL]

# combine site.dt and plot.dt
pft.dt <- pft.dt %>%
  left_join(sites.dt, by='site_id') %>%
  data.table()

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
pft.dt[, ':='(dataset_id = 'ds12',
               contributor = 'Raynolds M',
               country = 'USA',
               plot_area_m2 = 0.1,
               coord_type = 'site',
               method = 'harvest',
               citation = 'Raynolds M. 2018. Arctic Vegetation Plots ATLAS Project North Slope and Seward Peninsula, AK, 1998-2000. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1541.',
               citation_short = 'Raynolds 2018', 
               notes = 'none')]

# specify locale
sort(unique(pft.dt$site_id))
pft.dt[grep('QuartzCreek', site_id), locale := 'Seward Peninsula']
pft.dt[grep('QuartzCreek', site_id, invert = T), locale := 'North Slope']

# create site and plot codes
pft.dt[, site_code := paste(country, gsub(' ','', locale), site_id, sep='.')]
pft.dt[, plot_code := paste(country, gsub(' ','', locale), site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
pft.dt[, latitude := round(latitude, 6)]
pft.dt[, longitude := round(longitude, 6)]

# STANDARDIZE VEGETATION DATA ==================================================
unique(pft.dt$pft)

# change moss to bryophyte
pft.dt[pft == 'moss', pft := 'bryophyte']

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand vegetation_description to all pfts in each plot
pft.dt[, vegetation_description := unique(na.omit(vegetation_description)),
       by = .(plot_code)]


pft.dt

# set tree biomass from NA to zero because no trees present
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, 
                           biomass_density_gm2 = 0, 
                           method = 'survey')]

# expand notes to all pfts in each plot
pft.dt[, notes := unique(na.omit(notes)),
       by = .(plot_code)]

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'site_id']
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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_12_raynolds_usa_alaska_standardized.csv'
fwrite(pft.dt, out.file)
# fwrite(pft.dt, out.file, eol = '\r')

# END SCRIPT =================================================================================================