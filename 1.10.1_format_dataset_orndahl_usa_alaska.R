# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Katie Orndahl (Northern Arizona University)
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-06-07, 2023-03-09
# Dataset Notes: 
# - missing bryophyte biomass and tree biomass, but can set tree biomass to zero where cover was zero

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
site.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_10_orndahl_usa_alaska/site_attributes.csv'))
biomass.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_10_orndahl_usa_alaska/biomass.csv'))
# ne_download(type = 'admin_1_states_provinces', scale = 'large', destdir = 'data/gis_data/')
admin.sf <- st_read('data/gis_data/boundaries/ne_10m_admin_1_states_provinces.shp')

# STANDARDIZE SITE DATA ======================================================================================

# subset biomass data to relevant columns
biomass.dt <- biomass.dt[metric == 'ci', c('site_code', 'quadrat_num', 'PFT', 'biomass_observed')]

# subset site data to relevant columns
site.dt <- site.dt[, c('site_code', 'sample_date', 'lat', 'long', 'land_cover_text', 'cavm_cbvm_fine', 'cavm_cbvm_coarse', 'num_species')]

# add year, month, and day
site.dt <- separate(site.dt, 'sample_date', c('month', 'day', 'year'), sep = '/') %>% data.table()

# move site-level descriptions to site_description 
site.dt[, site_description := paste0(cavm_cbvm_fine,' with ', num_species, ' species.')]
site.dt <- site.dt[, c('cavm_cbvm_fine', 'cavm_cbvm_coarse', 'num_species') := NULL]

# move land_cover_text to vegetation_description
site.dt[, vegetation_description := paste0(land_cover_text, ' Tundra')]
site.dt <- site.dt[, land_cover_text := NULL]

# determine country and locale of each site
site.sf <- st_as_sf(site.dt, coords = c("long", "lat"), crs = 4326, agr = "constant") %>% st_cast('POINT')
admin.sf <- admin.sf %>% select('name')
site.sf <- st_intersection(site.sf, admin.sf)
site.dt[, locale := site.sf$name]
site.dt[locale == 'Yukon', country := 'Canada']
site.dt[locale == 'Alaska', ':='(locale = 'Interior Alaska', country = 'USA')]

# join biomass and site data
harv.dt <- site.dt[biomass.dt, on = 'site_code']

# change column names
names(harv.dt)
setnames(harv.dt, 
         c('site_code','quadrat_num','PFT','biomass_observed','lat','long'),
         c('site_id','plot_id','pft','biomass_density_gm2','latitude','longitude'))

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ':='(dataset_id = 'ds10',
               contributor = 'Orndahl K',
               plot_area_m2 = 0.25,
               coord_type = 'site',
               method = 'harvest',
               citation = 'Orndahl. 2022. Mapping tundra ecosystem plant functional type cover, height and aboveground biomass in Alaska and northwest Canada using unmanned aerial vehicles, 2018-2019. Arctic Data Center, doi:10.18739/A2R785Q5B',
               citation_short = 'Orndahl 2022',
               notes = 'none')]

# create site and plot codes
harv.dt[, site_code := paste(country, gsub(' ','', locale), site_id, sep='.')]
harv.dt[, plot_code := paste(country, gsub(' ','', locale), site_id, plot_id, sep='.')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]


# STANDARDIZE PFT DATA =======================================================================================

# standardize pfts
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]

# remove total AGB
harv.dt <- harv.dt[pft != 'total'] 
unique(harv.dt$pft)

# standardizing pft names
harv.dt[pft == 'deciduous shrubs', pft := 'shrub']
harv.dt[pft == 'evergreen shrubs', pft := 'shrub'] 
harv.dt[pft == 'forbs', pft := 'herb']
harv.dt[pft == 'graminoids', pft := 'herb']
harv.dt[pft == 'lichens', pft := 'lichen']
unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each pft  
value.cols <- 'biomass_density_gm2'

grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)),
                       by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]
dim(pft.dt)

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# fill in non-moss with zeros biomass values and keep moss as NA because it was not harvested
pft.dt[is.na(biomass_dry_weight_g) & pft != 'bryophyte', biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2) & pft != 'bryophyte', biomass_density_gm2 := 0]
pft.dt[pft == 'bryophyte', method := 'unmeasured']

# fill vegetation_description and notes based on plot_code
pft.dt <- fill_missing_metadata(pft.dt, 'vegetation_description', 'plot_code')
pft.dt <- fill_missing_metadata(pft.dt, 'notes', 'plot_code')

# change sites with trees in cover data to NA instead of 0 AGB (see note from K. Orndahl)
pft.dt[site_id == 'BUTLER' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'COALCREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'GRANITE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'FIRTH' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'GULCH' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'KINGSOLOMON' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'MATSONCREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'MOLLYCREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'MOSQUITOCREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'OBRIAN' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'OGILVIE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'PEDRO' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'PINNELL' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'PORCUPINECREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'PRINDLE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'QUARTZ02' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'SEELA' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'SHEEPCREEK' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'SIXTYMILE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'STONE01' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'STONE02' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'THISTLE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'TOPOFTHEWORLD' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'TWELVEMILE' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]
pft.dt[site_id == 'WICKERSHAM' & pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := NA]

pft.dt[pft == 'tree' & biomass_density_gm2 == 0, method := 'survey']
pft.dt[pft == 'tree' & is.na(biomass_density_gm2), method := 'unmeasured']

pft.dt[method == 'unmeasured', plot_area_m2 := NA]

# CHECK PFT SUMMARIES =========================================================================================
pft.dt[, .(biomass_density_gm2_avg = mean(biomass_density_gm2),
           biomass_dry_weight_g_avg = mean(biomass_dry_weight_g),
           biomass_density_gm2_sum = sum(biomass_density_gm2),
           biomass_dry_weight_g_sum = sum(biomass_dry_weight_g)), by = c('year','pft')]


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


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_10_orndahl_usa_alaska_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================