# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Susan Natali (Woods Hole Research Center)
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-05-19, 2023-03-09
# Dataset Notes:
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(tidyr)
require(dplyr)
require(leaflet)
require(sf)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))

site.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_07_natali_usa_alaska/ViPER_tree_biomass_site_information_2015.csv'))
coord.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_07_natali_usa_alaska/ViPER_US_biomass_coordinates_2015.csv'))
pft_us.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_07_natali_usa_alaska/ViPER_US_biomass_2015.csv'))
species_us.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_07_natali_usa_alaska/ViPER_US_biomass_shrub_bd_2015.csv'))
species_tr.dt <- data.table(read.csv('data/synthesis_database/2_contributed_data_raw/dataset_07_natali_usa_alaska/ViPER_tree_biomass_2015.csv'))


# STANDARDIZE SITE DATA ======================================================================================

# remove X column
harv.dt <- pft_us.dt[, X := NULL]

# change column names
names(harv.dt)[names(harv.dt) == 'Site']  <- 'site_id'
names(harv.dt)[names(harv.dt) == 'Transect']  <- 'plot_id'
names(harv.dt)[names(harv.dt) == 'FG']  <- 'pft'
names(harv.dt)[names(harv.dt) == 'AGB']  <- 'biomass_density_gm2'  #AGB is given as grams per square meter

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds07',
               contributor = 'Natali S, Kholodov A, Loranty M',
               country = 'USA',
               plot_area_m2 = 0.25,
               coord_type = 'plot',
               method = 'harvest',
               citation = 'Natali et al. 2014. Collaborative Research: Vegetation and Ecosystem Impacts on Permafrost Vulnerability. Arctic Data Center. doi:10.18739/A2F76677W.',
               citation_short = 'Natali et al. 2014',
               site_description = 'none',
               notes = 'none')]

# specify locale
harv.dt[site_id %in% 1:5 | site_id == 17 | site_id %in% 20:25, locale := 'Interior Alaska']
harv.dt[site_id %in% 6:8, locale := 'Brooks Range']
harv.dt[site_id %in% 9:16, locale := 'North Slope']
harv.dt[site_id %in% 18:19, locale := 'Seward Peninsula']

# create site and plot codes
harv.dt[, site_code := paste(country, gsub(' ','',locale), site_id, sep='.')]
harv.dt[, plot_code := paste(country, gsub(' ','',locale), site_id, plot_id, sep='.')]

sort(unique(harv.dt$site_code))

# add latitude and longitude
harv_plot.dt <- coord.dt[, X := NULL]
harv_plot.dt <- harv_plot.dt[Location != 20] #harvest plots are located at the 0-m end of the transect plots
harv_plot.dt <- harv_plot.dt[, Location := NULL]

# change column names 
setnames(harv_plot.dt, c('Site','Transect','Lat','Long'), c('site_id','plot_id','latitude','longitude'))

# combine understory pfts with plot location data
harv.dt <- harv_plot.dt[harv.dt, on = c("site_id", "plot_id")]
dim(harv.dt)

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6) * -1] # put in western hemisphere

# add year, month, and day
site_info.dt <- site.dt[, c('X', 'Site.name') := NULL]

site_info.dt <- separate(site_info.dt, "Sample.Date", c("month", "day", "year"), sep = "/") %>% data.table()
site_info.dt[, year := gsub(" ", "", paste('20',year, sep = ""))]

# change column names
setnames(site_info.dt, c('Site.Number','Description'), c('site_id','vegetation_description'))

# combine understory pft with site info 
harv.dt <- site_info.dt[harv.dt, on = 'site_id']
dim(harv.dt)


# STANDARDIZE UNDERSTORY DATA =================================================================================
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]

# add note to sites with O-DS (allometric measurements)-- this is needed here for the pft summaries
harv.dt <- harv.dt[pft == 'o-ds' & biomass_density_gm2 > 0, notes := 'Overstory shrubs surveyed']

# change pft names
harv.dt[pft == 'ds', pft := 'shrub']
harv.dt[pft == 'es', pft := 'shrub'] 
harv.dt[pft == 'o-ds', pft := 'shrub']
harv.dt[pft == 'lich', pft := 'lichen']
harv.dt[pft == 'moss', pft := 'bryophyte']
check_pfts(harv.dt)

# compute biomass density for each PFT  
value.cols <- paste(c('biomass_density_gm2','vegetation_description','notes'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                  vegetation_description = unique(na.omit(vegetation_description)),
                  notes = concat_rows(notes)),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

pft.dt[grep('Overstory shrubs', pft.dt$notes), ":="(notes = 'Overstory shrubs surveyed. Allometric model from Berner et al. 2015.', method = 'harvest + survey')] 
pft.dt[grep('Overstory shrubs', pft.dt$notes, invert = T), ":="(notes = 'none', method = 'harvest')] 

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand missing pfts
dim(pft.dt)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# STANDARDIZE TREE DATASET ====================================================================================
# remove dead trees
tree.dt <- species_tr.dt[Status != 'D']
tree.dt <- tree.dt[, c('X', 'BD', 'DBH', 'Status') := NULL]

# change column names
setnames(tree.dt, c('Site','Transect','Area','Biomass','Species'), c('site_id','plot_id','plot_area_m2','biomass_dry_weight_g','vegetation_description'))

# compute total AGB for all trees in a plot
pft_tree.dt <- tree.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g, na.rm = T)), by = c('site_id','plot_id','plot_area_m2')]

dim(tree.dt)
dim(pft_tree.dt)

# compute biomass density 
pft_tree.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# remove NA values 
pft_tree.dt <- pft_tree.dt[!is.na(pft_tree.dt$biomass_density_gm2),]

pft_tree.dt[, pft := 'tree']


# COMBINING PFT DATASETS ======================================================================================
dim(pft.dt)
xx <- pft_tree.dt[pft.dt, on = c('site_id','plot_id')]
pft.dt
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), plot_area_m2 := i.plot_area_m2]
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), biomass_dry_weight_g := i.biomass_dry_weight_g]
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), biomass_density_gm2 := i.biomass_density_gm2]

# set biomass to zero on plots where there were no live trees
pft.dt[pft == 'tree' & is.na(biomass_density_gm2), ':='(biomass_density_gm2 = 0, biomass_dry_weight_g = 0)]

pft.dt <- fill_missing_metadata(pft.dt, 'vegetation_description','plot_code')

# set sample plot area to 40 m2 (most common size) on plots w/o trees
pft.dt[pft == 'tree' & plot_area_m2 == 0.25, plot_area_m2 := 40]

# change method to survey for trees 
pft.dt[pft == 'tree', method := 'survey']

# specify allometric models
pft.dt[pft == 'tree' & biomass_density_gm2 > 0, notes := 'Allometric models from Alexander et al. 2012']

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]
pft.dt[is.na(vegetation_description), vegetation_description := 'none']
pft.dt[is.na(notes), notes := 'none']


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
unique(as.numeric(pft.dt$site_id))
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

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_07_natali_usa_alaska_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================