# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Mike Loranty (Colgate University)
# Script Author: Logan Berner
# Script Date: 2022-05-16
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
require(leaflet)
require(sf)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_08_loranty_russia_kolyma/tundra_veg_2014.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 'VEG_BIOMASS'))
tree.dt <- data.table(read_excel(in.file, sheet = 'DBH'))
site.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_08_loranty_russia_kolyma/loranty_russia_kolyma_treeline_site_coords.xlsx'))


# STANDARDIZE SITE DATA ======================================================================================
site.dt[, ":="(dataset_id = 'ds08',
               contributor = 'Loranty M, Natali S',
               country = 'Russia',
               locale = 'Kolyma',
               coord_type = 'site',
               citation = 'Loranty M and Natali N. Unpublished.',
               citation_short = 'Loranty and Natali Unpub.',
               notes = 'none')]

# round coordinates to 6 decimals
site.dt[, latitude := round(latitude, 6)]
site.dt[, longitude := round(longitude, 6)]

# specifcy coornidate type (site or plot) 
site.dt[, coord_type := as.character(coord_type)]
site.dt[, coord_type := 'plot']


# STANDARDIZE HARVEST DATA ================================================================
# subset columns
keep.cols <- c('Site','Trans','Plot','VegSample','Corr_Wt_g')
harv.dt <- harv.dt[, ..keep.cols]

# rename columns
colnames(harv.dt) <- c('site_id','trans','plot_id','pft_plus','biomass_dry_weight_g')

# create plot_id using transect and plot (i.e., embed transect info in plot_id)
harv.dt[, plot_id := paste(trans, plot_id, sep = '-')]
harv.dt <- harv.dt[, trans := NULL]

# add plot size (clarified with Mike)
harv.dt[, plot_area_m2 := 0.5 * 0.5]

# two moss samples and one lichen sample had biomass dry weight provided as "bad". Set these to NA 
harv.dt[biomass_dry_weight_g == 'bad', biomass_dry_weight_g := NA]

# specify that biomass is numeric
harv.dt[, biomass_dry_weight_g := as.numeric(biomass_dry_weight_g)]

# compute deciduous shrub [DS] biomass = DS + Leaf (LV) + Specific Leaf Area (SLA) sample 
harv.dt[pft_plus == 'DS' | pft_plus == 'LV' | pft_plus == 'SLA', pft := 'deciduous shrub']
harv.dt[is.na(pft), pft := pft_plus]
harv.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g)), by = c('site_id','plot_id','plot_area_m2','pft')]

# standardize plant functional types
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]
harv.dt[pft == 'deciduous shrub', pft := 'shrub']
harv.dt[pft == 'es', pft := 'shrub'] # evergreen shrub
harv.dt[pft == 'f', pft := 'herb']
harv.dt[pft == 'm', pft := 'bryophyte']
harv.dt[pft == 'l', pft := 'lichen']
harv.dt[pft == 'g', pft := 'herb']
check_pfts(harv.dt)

# compute total AGB for each PFT  
value.cols <- 'biomass_dry_weight_g'
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g)),
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# site T1 has trees that were measured on three belt transect (20 m x 4 m)
# compute live tree AGB using allometric models from Alexander et al. 2012 Ecosystems
colnames(tree.dt) <- c('site_id','plot_id','dbh','note')
tree.dt[is.na(note), note := 'live']
tree.dt <- tree.dt[note == 'live']
tree.dt[, plot_id := paste(plot_id, 0, sep='-')] # match plot_id to main harvest data.table
tree.dt[, biomass_dry_weight_g := 179.20*dbh^2.01] # DBH in cm
tree.dt <- tree.dt[, ':='(note = NULL, dbh = NULL)]
tree.plot.dt <- tree.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                            plot_area_m2 = 20*4,
                            pft = 'tree'), by = c('site_id','plot_id')]

pft.dt <- rbind(pft.dt, tree.plot.dt) # add tree biomass data to PFT data.table


# compute biomass density and set NA values where needed
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]
pft.dt[is.na(biomass_dry_weight_g), biomass_density_gm2 := NA]
pft.dt


# COMBINE PFT BIOMASS AND SITE DATA, THEN FINISH STANDARDIZING ============================================================================
pft.dt <- site.dt[pft.dt, on = 'site_id']

# create site and plot codes
pft.dt[, site_code := paste(country, locale, site_id, sep='.')]
pft.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]
pft.dt[is.na(vegetation_description), vegetation_description := 'none']
pft.dt

# add methods used to determine biomass
pft.dt[pft != 'tree', method := 'harvest']
pft.dt[pft == 'tree', method := 'survey']
pft.dt[pft == 'tree', notes := "Allometric model from Alexander et al. 2012"]
pft.dt[pft == 'tree', vegetation_description := 'Cajander larch']

pft.dt[is.na(notes), notes := 'none']

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

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_08_loranty_russia_kolyma_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================