# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Donie Bret-Harte (UAF) and Micheele Mack (NAU)
# Script Author: Logan Berner
# Script Date: 2023-09-11
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
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.unburn.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_30to32_bretharte_usa_anaktuvukriver/2011ARF_AbvgrdBiomassCN_control.xlsx',sheet = 2))
harv.mod.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_30to32_bretharte_usa_anaktuvukriver/2011ARF_AbvgrdBiomassCN_mod.xlsx',sheet = 2))
harv.sev.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_30to32_bretharte_usa_anaktuvukriver/2011ARF_AbvgrdBiomassCN_severe.xlsx',sheet = 2))

# ADD METADATA ======================================================================================

# adjust column names
names(harv.unburn.dt) <- tolower(names(harv.unburn.dt))
names(harv.mod.dt) <- tolower(names(harv.mod.dt))
names(harv.sev.dt) <- tolower(names(harv.sev.dt))

setnames(harv.unburn.dt, 'biomass categroy', 'biomass category') # fix typo
setnames(harv.mod.dt, 'biomass categroy', 'biomass category') # fix typo

# drop unneeded columns
harv.unburn.dt <- harv.unburn.dt[,1:26]
harv.mod.dt <- harv.mod.dt[,1:26]
harv.sev.dt <- harv.sev.dt[,1:26]

# melt long
harv.unburn.dt <- melt.data.table(harv.unburn.dt, id.vars = 1:7, variable.name = 'plot_id', value.name = 'biomass_density_gm2')
harv.mod.dt <- melt.data.table(harv.mod.dt, id.vars = 1:7, variable.name = 'plot_id', value.name = 'biomass_density_gm2')
harv.sev.dt <- melt.data.table(harv.sev.dt, id.vars = 1:7, variable.name = 'plot_id', value.name = 'biomass_density_gm2')

# combine datasets
harv.dt <- rbind(harv.unburn.dt, harv.mod.dt, harv.sev.dt)

# replace spaces with underscores in column names
names(harv.dt) <- gsub(pattern = ' ', replacement = '_', names(harv.dt))
  
# add metadata
harv.dt[, ":="(contributor = 'Bret-Harte M, Mack M, Shaver G, Laundre J',
               locale = 'Anaktuvuk River',
               country = 'USA',
               coord_type = 'site',
               method = 'harvest',
               plot_area_m2 = 0.04,
               notes = 'none')]

# add data set IDS and citations
harv.dt[fire_disturbance == 'Unburned', ':='(dataset_id = 'ds30', citation_short = 'Bret-Harte et al. 2020a')]
harv.dt[fire_disturbance == 'Moderate Burn', ':='(dataset_id = 'ds31', citation_short = 'Bret-Harte et al. 2020b')]
harv.dt[fire_disturbance == 'Severe Burn', ':='(dataset_id = 'ds32', citation_short = 'Bret-Harte et al. 2020c')]

harv.dt[fire_disturbance == 'Unburned', citation := 'Bret-Harte et al. 2020. Above ground plant and below ground stem biomass of samples from the moderately burned site at Anaktuvuk River fire, Alaska. Environmental Data Initiative. doi:10.6073/pasta/6646ac57a7397b9c8d1a2dc3c95a566c.']
harv.dt[fire_disturbance == 'Moderate Burn', citation := 'Bret-Harte et al. 2020. Above ground plant and below ground stem biomass of samples from the severely burned site of the Anaktuvuk River fire, Alaska. Environmental Data Initiative. doi:10.6073/pasta/7f609c982e2e6880f63bab4c3bd5af8d.']
harv.dt[fire_disturbance == 'Severe Burn', citation := 'Bret-Harte et al. 2020. Above ground plant and below ground stem biomass of samples from the unburned control site near the Anaktuvuk River fire, Alaska. Environmental Data Initiative. doi:10.6073/pasta/18fcdcaf43451b70610d55da6475b397']

# site descriptions
harv.dt[fire_disturbance == 'Unburned', site_description := 'Upland moist acidic mixed shrub tussock tundra']
harv.dt[fire_disturbance == 'Moderate Burn', site_description := 'Upland moist acidic mixed shrub tussock tundra that burned at moderate severity in 2007']
harv.dt[fire_disturbance == 'Severe Burn', site_description := 'Upland moist acidic mixed shrub tussock tundra that burned at high severity in 2007']
harv.dt <- harv.dt[, fire_disturbance := NULL]

# parse dates
harv.dt <- harv.dt %>% separate(col = date, into = c('year','month','day'), sep = '-', convert = T) %>% as.data.table()

# add coordinates based on metadata
harv.dt[, site_id := as.numeric(substr(plot_id, 0, 3))]
harv.dt[, plot_id := as.numeric(substr(plot_id, 6, 7))]
sort(unique(harv.dt$site_id))

harv.dt[site_id == 101, ':='(latitude = 68.99539, longitude = -150.28278)]
harv.dt[site_id == 103, ':='(latitude = 68.95383, longitude = -150.20697)]
harv.dt[site_id == 104, ':='(latitude = 68.95110, longitude = -150.20966)]
harv.dt[site_id == 108, ':='(latitude = 68.95235, longitude = -150.20770)]
harv.dt[site_id == 109, ':='(latitude = 68.93334, longitude = -150.27289)]
harv.dt[site_id == 114, ':='(latitude = 68.9970, longitude = -150.3070)] # from Bret-Harte et al. 2013 tabl 1, but fixed lat from 69 to 68

# create site and plot codes
harv.dt[, site := NULL]

harv.dt[, site_code := paste(country, gsub('\\ ','', locale), site_id, sep='.')]
harv.dt[, plot_code := paste(country, gsub('\\ ','', locale), site_id, plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)


# sort 
setorder(harv.dt, site_id, plot_id)


# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# drop dead vegetation and below ground vegetation
unique(harv.dt$biomass_category)
harv.dt <- harv.dt[biomass_category != 'litter']
harv.dt <- harv.dt[biomass_category != 'below']

# standardize plant functional types 
setnames(harv.dt, 'growth_form','pft')
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)

harv.dt[pft == 'lichens', pft := 'lichen']
harv.dt[pft %in% c('graminoids','forbs','dicot'), pft := 'herb']
harv.dt[pft %in% c('mosses','liverwort'), pft := 'bryophyte']
harv.dt[pft %in% c('deciduous','evergreen'), pft := 'shrub']

unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each species (sum across tissue types and biomass category)
harv.dt <- harv.dt[, tissue := NULL]
harv.dt <- harv.dt[, biomass_category := NULL]

value.cols <- 'biomass_density_gm2'
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

harv.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)),
                   by = grouping.cols]

dim(harv.dt)

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('biomass_density_gm2','species'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                      vegetation_description = concat_rows(species)),
                  by = grouping.cols]

dim(harv.dt)
dim(pft.dt)

# calculate biomass try weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set tree biomass to zero (treeless tundra)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey')]

pft.dt <- fill_missing_metadata(pft.dt, fill.cols = c('notes','plot_area_m2','site_description','vegetation_description'), by = 'plot_code')

pft.dt
dim(pft.dt)

# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('site_code')]
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(site_code)) %>% 
  addLabelOnlyMarkers(label = ~as.character(site_code),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# WRITE OUTPUT ===============================================================================================
fwrite(pft.dt[dataset_id == 'ds30'], 'data/synthesis_database/3_contributed_data_processed/dataset_30_bretharte_usa_anaktuvukriver_standardized.csv')
fwrite(pft.dt[dataset_id == 'ds31'], 'data/synthesis_database/3_contributed_data_processed/dataset_31_bretharte_usa_anaktuvukriver_standardized.csv')
fwrite(pft.dt[dataset_id == 'ds32'], 'data/synthesis_database/3_contributed_data_processed/dataset_32_bretharte_usa_anaktuvukriver_standardized.csv')

# END SCRIPT =================================================================================================