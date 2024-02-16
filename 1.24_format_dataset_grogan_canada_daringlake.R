# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Paul Grogan (Queens University)
# Script Author: Logan Berner
# Script Date: 2023-08-23
# Dataset Notes: 
# - mix of plot and site coordinates

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
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_24_vankoughnett_canada_daringlake/Logan Berner Copy of biomass stats Paul species combined data 2008.xlsx', sheet = 2))
coord.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_24_vankoughnett_canada_daringlake/Grogan lab plots full data set incl exclosures.xls', sheet = 1))
dates.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_24_vankoughnett_canada_daringlake/Sampledates.xlsx', sheet = 1))

# ADD METADATA ======================================================================================
harv.dt[, ":="(dataset_id = 'ds24',
               contributor = 'Grogan P, Vankoughnett M',
               country = 'Canada',
               locale = 'Daring Lake',
               citation = 'Vankoughnett M and Grogan P. 2016. Plant production and nitrogen accumulation above- and belowground in low and tall birch tundra communities: the influence of snow and litter. Plant and Soil 408:195-210.',
               citation_short = 'Vankoughnett and Grogan 2016',
               notes = 'none',
               method = 'harvest')]

# adjust site_id and plot_id
setnames(harv.dt, c('Site','Plot'), c('site_id','plot_id'))

# add harvest dates for each plot
harv.dt[, dates := dates.dt$`harvest date`[match(harv.dt$plot_id, dates.dt$Code)]]
harv.dt <- separate(data = harv.dt, col = 'dates', into = c('year','month','day')) %>% data.table()

# add coordinates and specify coordinate type
harv.dt[, latitude := coord.dt$Lat[match(harv.dt$plot_id, coord.dt$Name)]]
harv.dt[, longitude := coord.dt$Long[match(harv.dt$plot_id, coord.dt$Name)]]
harv.dt[site_id == "BH", coord_type := 'plot']

harv.dt[site_id == "TB", latitude := coord.dt[Name == "Tall birch"]$Lat]
harv.dt[site_id == "TB", longitude := coord.dt[Name == "Tall birch"]$Long]
harv.dt[site_id == "TB", coord_type := 'site']

# adjust site names and plot ids per conversation with Paul Grogan
harv.dt[site_id == 'BH', site_id := 'LowBirch'] # low birch
harv.dt[site_id == 'TB', site_id := 'MediumBirch'] # medium birch

harv.dt[, plot_id := gsub('C','', plot_id)]
harv.dt[, plot_id := gsub('TB','', plot_id)]

# add site and vegetation descriptions
harv.dt[, site_description := 'Gently sloping valley underlain by continuous permafrost.']
harv.dt[site_id == 'LowBirch', vegetation_description := 'Low birch hummock tundra dominated by evergreen shrubs, mountain cranberry (Vaccinium vitis-idaea L.), labrador tea (Rhododendron subarcticum), bog rosemary (Andromeda polifolia L.), mosses and lichens (Cetraria spp. and Cladina spp.).']
harv.dt[site_id == 'MediumBirch', vegetation_description := "Medium birch tundra dominated by Betula glandulosa (about 80 cm high) with species compositions similar to birch hummock tundra, but with occasional tall willow shrubs (Salix)."]

# create site and plot codes
harv.dt[, site_code := paste(country, 'DaringLake', site_id, sep='.')]
harv.dt[, plot_code := paste(country, 'DaringLake', site_id, plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)

# sort 
setorder(harv.dt, site_id, plot_id)


# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# standardize plant functional types 
setnames(harv.dt, 'Species','pft')
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)

harv.dt <- harv.dt[pft != 'litter']

harv.dt[pft == 'moss', pft := 'bryophyte']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft %in% c('birch','rhododendron subarcticum (formerly ledum)','rosemary','vacc. ulig','vacc.vitis','salix'), pft := 'shrub']

unique(harv.dt$pft)
check_pfts(harv.dt)

# specify plot areas, which differed between the two sites
harv.dt[site_id == 'LowBirch', plot_area_m2 := 0.4 * 0.4]
harv.dt[site_id == 'MediumBirch', plot_area_m2 := 0.8 * 0.8]

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('biomass_dry_weight_g','biomass_density_gm2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      biomass_density_gm2 = sum(biomass_density_gm2)),
                  by = eval(grouping.cols)]

# expand data table so every pft occurs in every plot
dim(pft.dt)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set tree biomass to zero on all plots (treeless tundra) and copy plot metadata
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]
pft.dt[pft == 'tree', method := 'survey']

fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')
fill_missing_metadata(pft.dt, fill.cols = 'site_description', by = 'plot_code')
pft.dt[is.na(notes), notes := 'none']

pft.dt
dim(pft.dt)


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


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_24_grogan_canada_daringlake_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================