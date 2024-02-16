# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: M. Arndal (University of Copenhagen) 
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-08-29, updated 2023-01-31
# Dataset Notes: check herb pfts

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(sf)
require(tidyr)
require(dplyr)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_14_arndal_greenland_zackenberg/Zackenberg biomass from 20060104 MarieArndal 20220803.xls', sheet = 1))
plot.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_14_arndal_greenland_zackenberg/SCHAPPE_2002_2004_Plot_UTM_positions.xlsx', skip = 5))

# STANDARDIZE BIOMASS DATA ===================================================================================

# remove unnecessary columns 
harv.dt <- harv.dt[, c('week', 'dayno', 'Cover', 'Wet weight') := NULL]

# add year, month, and day
harv.dt <- data.table(separate(harv.dt, 'Date', c('day', 'month', 'year'), sep = c(2,4)))
harv.dt <- harv.dt[, year := 2004]

# create unique site_id and plot_id codes
harv.dt[, site_id := paste0(vegtype, nr)]
harv.dt[, plot := toupper(plot)]
harv.dt <- harv.dt[, c('vegtype', 'nr') := NULL]

# change column names 
names(harv.dt)[names(harv.dt) == 'plot']  <- 'plot_id'
names(harv.dt)[names(harv.dt) == 'Dry  weight']  <- 'biomass_dry_weight_g'
names(harv.dt)[names(harv.dt) == 'DW g/m2']  <- 'biomass_density_gm2'
dim(harv.dt)

# remove rows with NA year and 0 biomass
harv.dt <- harv.dt[is.na(year) == FALSE | is.na(year) == TRUE & biomass_dry_weight_g > 0]
dim(harv.dt)

# combine site_id and plot_id into unique Name   
harv.dt[, Name := paste0(site_id, plot_id)]

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, method, site_description, notes, citation, and citation_short
harv.dt[, ':='(dataset_id = 'ds14',
               contributor = 'Arndal M, Michelsen A, Tamstorf M',
               country = 'Greenland',
               locale = 'Zackenberg',
               plot_area_m2 = 0.038025,
               coord_type = 'plot',
               method = 'harvest',
               site_description = 'flat valley about 30 km from the coast', 
               notes = 'none',
               citation = 'Arndal et al. 2009. Seasonal variation in gross ecosystem production, plant biomass, and carbon and nitrogen pools in five high arctic vegetation types. Arctic, Antarctic, and Alpine Research 41:164-173.', 
               citation_short = 'Arndal et al. 2009')]

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]


# STANDARDIZE PLOT DATA =======================================================================================

# change coordinate type
sf <- st_as_sf(plot.dt, coords = c('XUTM', 'YUTM'), crs = 'EPSG:32627')
transform <- st_transform(sf, crs = 'EPSG:4326')
plot_locations <- transform %>% mutate(
  longitude = st_coordinates(transform$geometry)[,1], 
  latitude = st_coordinates(transform$geometry)[,2])

plot.dt <- data.table(plot_locations)

# round coordinates to 6 decimals
plot.dt[, latitude := round(latitude, 6)]
plot.dt[, longitude := round(longitude, 6)]

# remove unnecessary columns
plot.dt <- plot.dt[, c('Date', 'geometry') := NULL]

# change column names
names(plot.dt)[names(plot.dt) == 'Type']  <- 'vegetation_description'

# recode "Kaer" as "Fen" based on examination of peer-reviewed paper
plot.dt[vegetation_description == 'Kaer', vegetation_description := 'Fen']

# combine harv.dt and plot.dt
harv.dt <- harv.dt %>%
  left_join(plot.dt, by='Name')
dim(harv.dt)

# STANDARDIZE PFT DATA =======================================================================================

# standardize pfts
harv.dt[, Category := tolower(Category)]
harv.dt[, species := tolower(species)]
unique(harv.dt$Category)
unique(harv.dt$species)
dim(harv.dt)

# remove litter pft groups 
harv.dt <- harv.dt[Category != 'litter liggende']
harv.dt <- harv.dt[Category != 'litter stående']
dim(harv.dt)

# remove cyanobacteria
harv.dt <- harv.dt[Category != 'nostoc']

# standardizing pft names
harv.dt[Category == 'cassiope', pft := 'shrub']
harv.dt[Category == 'dryas', pft := 'shrub']
harv.dt[Category == 'salix', pft := 'shrub']
harv.dt[Category == 'vaccinium', pft := 'shrub']

harv.dt[Category == 'polygonum', pft := 'herb'] 
harv.dt[Category == 'silene', pft := 'herb'] 
harv.dt[Category == 'cerastium', pft := 'herb'] 
harv.dt[Category == 'papaver', pft := 'herb'] 
harv.dt[Category == 'saxifraga', pft := 'herb'] 
harv.dt[Category == 'huperzia', pft := 'herb'] 
harv.dt[Category == 'pedicularis', pft := 'herb'] 
harv.dt[Category == 'taraxacum', pft := 'herb'] 
harv.dt[Category == 'ranunculus', pft := 'herb'] 
harv.dt[Category == 'urt', pft := 'herb'] 
harv.dt[Category == 'urt sp', pft := 'herb'] 

harv.dt[Category == 'kobresia', pft := 'herb']   # sedge
harv.dt[Category == 'alopecurus', pft := 'herb']   # grass
harv.dt[Category == 'arctagrostris', pft := 'herb']   # grass
harv.dt[Category == 'eriophorum', pft := 'herb']   # sedge
harv.dt[Category == 'juncus', pft := 'herb']   # rush
harv.dt[Category == 'carex', pft := 'herb']   # sedge
harv.dt[Category == 'dupontia', pft := 'herb']   # grass
harv.dt[Category == 'poa', pft := 'herb']   # grass
harv.dt[Category == 'poa sp', pft := 'herb']   # grass
harv.dt[Category == 'luzula', pft := 'herb']   # rush 
harv.dt[Category == 'carex grøn (cappilaris)', pft := 'herb']   # sedge
harv.dt[Category == 'hierochloë', pft := 'herb']   # grass
harv.dt[Category == 'equisetum', pft := 'herb']   # horsetail
harv.dt[Category == 'equisetum sp', pft := 'herb']   # horsetail
harv.dt[Category == 'græs', pft := 'herb']   # grass

harv.dt[Category == 'mos høst', pft := 'bryophyte']
harv.dt[Category == 'mos andet', pft := 'bryophyte']

harv.dt[Category == 'lav', pft := 'lichen']

unique(harv.dt$pft)
check_pfts(harv.dt)

# remove unnecessary columns
harv.dt <- harv.dt[, c('Category', 'species', 'fraction', 'Name', 'biomass_dry_weight_g') := NULL]

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

# fill in NA biomass values with 0
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]

# expand vegetation_description to all pfts in each plot
pft.dt[, vegetation_description := unique(na.omit(vegetation_description)),
       by = .(plot_code)]

# set tree method to survey
pft.dt[pft == 'tree', method := 'survey']

# expand notes to all pfts in each plot
pft.dt[, notes := unique(na.omit(notes)),
       by = .(plot_code)]

# remove erroneous row 
pft.dt <- pft.dt[plot_code != "Greenland.Zackenberg.G1.CO2NEW"]


# CHECK PFT SUMMARIES =========================================================================================

# total AGB for each pft across all sites
pft.dt[, .(biomass_dry_weight_g_sum = sum(biomass_dry_weight_g),
           biomass_density_gm2_avg = mean(biomass_density_gm2)), by = pft]


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# # make all character columns lowercase except contributor, country, locale, citation and citations short
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
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_14_arndal_greenland_zackenberg_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================