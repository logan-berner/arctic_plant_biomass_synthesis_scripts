# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Skip Walker, UAF
# Script Author: Logan Berner
# Script Date: 2023-03-20
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
require(viridis)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
colnames(database.tmplt.dt)
harv.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_18_walker_northamerica_arctic/NA-Artic_vegetation2.tab', skip = 37)
meta1.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_18_walker_northamerica_arctic/1-NAATreleves.Patrick.xls', sheet = 7))
meta2.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_18_walker_northamerica_arctic/1-NAATreleves.Patrick.xls', sheet = 8))
descrptions.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_18_walker_northamerica_arctic/walker_etal_2012_erl_table_1_metadata.csv')
  
# STANDARDIZE SITE DATA ======================================================================================

# subset columns to keep
colnames(harv.dt) <- tolower(colnames(harv.dt))
col.names <- colnames(harv.dt); col.names
keep.cols <- col.names[c(1,4:6,8:10,12,13,15:18)]; keep.cols
harv.dt <- harv.dt[, keep.cols, with = F]

# adjust column names
colnames(harv.dt) <- gsub(pattern = 'veg biom [g/m**2] ', replacement = '', x = colnames(harv.dt), fixed = T)
colnames(harv.dt) <- gsub(pattern = '(', replacement = '', x = colnames(harv.dt), fixed = T)
colnames(harv.dt) <- gsub(pattern = ')', replacement = '', x = colnames(harv.dt), fixed = T)
setnames(harv.dt, c('event','sample label sagwon: sa = acidic tundra, s...'), c('site_id','plot_id'))

# combine meta data and harvest data 
meta.dt <- rbind(meta1.dt[, c(2:4,9:11,13)], meta2.dt[, c(2:4,9:11,13)])
names(meta.dt) <- c('plot_id','latitude','longitude','year','month','day','community')

harv.dt <- meta.dt[harv.dt, on = 'plot_id']

# melt dataset into long format 
harv.dt <- melt(harv.dt, id.vars = 1:8, variable.name = 'pft', value.name = 'biomass_density_gm2')

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds18',
               contributor = 'Walker S, Epstein H',
               plot_area_m2 = 0.1,
               coord_type = 'plot',
               method = 'harvest',
               citation = 'Walker et al. 2011. Plant species, biomass and environmental characteristics of releves along the North America Arctic bioclimate gradient. PANGAEA, https://doi.org/10.1594/PANGAEA.837761.',
               citation_short = 'Walker et al. 2011')]

# adjust site names
unique(harv.dt$site_id)
harv.dt[, site_id := gsub('_',' ', site_id, fixed = T)]
harv.dt[, site_id := gsub('2','', site_id, fixed = T)]
unique(harv.dt$site_id)

# add descrptions
harv.dt <- descrptions.dt[harv.dt, on = 'site_id']

# specify country and locale of each site
unique(harv.dt$site_id)
harv.dt[, country := 'USA']
harv.dt[, locale  := 'North Slope']

harv.dt[site_id == 'Green Cabin' | site_id == 'Isachsen' | site_id == 'Mould Bay', country := 'Canada']

harv.dt[site_id == 'Green Cabin', locale := 'Banks Island']
harv.dt[site_id == 'Mould Bay', locale := 'Prince Patrick Island']
harv.dt[site_id == 'Isachsen', locale := 'Ellef Ringnes Island']

# subdivide sites by plant community
harv.dt[, community := substr(community,1,3)] # drop a/b distinction from communities
harv.dt[, site_id := paste(site_id, community, sep = '-')]
harv.dt[, community := NULL]
sort(unique(harv.dt$site_id))

# adjust plot_id
sort(unique(harv.dt$plot_id))
harv.dt[, plot_id := substr(plot_id, 5,7)]

# create site and plot codes
harv.dt[, site_code := paste(country, gsub(' ', '', locale), gsub(' ', '', site_id), sep='.')]
harv.dt[, plot_code := paste(country, gsub(' ', '', locale), gsub(' ', '', site_id), plot_id, sep='.')]
sort(unique(harv.dt$site_code))


# STANDARDIZE UNDERSTORY DATA =================================================================================
unique(harv.dt$pft)

# change pft names
harv.dt[pft == 'deciduous shrub stems', pft := 'shrub']
harv.dt[pft == 'deciduous shrub life leaves', pft := 'shrub']
harv.dt[pft == 'deciduous shrub flower fruit', pft := 'shrub']
harv.dt[pft == 'evergreen shrub stems', pft := 'shrub']
harv.dt[pft == 'evergreen shrub life leaves', pft := 'shrub']
harv.dt[pft == 'evergreen shrub flower fruit', pft := 'shrub']
harv.dt[pft == 'graminoids life', pft := 'herb']
harv.dt[pft == 'forbs', pft := 'herb']
harv.dt[pft == 'horsetail', pft := 'herb']
harv.dt[pft == 'mosses', pft := 'bryophyte']
harv.dt[, pft := as.character(pft)]
check_pfts(harv.dt)

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('vegetation_description','biomass_density_gm2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                  vegetation_description = unique(na.omit(vegetation_description))),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# drop the one site that is missing all biomass data...  
pft.dt <- pft.dt[plot_id != '048']

# expand data table so every pft occurs in every plot and then specify whether
# the pft data are missing because the pft was not present, or it wasn't harvested
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, 
                           biomass_density_gm2 = 0,
                           method = 'survey')]

pft.dt[is.na(notes), notes := 'none']

pft.dt <- fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
col.key <- data.table(site_id = unique(pft.dt$site_id),
                      col = sample(turbo(length(unique(pft.dt$site_id))))) # randomize cols

pft.dt[, site_col := col.key$col[match(pft.dt$site_id, col.key$site_id)]]

pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('site_id','plot_id','site_col')]
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircles(label = ~as.character(site_id), color = ~as.character(site_col)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

pft.dt <- pft.dt[, site_col := NULL]

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_18_walker_northamerican_arctic_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================