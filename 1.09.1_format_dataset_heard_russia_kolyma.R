# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Mike Loranty (Colgate University)
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-05-25, updated 2023-01-31
# Dataset Notes: 
# - Lichen and moss biomass data missing for all sites in 2013 due to "overestimation of C because of subsampling error"
# - Maybe check with Sue about whether this was only related to C concentration, or a problem with the biomass harvest
# - For now, set biomass density to zero or NA in each plot based on % cover 

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
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_09_heard_russia_kolyma/TS_2012_2013_Master.xlsx'
site.dt <- data.table(read_excel(in.file, sheet = 1))
tree1.dt <- data.table(read_excel(in.file, sheet = 8))
us1.dt <- data.table(read_excel(in.file, sheet = 12))
us2.dt <- data.table(read_excel(in.file, sheet = 13))
cover.dt <- data.table(read_excel(in.file, sheet = 15))
meta.dt <- data.table(read_excel(in.file, sheet = 16, range = "A9:F48"))


# STANDARDIZE SITE DATA ======================================================================================

# create new data table with only the relevant columns
harv.dt <- site.dt[, c('Site name', 'Description', 'Sample Date', 'Lat', 'Long')]

# add year, month, and day
harv.dt <- separate(harv.dt, "Sample Date", c("year", "month", "day"), sep = "-") %>% data.table()

# change column names
setnames(harv.dt, c('Site name','Description','Lat','Long'), c('site_id','site_description','latitude','longitude'))

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
harv.dt[, ":="(dataset_id = 'ds09',
               contributor = 'Natali S, Loranty M, Alexander H, Bunn A, Holmes R',
               country = 'Russia',
               locale = 'Kolyma',
               plot_area_m2 = 0.25,
               coord_type = 'site',
               method = 'harvest',
               citation = 'Heard et al. 2016. Northeast Siberia Plant and Soil Data: Plant Composition and Cover, Plant and Soil Carbon Pools, and Thaw Depth. Arctic Data Center. doi:10.5065/D6NG4NP0.',
               citation_short = 'Heard et al. 2016',
               notes = 'none')]

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# add vegetation type
setnames(meta.dt, c('Site ID','Ecosystem type'), c('site_id','vegetation_description'))
meta.dt <- meta.dt[, c('site_id','vegetation_description')]
harv.dt <- meta.dt[harv.dt, on = 'site_id']

# STANDARDIZE UNDERSTORY DATA =================================================================================

# remove dead plants - Status (live[1] or dead[0]) only in 2012 dataset
us1.dt <- us1.dt[Status != 0] 
us1.dt <- us1.dt[, Status := NULL] 

# make column types compatible 
us1.dt[, Mass := as.numeric(Mass)]
us2.dt[, Trans := as.character(Trans)]

# combine understory datasets 
us_combined.dt <- rbind(us1.dt, us2.dt)
setnames(us_combined.dt, 'Site','site_id')
dim(us1.dt)
dim(us2.dt)
dim(us_combined.dt)

us_combined.dt <- us_combined.dt[, Notes := NULL]

# fix PFB
us_combined.dt[site_id == 'PFB']
us_combined.dt <- us_combined.dt[(site_id == 'PFB' & (Trans == '1b' | Trans == '2b' | Trans == '3b')) == F]
us_combined.dt[site_id == 'PFB', Trans := substr(Trans, 0, 1)]

# join understory and site data
harv.dt <- harv.dt[us_combined.dt, on = 'site_id']

# rename columns
setnames(harv.dt, c('Trans','Func','Mass'), c('plot_id','pft','biomass_density_gm2'))

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

setorder(harv.dt, year, site_id, plot_id)
dim(us_combined.dt)
dim(harv.dt)

# standardize understory pft
unique(harv.dt$pft)
harv.dt[, pft := tolower(pft)]

# remove gram dead, gram/ brown, gram brown, and gram/litter
harv.dt <- harv.dt[pft != 'gram dead'] 
harv.dt <- harv.dt[pft != 'gram/ brown']
harv.dt <- harv.dt[pft != 'gram brown']
harv.dt <- harv.dt[pft != 'gram/litter']
unique(harv.dt$pft)
dim(harv.dt)

# standardizing pft names
harv.dt[pft == 'decid', pft := 'shrub']
harv.dt[pft == 'evg', pft := 'shrub'] 
harv.dt[pft == 'gram', pft := 'herb']
harv.dt[pft == 'moss', pft := 'bryophyte']
check_pfts(harv.dt)

# compute biomass density for each PFT  
value.cols <- 'biomass_density_gm2'
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2)),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# change method to harvest/ allometry
pft.dt[pft == 'shrub', method := 'harvest + survey']

# compute biomass dry weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]

pft.dt[, notes := 'none']


# shrub data are missing for sites LBR and HBR per a note in the metadata 
pft.dt[pft == 'shrub' & (site_id == 'LBR' | site_id == 'HBR'), ':='(biomass_dry_weight_g = NA, 
                                                                    biomass_density_gm2 = NA,
                                                                    method = 'unmeasured')]

# lichen and bryophyte biomass data missing for all sites in 2013 but have cover data
# set biomass to zero where cover is zero and to NA where cover is >0%
cover.dt[, Quadrat := tolower(Quadrat)]
cover.dt <- cover.dt[Meter == 0 & Quadrat == 'd']
cover.dt <- cover.dt[, c(1,2,7,8)] # grab cols of interest
setnames(cover.dt, c('Site','Trans','% moss','% lichen'), c('site_id','plot_id','moss_cover','lichen_cover'))
cover.dt[, ':='(moss_cover = round(as.numeric(moss_cover)), 
                lichen_cover = round(as.numeric(lichen_cover)))]

pft.dt <- cover.dt[pft.dt, on = c('site_id','plot_id')]
table(pft.dt$year)

pft.dt[year == 2013 & pft == 'bryophyte' & moss_cover == 0, ':='(biomass_dry_weight_g = 0, 
                                                                 biomass_density_gm2 = 0, 
                                                                 method = 'survey',
                                                                 notes = paste0('Bryophyte cover ', moss_cover, '%.'))]

pft.dt[year == 2013 & pft == 'bryophyte' & moss_cover != 0, ':='(biomass_dry_weight_g = NA, 
                                                                 biomass_density_gm2 = NA, 
                                                                 method = 'unmeasured',
                                                                 notes = paste0('Bryophyte cover ', moss_cover, '%.'))]

pft.dt[year == 2013 & pft == 'lichen' & lichen_cover == 0, ':='(biomass_dry_weight_g = 0, 
                                                                biomass_density_gm2 = 0, 
                                                                method = 'survey',
                                                                notes = paste0('Lichen cover ', lichen_cover, '%.'))]

pft.dt[year == 2013 & pft == 'lichen' & lichen_cover != 0, ':='(biomass_dry_weight_g = NA, 
                                                                biomass_density_gm2 = NA,
                                                                method = 'unmeasured',
                                                                notes = paste0('Lichen cover ', lichen_cover, '%.'))]

pft.dt <- pft.dt[, c('moss_cover','lichen_cover') := NULL]

pft.dt[method == 'unmeasured', plot_area_m2 := NA]

# STANDARDIZE TREE DATASET ====================================================================================
tree.dt <- tree1.dt[, c('Site', 'Trans', 'Plot_area', 'Mass')]

# rename columns
setnames(tree.dt, c('Site','Trans','Plot_area','Mass'), c('site_id','plot_id','plot_area_m2','biomass_dry_weight_g'))

# make column types compatible
tree.dt[, biomass_dry_weight_g := as.numeric(biomass_dry_weight_g)]
tree.dt[, plot_area_m2 := as.numeric(plot_area_m2)]

# add plant functional type
tree.dt[, pft := 'tree']

# sum AGB for all trees in each plot
value.cols <- 'biomass_dry_weight_g'
grouping.cols <- grep(value.cols, colnames(tree.dt), invert = T, value = T)

pft_tree.dt <- tree.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g)),
                  by = eval(grouping.cols)]
dim(tree.dt)
dim(pft_tree.dt)

# compute biomass density 
pft_tree.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]


# COMBINING PFT DATASETS ======================================================================================
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), plot_area_m2 := i.plot_area_m2]
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), biomass_dry_weight_g := i.biomass_dry_weight_g]
pft.dt <- pft.dt[pft_tree.dt, on = c('site_id', 'plot_id', 'pft'), biomass_density_gm2 := i.biomass_density_gm2]

# set tree biomass to zero for sites w/o trees 
pft.dt[pft == 'tree' & is.na(biomass_density_gm2) == T, ':='(biomass_density_gm2 = 0, biomass_dry_weight_g = 0)]

# change method to allometry for trees 
pft.dt[pft == 'tree', method := 'survey']

# change plot_area to 80 for sites w/o trees
pft.dt[pft == 'tree' & biomass_density_gm2 == 0, plot_area_m2 := 80]

# fill in missing 
pft.dt <- fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')
dim(pft.dt)

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

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# recheck PFTs
check_pfts(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_09_heard_russia_kolyma_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================