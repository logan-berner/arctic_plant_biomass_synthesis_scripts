# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Anna Virkkala (WCRC)
# Script Author: Logan Berner
# Script Date: 2022-05, 2023-03-10
# Dataset Notes: 
# - data collected over three years with differences between years
# - 2016: plot_id (e.g., 178HA, 178HB) should be parsed into site_id (e.g., 178) and plot_id (e.g., HA)
# - 2017/2018: Plots visited once (?) both years and provided with same plot_id. Make the 
#   plot_id the site_id and attribute plot ID as 1 or 2 depending on year
   
# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(tidyr)
require(sf)
require(tidyr)
require(dplyr)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx'))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_04_virkkala_finland_kilpisjarvi/Virkkala_Finland_Kilpisjarvi_biomass_data_20220103_v2.xlsx'
harv.2016.dt <- data.table(read_excel(in.file, sheet = 2))
harv.2017.dt <- data.table(read_excel(in.file, sheet = 3))
harv.2018.dt <- data.table(read_excel(in.file, sheet = 4))
harv.dt <- rbind(harv.2016.dt, harv.2017.dt, harv.2018.dt)
cover.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_04_virkkala_finland_kilpisjarvi/env_2016_cover.csv')

# STANDARDIZE DATASET ======================================================================================

# add dataset ID
harv.dt[, dataset_id := 'ds04']

# add contributors
harv.dt[, contributor := 'Virkkala A-M, Happonen K, Luoto M']

# set locale to Kilpisjärvi instead of the region Enontekiö
harv.dt[, locale := 'Kilpisjarvi']

# parse site_id and plot_id
harv.dt[year == 2016, site_id := substr(plot_id, 1, 3)]
harv.dt[year == 2016, plot_id := substr(plot_id, 4, 6)]

harv.dt[year == 2017 | year == 2018, site_id := plot_id]
harv.dt[year == 2017, plot_id := 'A']
harv.dt[year == 2018, plot_id := 'B']

#  the harvest day is missing for a few plots, set these to blank
harv.dt[day == 'NA', day := '']

# create site and plot codes
harv.dt[, site_code := paste(country, locale, site_id, sep='.')]
harv.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# specify coordinate type (site or plot) 
harv.dt[, coord_type := as.character(coord_type)]
harv.dt[, coord_type := 'plot']

# round coordinates to 6 decimals
harv.dt[, latitude := round(latitude, 6)]
harv.dt[, longitude := round(longitude, 6)]

# set method to harvest
harv.dt[, method := 'harvest']

# round plot area (m2) to five decimals
harv.dt[, plot_area_m2 := round(plot_area_m2,5)]

# shorten the site description
harv.dt[, site_description := 'Mountain tundra with thin soils and small scale topographic heterogeneity from drier ridges and plateaus dominated by dwarf shrubs to wet streams and meadows']

# set notes
harv.dt[, notes := 'Each field site includes sample plots harvested during multiple years and through the growing seasons.']

# a pft with no biomass on a plot was coded as NA, so change to zero
harv.dt[, biomass_dry_weight_g := as.numeric(biomass_dry_weight_g)]
harv.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]

# standardize plant functional types
# in 2016, graminoids and forbs were pooled into a 'herbaceous' category, but otherwise partitioned
harv.dt[, pft := tolower(pft)]
unique(harv.dt$pft)
harv.dt[pft == 'deciduous' | pft == 'evergreen', pft := 'shrub']
harv.dt[pft == 'woody', pft := 'shrub']
harv.dt[pft == 'herbaceous', pft := 'herb']
harv.dt[pft == 'graminoid', pft := 'herb']
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'fern', pft := 'herb']  # horsetail
harv.dt[pft == 'moss', pft := 'bryophyte']
harv.dt <- harv.dt[pft != 'litter'] # drop litter
harv.dt <- harv.dt[pft != 'negromass'] # drop 'negromass' 

# check PFTs 
check_pfts(harv.dt)

# remove biomass density column because it messes up calculation below
harv.dt <- harv.dt[, biomass_density_gm2 := NULL]

# due to reclassifying PFTs above, some PFTs occur multiple times on a plot, so compute total AGB for each PFT
value.cols <- paste(c('vegetation_description','biomass_dry_weight_g'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      vegetation_description = first(vegetation_description)), 
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# compute biomass density 
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# expand data table so every pft occurs in every plot, even if not present (zero) or not harvested (NA)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# document which biomass pools present but not harvested
pft.dt[year == 2016 & pft == 'lichen', ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured')]
pft.dt[(year == 2017 | year == 2018) & (pft == 'bryophyte' | pft == 'lichen'), 
       ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured')]

# set missing lichen and bryophyte biomass to zero when cover was zero
cover.dt <- cover.dt[, c('plot','lichen_c','moss_c')]
setnames(cover.dt, c('plot','lichen_c','moss_c'), c('plot_id','lichen_cov','moss_cov'))
cover.dt[is.na(lichen_cov), lichen_cov := 0]
cover.dt[is.na(moss_cov), moss_cov := 0]
cover.dt[, site_id := substr(plot_id, 1, 3)]
cover.dt[, plot_id := substr(plot_id, 4, 6)]

pft.dt <- cover.dt[pft.dt, on = c('site_id','plot_id')]

pft.dt[pft == 'lichen' & lichen_cov == 0, ':='(biomass_dry_weight_g = 0, 
                                               biomass_density_gm2 = 0, 
                                               method = 'survey',
                                               notes = paste0('Lichen cover ', lichen_cov, '%.'))]

pft.dt[pft == 'bryophyte' & moss_cov == 0, ':='(biomass_dry_weight_g = 0, 
                                               biomass_density_gm2 = 0, 
                                               method = 'survey',
                                               notes = paste0('Bryophyte cover ', lichen_cov, '%.'))]

pft.dt <- pft.dt[, c('lichen_cov','moss_cov') := NULL]


# specify that tree were not present and set biomass to zero
pft.dt[pft == 'tree', ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey')]

# fix a few herbs that are missing data but should be zero
pft.dt[(pft == 'herb' | pft == 'shrub') & is.na(biomass_density_gm2), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'harvest')]

pft.dt[, vegetation_description := first(vegetation_description, na_rm = T), by = 'plot_code']

# specify citation
pft.dt[, citation := as.character(citation)]
pft.dt[, citation := 'Happonen et al. 2022. Relationships between above-ground plant traits and carbon cycling in tundra plant communities. Journal of Ecology 110:700-716.']
pft.dt[, citation_short := 'Happonen et al. 2022']
pft.dt[is.na(notes), notes := 'none']

pft.dt[method == 'unmeasured', plot_area_m2 := NA]


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('plot_code','vegetation_description')]
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

# # make all character columns lowercase except contributor, country, locale, citation and citations short
# pft.dt <- columns_tolower(pft.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_04_virkkala_finland_kilpisjarvi_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================

unique(pft.dt$site_code)

table(pft.dt$vegetation_description)
pts.sf

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_code), color = ~as.character(vegetation_description)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_code),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))



length(unique(pft.dt$site_code))

# site.dt <- pft.dt[, .(agb.gm2 = sum(biomass_density_gm2, na.rm=T)), by = c('site_code','vegetation_description')]
# site.dt[, .(agb.gm2.avg = mean(agb.gm2, na.rm=T),
#             agb.gm2.sd = sd(agb.gm2, na.rm=T), 
#             n = .N), by = 'vegetation_description']


# # USE RANDOM FOREST MODEL TO PREDICT VEGETATION TYPE FOR PLOTS MEASURED IN 2017 AND 2018 -------------------------
# 
# # cast wide for use in RF model
# pft.wide.dt <- dcast(pft.dt,  site_code + plot_code + year + latitude + longitude + vegetation_description ~ pft, value.var = 'biomass_density_gm2')
# 
# # grab subset of columns for predictors, excluding bryophyte and lichen biomass b/c there are too many missing values to be useful
# pft.wide.dt <- pft.wide.dt[, c(1:6,8,10)]
# 
# # fit RF model to predict vegetation type using data from 2016 when vegetation was described
# veg_rf <- randomForest(vegetation_description ~ latitude + longitude + shrub + herb, data = pft.wide.2016.dt[year == 2016], type = 'classification')
# ##### ADD JULIAN DAY AS A PREDICTOR
# 
# # examine RF model 
# veg_rf
# varImpPlot(veg_rf)
# 
# # use RF to predict vegetation type on plots measured in 2017 and 2018 when vegetation was not described
# pft.wide.dt[year != 2016, vegetation_description := predict(veg_rf, pft.wide.dt[year != 2016]) ]
# table(pft.wide.dt$vegetation_description)
# 
# # attribute each sample plot with observed vegetation, or predicted vegetation if not observed in the field 
# pft.dt[, vegetation_description := pft.wide.dt$vegetation_description[match(pft.dt$plot_code, pft.wide.dt$plot_code)]]
# table(pft.dt$vegetation_description)


# # CLUSTER 2017 AND 2018 SAMPLE PLOTS ---------------------------------------------------------------------
# pft.wide.dt <- dcast(pft.dt[year != 2016],  plot_code + latitude + longitude  ~ pft, value.var = 'biomass_density_gm2')
# 
# # grab subset of columns for predictors, excluding bryophyte and lichen biomass b/c there are too many missing values to be useful
# pft.wide.dt <- pft.wide.dt[, c(1:3,5,7)]
# 
# pft.wide.dt
# 
# pft.wide.dt <- pft.wide.dt[, ':='(lat.scale = scale(latitude), 
#                                   lon.scale = scale(longitude),
#                                   herb.scale = scale(herb),
#                                   shrub.scale = scale(shrub))]
# 
# n.clusters = 20
# clust <- kmeans(pft.wide.dt[,c(6:9)], iter.max = 100, centers = n.clusters, nstart = 1)
# clust
# table(clust$cluster)
# 
# pft.wide.dt[, clust := clust$cluster]
# 
# pft.dt[, cluster := pft.wide.dt$clust[match(pft.dt$plot_code, pft.wide.dt$plot_code)]]
# 
# # add colors
# col.key <- data.table(cluster = unique(clust$cluster), 
#                       col = viridis(length(unique(clust$cluster))))
# 
# pft.dt[, cluster_col := col.key$col[match(pft.dt$cluster, col.key$cluster)]]
# 
# pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('plot_code','cluster','cluster_col')]
# pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
# pts.sf <- pts.sf %>% st_cast('POINT')
# 
# leaflet(pts.sf) %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   addCircles(label = ~as.character(cluster), color = ~as.character(cluster_col)) %>% 
#   addScaleBar(options = scaleBarOptions(imperial = F))



