# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Verity Salmon (ORNL)
# Script Author: Logan Berner
# Script Date: 2022-04-27, 2023-03-27
# Dataset Notes: 
# - Verity grouped individual quadrats ("Plots") into vegetation community types, each of which will be considered
#     as a "Site" in the synthesis database. 
# - Standing dead not harvested

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(data.table)
require(dplyr)
require(sf)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx'))
in.file <- 'data/synthesis_database/2_contributed_data_raw/dataset_03_salmon_usa_kougarok/Salmon_NGEEArctic_USA_Kougarok_biomass_data_20211206.xlsx'
harv.dt <- data.table(read_excel(in.file, sheet = 'Biomass_PerPlot&Species'))
site.dt <- data.table(read_excel(in.file, sheet = '2016BiomassHarvestLocations'))
# site.dt <- data.table(read_excel(in.file, sheet = 'Biomass_PerPlot'))


# PREPARE SITE AND COORDINATE DATA ===============================================================

# Get key that groups plots into vegetation communities, plot ids, and coordinates 
names(site.dt)
site.keep.cols <- c('Merged Community name for Berner','Plot ID','Nlat_WGS84dd','Wlon_WGS84dd')
site.dt <- site.dt[, ..site.keep.cols]

# Adjust column names 
setnames(site.dt, 
         site.keep.cols,
         c('site_id','plot_id','latitude','longitude'))


# Adjust vegetation community types which become the site_id
unique(site.dt$site_id)
site.dt[site_id == 'Low Shrub', site_id := 'LowShrub']
site.dt[site_id == 'Graminoid Tundra + Shrub', site_id := 'GraminoidShrub']
site.dt[site_id == 'Alder-Willow Shrub', site_id := 'TallShrub']

# Round coordinates to 6 decimals
site.dt[, latitude := round(latitude, 6)]
site.dt[, longitude := round(longitude, 6)]


# Add metadata consistent across all samples
colnames(database.tmplt.dt)

site.dt[, ":="(dataset_id = 'ds03',
               contributor = 'Salmon V, Iversen C, Kumar J',
               country = 'USA',
               locale = 'Kougaork',
               coord_type = 'plot',
               year = 2016,
               month = 7,
               day = NA,
               site_description = 'Rocky outcrop with steep slope transitioning to lowland wet tundra', 
               citation = 'Salmon et al. 2019. NGEE Arctic Plant Traits: Plant Aboveground Biomass NPP and Traits 
               Kougarok Road Mile Marker 64 Seward Peninsula Alaska beginning 2016. Next Generation Ecosystem 
               Experiments Arctic Data Collection, https://doi.org/10.5440/1346199.',
               citation_short = 'Salmon et al. 2019')]

# create site and plot codes
site.dt[, site_code := paste(country, locale, site_id, sep='.')]
site.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

site.dt


# STANDARDIZE HARVEST DATASET =================================================================

# turn column names lower case 
colnames(harv.dt) <- tolower(colnames(harv.dt))

# specify method for quantifying biomass (harvest or survey)
# sort(unique(harv.dt$speciescode))
# harv.dt[speciescode == "ALNFRU" | speciescode == "BETGLA" | speciescode == "SALGLA" | speciescode == "SALPUL", method := 'survey']
# harv.dt[speciescode != "ALNFRU" & speciescode != "BETGLA" & speciescode != "SALGLA" & speciescode != "SALPUL", method := 'harvest']

# sum biomass across organs for each species (e.g., leaf, wood,...) and, for now, convert from g / m2 back to g / quadrat
harv.dt[, biomass_density_gm2 := biomassfruit_gdwperm2 + biomassinflorescense_gdwperm2 + biomassleaf_gdwperm2 + biomassstem_gdwperm2 + biomassnonvasculartissue_gdwperm2]

# select subset of columns
# keep.cols <- c('plot_id','method','speciescode','growthform','biomass_density_gm2')
keep.cols <- c('plot_id','speciescode','growthform','biomass_density_gm2')
harv.dt <- harv.dt[, ..keep.cols]

# group species into plant functional types
unique(harv.dt$speciescode)
unique(harv.dt$growthform)
harv.dt[growthform == 'woody plant', pft := 'shrub']
harv.dt[growthform == 'graminoid', pft := 'herb']
harv.dt[growthform == 'forb', pft := 'herb']
harv.dt[growthform == 'seedless vascular plant', pft := 'herb'] # horse tails
harv.dt[growthform == 'nonvascular' & speciescode == 'lichen', pft := 'lichen']
harv.dt[growthform == 'nonvascular' & speciescode == 'moss', pft := 'bryophyte']
harv.dt[growthform == 'nonvascular' & speciescode == 'liverwort', pft := 'bryophyte']
unique(harv.dt$pft)
harv.dt <- harv.dt[, growthform := NULL]

# check dataset PFTs against database (note: standing dead not harvested)
check_pfts(harv.dt)

# add plot area for each PFT (based on email with Verity 2022-05-10 "Field biomass measurements") -------

# non-shrubs harvested on 50 cm x 20 cm plots
harv.dt[pft != 'shrub', ':='(plot_area_m2 = 0.5 * 0.2, method = 'harvest')]

# all non-tall shrubs clipped on 50 cm x 20 cm plots
harv.dt[pft == 'shrub' & 
          (speciescode != 'ALNFRU' & speciescode != 'BETGLA' & speciescode != 'BETNAN' & speciescode != 'SALGLA' & speciescode != 'SALPUL'),
        ':='(plot_area_m2 = 0.5 * 0.2, method = 'harvest')]

# tall shrubs (alder, willow, birch) on AS-1 and AS-2 measured on 5 x 5 m plots
harv.dt[(plot_id == 'AS-1' | plot_id == 'AS-2') & 
          (speciescode == 'ALNFRU' | speciescode == 'BETGLA' | speciescode == 'BETNAN' | speciescode == 'SALGLA' | speciescode == 'SALPUL'),
        ':='(plot_area_m2 = 5 * 5, method = 'survey')]

# tall shrubs on TT-WBT-1 & TT-WBT-3 measured on 2.5 x 2.5 m plots
harv.dt[(plot_id == 'TT-WBT-1' | plot_id == 'TT-WBT-3') & 
          (speciescode == 'ALNFRU' | speciescode == 'BETGLA' | speciescode == 'BETNAN' | speciescode == 'SALGLA' | speciescode == 'SALPUL'),
        ':='(plot_area_m2 = 2.5 * 2.5, method = 'survey')]

# tall shrubs on TT-1 & TT-2 measured on 2.5 x 2.5 m plots
harv.dt[(plot_id == 'TT-1' | plot_id == 'TT-2') & 
          (speciescode == 'ALNFRU' | speciescode == 'BETGLA' | speciescode == 'BETNAN' | speciescode == 'SALGLA' | speciescode == 'SALPUL'),
        ':='(plot_area_m2 = 2.5 * 2.5, method = 'survey')]

# tall shrubs on WBT-1 & WBT-2 measured on 1 x 1 m plots
harv.dt[(plot_id == 'WBT-1' | plot_id == 'WBT-2') & 
          (speciescode == 'ALNFRU' | speciescode == 'BETGLA' | speciescode == 'BETNAN' | speciescode == 'SALGLA' | speciescode == 'SALPUL'),
        ':='(plot_area_m2 = 1 * 1, method = 'survey')]

# DSLT-1 (follow up with Verity)
harv.dt[plot_id == 'DSLT-1' & speciescode == 'BETNAN', ':='(plot_area_m2 = 0.5 * 0.2, method = 'survey')]


# sum biomass across species within each PFT for each site x plot ---------
value.cols <- paste(c('speciescode','biomass_density_gm2','plot_area_m2','method'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                      plot_area_m2 = max(plot_area_m2),
                      method = concat_rows(method),
                      vegetation_description = concat_rows(speciescode)), 
                  by = eval(grouping.cols)]

pft.dt

# adjust harvest to account for quadrats where there shrub biomass was both harvested and surveyed
pft.dt[grep('survey, harvest', method), method := 'harvest + survey']
pft.dt[grep('harvest, harvest', method), method := 'harvest']
pft.dt

# add note about hierarchial plot sizes
pft.dt[method == 'harvest + survey', notes := 'tall shrubs surveyed on specified plot area with low shrubs harvested on 0.5 m x 0.2 m subplot']

# back calculate biomass dry weight using biomass density (g/m2) and plot area (m2)
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]


# JOIN SITE AND HARVEST DATA ================================================================================
pft.dt <- site.dt[pft.dt, on = 'plot_id']

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
pft.dt[is.na(biomass_dry_weight_g), biomass_dry_weight_g := 0]
pft.dt[is.na(biomass_density_gm2), biomass_density_gm2 := 0]
pft.dt[is.na(vegetation_description), vegetation_description := 'none']
pft.dt[is.na(notes), notes := 'none']

pft.dt[pft == 'tree', ':='(method = 'survey', plot_area_m2 = 25)] # set plot size to size used for alder patches 


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_id']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_id)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_id),
                      labelOptions = labelOptions(noHide = T, textsize = '20px',
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_03_salmon_usa_kougaork_standardized.csv'
fwrite(pft.dt, out.file) #, eol = "\r")

# END SCRIPT =================================================================================================