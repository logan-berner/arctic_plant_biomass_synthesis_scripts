# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Anna Gospard (LAval University)
# Script Author: Logan Berner
# Script Date: 2023-08-23
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
harv.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_25_gaspard_canada_nunavik/Biomass_Nunavik_Gaspard_Boudreau_sampling.csv')

# ADD METADATA ======================================================================================

# Convert from commas to dots for decimal places
harv.dt[ , (names(harv.dt)) := lapply(.SD, FUN = function(x){gsub(",","\\.",x)}), .SDcols = names(harv.dt)]

# convert names to lower case
names(harv.dt) <- tolower(names(harv.dt))

# adjust column names
names(harv.dt)
setnames(harv.dt, 
         c('region','vegetation description','vegetation type','site id','functional group','number of quadrats','biomass (g)','individual quadrat size (ha)','total sampling area (ha)','cover (%)'), 
         c('locale','vegetation_description','site_description','site_id','pft','n_subplots','biomass_dry_weight_g','subplot_area_ha','plot_area_ha','cover_pcnt'))

# add metadata
harv.dt[, ":="(dataset_id = 'ds25',
               contributor = 'Gaspard A, Boudreau S',
               country = 'Canada',
               citation = 'Gaspard A and Boudreau S. 2023. Climatic data are better predictors of NDVI values than plant functional group biomass and cover along a latitudinal gradient in Nunavik. Manuscript submitted for publication.',
               citation_short = 'Gaspard and Boudreau 2023',
               method = 'harvest',
               coord_type = 'site')]

# separate dates into year month and day
harv.dt <- separate(data = harv.dt, col = date, into = c('year','month','day'), sep = '-') %>% as.data.table()

# convert locale from abbreviation to full place name provided by Anna
sort(unique(harv.dt$locale))
harv.dt[locale == 'BEDE', locale := 'Deception Bay']
harv.dt[locale == 'BON', locale := 'Boniface River']
harv.dt[locale == 'CHUK', locale := 'Chukotka River']
harv.dt[locale == 'IVUJ', locale := 'Ivujivik']
harv.dt[locale == 'LEC', locale := 'Clearwater Lake']
harv.dt[locale == 'NBON', locale := 'Le Roy Lake']
harv.dt[locale == 'PAY', locale := 'Payne Lake']
harv.dt[locale == 'PUVI', locale := 'Couture Lake']

# drop region abbreviation from site_id
harv.dt[, site_id := matrix(unlist(strsplit(harv.dt$site_id, split = '_')), ncol = 2, byrow = T)[,2]]

# each field site considered to have one plot (that is composed of 5 - 10 subplots...)
harv.dt[, plot_id := '1']

# create site and plot codes
harv.dt[, site_code := paste(country, gsub('\\ ','', locale), site_id, sep='.')]
harv.dt[, plot_code := paste(country, gsub('\\ ','', locale), site_id, plot_id, sep='.')]

unique(harv.dt$site_code)
unique(harv.dt$plot_code)

# sort 
setorder(harv.dt, site_id, plot_id)


# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# standardize plant functional types 
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)

harv.dt[pft == 'bryophytes', pft := 'bryophyte']
harv.dt[pft == 'herbaceous', pft := 'herb']
harv.dt[pft == 'lichens', pft := 'lichen']
harv.dt[pft %in% c('prostrate shrubs','erect shrubs'), pft := 'shrub']

unique(harv.dt$pft)
check_pfts(harv.dt)

# convert plot area from hectares to square meters
harv.dt[, plot_area_m2 := as.numeric(harv.dt$plot_area_ha)*10^4]
harv.dt <- harv.dt[, plot_area_ha := NULL]

harv.dt[, subplot_area_m2 := as.numeric(harv.dt$subplot_area_ha)*10^4]
harv.dt <- harv.dt[, subplot_area_ha := NULL]

# specify that harvest biomass was pooled across n subplots
harv.dt[, notes := paste0("Sample pooled from ", n_subplots, " subplots that were each ", subplot_area_m2, " m2.")]
harv.dt <- harv.dt[, ':='(n_subplots = NULL, subplot_area_m2 = NULL)]

# make biomass and cover numeric 
harv.dt[, biomass_dry_weight_g := as.numeric(biomass_dry_weight_g)]
harv.dt[, cover_pcnt := as.numeric(cover_pcnt)]

# calculate biomass density
harv.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# adjust lichen and bryophyte biomass density using percent cover because samples were taken from solid patches
harv.dt[pft == 'lichen' | pft == 'bryophyte', biomass_density_gm2 := biomass_density_gm2 * (cover_pcnt / 100)]
harv.dt[pft == 'lichen' | pft == 'bryophyte', method := 'harvest + survey']
harv.dt[pft == 'lichen' | pft == 'bryophyte', notes := paste0(notes, " ", capitalize(pft), " cover ", round(cover_pcnt), "%.")]
harv.dt[pft == 'lichen' | pft == 'bryophyte', notes := paste0(notes, " ", "Biomass scaled by percent cover.")]
harv.dt <- harv.dt[, cover_pcnt := NULL]
harv.dt

# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('biomass_dry_weight_g','biomass_density_gm2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g),
                      biomass_density_gm2 = sum(biomass_density_gm2)),
                  by = eval(grouping.cols)]

dim(harv.dt)
dim(pft.dt)

# expand data table so every pft occurs in every plot
dim(pft.dt)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

pft.dt[pft == 'herb' & is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]

fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')
fill_missing_metadata(pft.dt, fill.cols = 'site_description', by = 'plot_code')


# set tree biomass to zero on tundra plots and unmeasured on forest plots
unique(pft.dt$vegetation_description)

pft.dt[pft == 'tree' & is.na(biomass_dry_weight_g),
       ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0, method = 'survey', plot_area_m2 = 30 * 40)]

pft.dt[pft == 'tree' & (vegetation_description == "Coniferous forest with lichen" | vegetation_description == "Coniferous forest with lichens and mosses"), 
       ':='(biomass_dry_weight_g = NA, biomass_density_gm2 = NA, method = 'unmeasured', plot_area_m2 = NA)]

pft.dt[is.na(notes), notes := 'none']

pft.dt
dim(pft.dt)


# SPOT CHECK BIOMASS DENSITY FOR SEVERAL SITES AGAINST SUMMARIES FROM DATA PROVIDER
harv2.dt <- fread('data/synthesis_database/2_contributed_data_raw/dataset_25_gaspard_canada_nunavik/Biomass_Nunavik_Gaspard_Boudreau.csv')
harv2.dt <- harv2.dt[ , (names(harv2.dt)) := lapply(.SD, FUN = function(x){gsub(",","\\.",x)}), .SDcols = names(harv2.dt)]
harv2.dt[2,]
pft.dt[site_id == "BEGL2"]


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = c('site_code','vegetation_description')]
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

# st_write(pts.sf, 'data/gis_data/tmp/gospard_sites.gpkg')

# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 


# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_25_gaspard_canada_nunavik_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================