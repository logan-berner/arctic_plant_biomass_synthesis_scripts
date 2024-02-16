# Description: R script to combine standardized biomass datasets into the synthesis dataset, 
# as well as summarize features of synthesis dataset. 
# Date: 2022-06-7
# Author: Logan Berner, NAU
# Notes: 

# SET UP ================================================================================
rm(list=ls())
require(data.table)
require(tidyverse)
require(sf); sf_use_s2(F)
require(terra)
require(leaflet)
require(R.utils)
require(readxl)
require(exactextractr)
source('scripts/synthesis_database_functions.R')
mkdirs('data/synthesis_database/4_synthesis_dataset/')
laea <- ("+proj=laea +x_0=0 +y_0=0 +lon_0=180 +lat_0=90")

# READ FILES ===================================================================

# field data
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
in.files <- list.files('data/synthesis_database/3_contributed_data_processed/', full.names = T)
pft.dt <- do.call("rbind", lapply(in.files, fread))
dim(pft.dt)

# spatial data
bioclim.zone.sf <- read_sf('data/gis_data/boundaries/northern_bioclimatic_zones_laea.shp')
boreal.sf <- read_sf('data/gis_data/boundaries/wwf_boreal_biome_laea.shp')
mat.r <- rast('data/gis_data/climate/CHELSA_bio1_1981-2010_arctic_laea_V.2.1.tif')
ppt.r <- rast('data/gis_data/climate/CHELSA_bio12_1981-2010_arctic_laea_V.2.1.tif')
gdd.r <- rast('data/gis_data/climate/CHELSA_gdd0_1981-2010_arctic_laea_V.2.1.tif')

# ROUND OFF BIOMASS VALUES =====================================================
pft.dt[, ':='(biomass_dry_weight_g = round(biomass_dry_weight_g, 2),
              biomass_density_gm2 = round(biomass_density_gm2, 2))]


# ==============================================================================
# EXTRACT SPATIAL DATA FOR SAMPLE PLOTS
# ==============================================================================

# spatialize sample plots
plot.dt <- subset(pft.dt, select = c(dataset_id, citation_short, site_code, plot_code, coord_type, year, latitude, longitude)) 
plot.dt <- unique(plot.dt, by='plot_code')
plot.sf <- plot.dt %>% data.frame %>% st_as_sf(coords = c('longitude', 'latitude'), crs = 'EPSG:4326', agr = "constant") %>% st_cast('POINT') %>% st_transform(laea)
plot.sf <- plot.sf %>% mutate(longitude = st_coordinates(plot.sf$geometry)[,1], latitude = st_coordinates(plot.sf$geometry)[,2])
plot.vec <- vect(plot.sf)

fwrite(plot.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset_plot_locations.csv')
st_write(plot.sf, 'data/gis_data/field_locations/arctic_tundra_biomass_synthesis_plots.gpkg', append = F)


# extract bioclimatic zone for each sample plot
bioclim.zone.extract <- st_intersection(bioclim.zone.sf, plot.sf)
plot.dt$bioclim_zone <- bioclim.zone.extract$Zone[match(plot.dt$plot_code, bioclim.zone.extract$plot_code)]

unique(plot.dt[is.na(bioclim_zone)]$site_code)
plot.dt[site_code == 'Canada.PrincePatrickIsland.MouldBay-C05', bioclim_zone := 'High Arctic']
plot.dt[site_code == 'USA.NorthSlope.HoweIsland-C12', bioclim_zone := 'High Arctic']
plot.dt[site_code == 'USA.NorthSlope.HoweIsland-C13', bioclim_zone := 'High Arctic']
plot.dt[site_code == 'Russia.Yamal.Kharasavey-2a', bioclim_zone := 'High Arctic']

# extract climatic norms
plot.dt[, ':='(mat_degC = terra::extract(mat.r, plot.vec)[,2], 
               gdd_degC = terra::extract(gdd.r, plot.vec)[,2],
               map_mm = terra::extract(ppt.r, plot.vec)[,2])]

# append spatial data to main plot data
pft.dt$bioclim_zone <- plot.dt$bioclim_zone[match(pft.dt$plot_code, plot.dt$plot_code)]
pft.dt$mat_degC <- plot.dt$mat_degC[match(pft.dt$plot_code, plot.dt$plot_code)]
pft.dt$gdd_degC <- plot.dt$gdd_degC[match(pft.dt$plot_code, plot.dt$plot_code)]
pft.dt$map_mm <- plot.dt$map_mm[match(pft.dt$plot_code, plot.dt$plot_code)]


# ==============================================================================
# SPATIALIZE FIELD SITES & SAMPLE PLOTS, CREATE INTERACTIVE MAP, & WRITE OUT ===
# ==============================================================================

site.dt <- pft.dt[, .(latitude = mean(latitude), longitude = mean(longitude)), 
                  by = c('dataset_id','citation_short', 'site_code','bioclim_zone','mat_degC','gdd_degC','map_mm')]
site.sf <- site.dt %>% data.frame %>% st_as_sf(coords = c("longitude", "latitude"), crs = 'EPSG:4326', agr = "constant") %>% st_cast('POINT') 
site.sf <- site.sf %>% mutate(longitude = st_coordinates(site.sf$geometry)[,1], latitude = st_coordinates(site.sf$geometry)[,2])

# create map
leaflet(site.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(site_code), radius = 3, 
                   color = 'purple', fill = T, fillColor = 'white', fillOpacity = 1) %>% 
  addScaleBar(options = scaleBarOptions(imperial = F))

# write shapefile
fwrite(site.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset_site_locations.csv')
st_write(site.sf, 'data/gis_data/field_locations/arctic_tundra_biomass_synthesis_sites.gpkg', append = F)


# ==============================================================================
# CREATE COMPLETENESS FLAGS 
# ==============================================================================


# IDENTIFY PLOTS WHERE BIOMASS WAS OR WAS NOT MEASURED FOR EVERY PFT ===========
pft.dt[method == 'unmeasured', community_measured := FALSE]

# subset all plots where biomass is missing for any pft
community.unmeasured.dt <- pft.dt[, .SD[any(community_measured == FALSE)], by = plot_code]
community.unmeasured.dt[, community_measured := FALSE]

# subset all plots where biomass was measured for every pft
community.measured.dt <- pft.dt[(plot_code %in% unique(community.unmeasured.dt$plot_code)) == F]
community.measured.dt[, community_measured := TRUE]

# combine back together and sort
pft.dt <- rbind(community.measured.dt, community.unmeasured.dt)
setorder(pft.dt, dataset_id, site_code)


# IDENTIFY PLOTS WHERE PLANT BIOMASS (TREE + SHRUB + HERB + BRYOPHYTE) BIOMASS WAS OR WAS NOT MEASURED ====
pft.dt[method == 'unmeasured' & (pft %in% c('tree', 'shrub', 'herb', 'bryophyte')), plants_measured := FALSE]
pft.dt[pft == 'total', plants_measured := FALSE]

plants.unmeasured.dt <- pft.dt[, .SD[any(plants_measured == FALSE)], by = plot_code]
plants.unmeasured.dt[, plants_measured := FALSE]

plants.measured.dt <- pft.dt[(plot_code %in% unique(plants.unmeasured.dt$plot_code)) == F]
plants.measured.dt[, plants_measured := TRUE]

pft.dt <- rbind(plants.measured.dt, plants.unmeasured.dt)
setorder(pft.dt, dataset_id, site_code)


# IDENTIFY PLOTS WHERE NON-TREE PLANT BIOMASS (SHRUB + HERB + BRYOPHYTE) BIOMASS WAS OR WAS NOT MEASURED ====
pft.dt[method == 'unmeasured' & (pft %in% c('shrub', 'herb', 'bryophyte')), nontree_plants_measured := FALSE]
pft.dt[pft == 'total', nontree_plants_measured := FALSE]

tundra.plants.unmeasured.dt <- pft.dt[, .SD[any(nontree_plants_measured == FALSE)], by = plot_code]
tundra.plants.unmeasured.dt[, nontree_plants_measured := FALSE]

tundra.plants.measured.dt <- pft.dt[(plot_code %in% unique(tundra.plants.unmeasured.dt$plot_code)) == F]
tundra.plants.measured.dt[, nontree_plants_measured := TRUE]

pft.dt <- rbind(tundra.plants.measured.dt, tundra.plants.unmeasured.dt)
setorder(pft.dt, dataset_id, site_code)


# IDENTIFY PLOTS WHERE VASCULAR (TREE + SHRUB + HERB) BIOMASS WAS OR WAS NOT MEASURED ====
pft.dt[method == 'unmeasured' & (pft %in% c('tree', 'shrub', 'herb')), vascular_measured := FALSE]
pft.dt[pft == 'total', vascular_measured := FALSE]

vascular.unmeasured.dt <- pft.dt[, .SD[any(vascular_measured == FALSE)], by = plot_code]
vascular.unmeasured.dt[, vascular_measured := FALSE]

vascular.measured.dt <- pft.dt[(plot_code %in% unique(vascular.unmeasured.dt$plot_code)) == F]
vascular.measured.dt[, vascular_measured := TRUE]

pft.dt <- rbind(vascular.measured.dt, vascular.unmeasured.dt)
setorder(pft.dt, dataset_id, site_code)


# IDENTIFY PLOTS WHERE WOODY (TREE + SHRUB) BIOMASS WAS OR WAS NOT MEASURED ====
pft.dt[method == 'unmeasured' & (pft == 'tree' | pft == 'shrub'), woody_measured := FALSE]
pft.dt[pft == 'total', woody_measured := FALSE]

woody.unmeasured.dt <- pft.dt[, .SD[any(woody_measured == FALSE)], by = plot_code]
woody.unmeasured.dt[, woody_measured := FALSE]

woody.measured.dt <- pft.dt[(plot_code %in% unique(woody.unmeasured.dt$plot_code)) == F]
woody.measured.dt[, woody_measured := TRUE]

pft.dt <- rbind(woody.measured.dt, woody.unmeasured.dt)
setorder(pft.dt, dataset_id, site_code)


# ==============================================================================
#  WRITE OUT PFT SYNTHESIS DATASET
# ==============================================================================

# adjust column order
names(pft.dt)
neworder = c('dataset_id','contributor','country','locale','site_id','site_code',
             'plot_id','plot_code','coord_type','latitude','longitude','year','month',
             'day','pft','plot_area_m2','method','biomass_dry_weight_g','biomass_density_gm2',
             'vegetation_description','site_description','bioclim_zone','mat_degC','gdd_degC','map_mm',
             'community_measured','plants_measured','nontree_plants_measured','vascular_measured','woody_measured','citation','citation_short','notes')
neworder
setcolorder(pft.dt, neworder = neworder)

fwrite(pft.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')
fwrite(pft.dt, 'data/synthesis_database/4_synthesis_dataset/the_arctic_plant_aboveground_biomass_synthesis_dataset.csv')
dim(pft.dt)


# HARMONIZE DATES FOR SITES WHERE BIOMASS WAS HARVESTED AT DIFFERENT TIMES =====
# - this is needed for the total and woody biomass summaries below
# - set dates to when herbs were harvested
dim(pft.dt)

tot.dt <- pft.dt[pft == 'total']
pft.dt <- pft.dt[pft != 'total']
herb.dates.dt <- pft.dt[pft == 'herb', c('plot_code','year','month','day')]
pft.dt <- pft.dt[, c('year','month','day') := NULL]
pft.dt <- herb.dates.dt[pft.dt, on = 'plot_code']
pft.dt <- rbind(pft.dt, tot.dt)
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

dim(pft.dt)


# COMPUTE TOTAL PLANT BIOMASS (EXCLUDING LICHENS) FOR PLOTS WITH COMPLETE DATA  =====================
grouping.cols <- c('dataset_id','contributor','country','locale','site_code','site_id','plot_id','plot_code',
                   'coord_type','latitude','longitude','year','month','day','citation_short','citation','bioclim_zone')

plot.plant.agb.dt <- pft.dt[plants_measured == TRUE & (pft != 'lichen'),
                            .(plant_biomass_density_gm2 = sum(biomass_density_gm2)), 
                            by = eval(grouping.cols)]

check.na <- plot.plant.agb.dt[is.na(plant_biomass_density_gm2)]
check.na

fwrite(plot.plant.agb.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_total_plant_biomass_synthesis_dataset.csv')


# COMPUTE TOTAL TUNDRA PLANT BIOMASS (EXCLUDES TREES AND LICHENS) FOR PLOTS WITH COMPLETE DATA  =====================
grouping.cols <- c('dataset_id','contributor','country','locale','site_code','site_id','plot_id','plot_code',
                   'coord_type','latitude','longitude','year','month','day','citation_short','citation','bioclim_zone')

plot.nontree.plant.agb.dt <- pft.dt[nontree_plants_measured == TRUE & (pft %in% c('shrub', 'herb', 'bryophyte')),
                            .(plant_biomass_density_gm2 = sum(biomass_density_gm2)), 
                            by = eval(grouping.cols)]

check.na <- plot.nontree.plant.agb.dt[is.na(plant_biomass_density_gm2)]
check.na

fwrite(plot.nontree.plant.agb.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_total_nontree_plant_biomass_synthesis_dataset.csv')


# COMPUTE WOODY PLANT BIOMASS FOR PLOTS WITH COMPLETE DATA  =====================
grouping.cols <- c('dataset_id','contributor','country','locale','site_code','site_id','plot_id','plot_code',
                   'coord_type','latitude','longitude','year','month','day','citation_short','citation','bioclim_zone')

plot.woody.agb.dt <- pft.dt[(woody_measured == TRUE) & (pft == 'tree' | pft == 'shrub'),
                            .(plant_biomass_density_gm2 = sum(biomass_density_gm2)), 
                            by = eval(grouping.cols)]

plot.woody.agb.dt[is.na(plant_biomass_density_gm2)]

fwrite(plot.woody.agb.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_total_woody_plant_biomass_synthesis_dataset.csv')


# WRITE OUT SHURB BIOMASS FOR PLOTS WITH DATA ==================================
shrub.agb.dt <- pft.dt[pft == 'shrub' & method != 'unmeasured']
fwrite(shrub.agb.dt, 'data/synthesis_database/4_synthesis_dataset/arctic_tundra_total_shrub_biomass_synthesis_dataset.csv')

# END SCRIPT ===========================================================================