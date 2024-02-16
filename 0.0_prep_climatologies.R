# DESCRIPTION ==================================================================
# This R script downloads and prepares Chelsa GDD0 and PPT climatologies for the Arctic.
# Author: Logan Berner, NAU
# Date: 2022-06-14
# Notes: 
# -- Downloading the Geotiffs using download.file() resulted in files that seemed
# -- not to have any data... Ended up manually downloading files. 
# -- Other data layers available through: https://chelsa-climate.org/

# SET UP =======================================================================
rm(list=ls())
require(terra)
require(raster)
require(sf)
require(ggplot2)
require(R.utils)
require(gdalUtilities)

# LOAD ARCTIC SPATIAL DOMAIN SHAPEFILE ========================================
# arctic.sf <- read_sf('data/gis_data/boundaries/arctic_oroarctic_laea.shp')
arctic.sf <- read_sf('data/gis_data/boundaries/northern_bioclimatic_zones_laea.shp')
extnt <- ext(arctic.sf)

# DOWNLOAD CHELSEA CLIMATE DATA ===============================================
# mkdirs('data/gis_data/climate')
# ppt.url <- 'https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif'
# gdd.url <- 'https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gdd0_1981-2010_V.2.1.tif'
# download.file(ppt.url, 'data/gis_data/climate/CHELSA_bio12_1981-2010_V.2.1.tif')
# download.file(gdd.url, 'data/gis_data/climate/CHELSA_gdd0_1981-2010_V.2.1.tif')

# CLIP AND REPROJECT CLIMATE DATA =============================================
mat.r <- rast('data/gis_data/climate/CHELSA_bio1_1981-2010_V.2.1.tif')
gdd.r <- rast('data/gis_data/climate/CHELSA_gdd0_1981-2010_V.2.1.tif')
prec.r <- rast('data/gis_data/climate/CHELSA_bio12_1981-2010_V.2.1.tif')

gdalwarp(srcfile = 'data/gis_data/climate/CHELSA_bio1_1981-2010_V.2.1.tif', 
         dstfile = 'data/gis_data/climate/CHELSA_bio1_1981-2010_arctic_laea_V.2.1.tif', 
         t_srs = 'EPSG:3571', r = 'average', tr = c(1000,1000), te = extnt[c(1,3,2,4)])

gdalwarp(srcfile = 'data/gis_data/climate/CHELSA_gdd0_1981-2010_V.2.1.tif', 
         dstfile = 'data/gis_data/climate/CHELSA_gdd0_1981-2010_arctic_laea_V.2.1.tif', 
         t_srs = 'EPSG:3571', r = 'average', tr = c(1000,1000), te = extnt[c(1,3,2,4)])

gdalwarp(srcfile = 'data/gis_data/climate/CHELSA_bio12_1981-2010_V.2.1.tif', 
         dstfile = 'data/gis_data/climate/CHELSA_bio12_1981-2010_arctic_laea_V.2.1.tif', 
         t_srs = 'EPSG:3571', r = 'average', tr = c(1000,1000), te = extnt[c(1,3,2,4)])

# check files
r <- rast('data/gis_data/climate/CHELSA_bio1_1981-2010_arctic_laea_V.2.1.tif')
plot(r)

r <- rast('data/gis_data/climate/CHELSA_gdd0_1981-2010_arctic_laea_V.2.1.tif')
plot(r)

r <- rast('data/gis_data/climate/CHELSA_bio12_1981-2010_arctic_laea_V.2.1.tif')
plot(r)

# END SCRIPT ====================================================================