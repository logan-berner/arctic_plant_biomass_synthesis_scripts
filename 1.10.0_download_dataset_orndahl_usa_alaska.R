# SCRIPT METADATA ==========================================================================================
# Description: R script to download biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Katie Orndahl (Northern Arizona University)
# Script Author: Melissa Rose
# Script Date: 2022-06-07
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(utils)
setwd('data/synthesis_database/2_contributed_data_raw/')

# create download directory
download_directory <- 'dataset_10_orndahl_usa_alaska/'
if (file.exists(download_directory)){
  print(paste('directory already exists:', download_directory))
  setwd(download_directory)
} else {
  print(paste('creating directory:', download_directory))
  dir.create(download_directory)
  setwd(download_directory)
}

# DOWNLOAD DATASET ==============================================================================================

# download plot location and understory biomass data
url <-'https://arcticdata.io/metacat/d1/mn/v2/packages/application%2Fbagit-097/resource_map_doi%3A10.18739%2FA2R785Q5B'
download.file(url, destfile = 'biomass.zip', mode = 'wb')

biomass <- unz(description = 'biomass.zip', filename = 'data/biomass.csv')
biomass.dt <- read.csv(biomass)
write.csv(biomass.dt, file = 'biomass.csv', eol = "\r")

site <- unz(description = 'biomass.zip', filename = 'data/site_attributes.csv')
site.dt <- read.csv(site)
write.csv(site.dt, file = 'site_attributes.csv', eol = "\r")

quad <- unz(description = 'biomass.zip', filename = 'data/quad_attributes.csv')
quad.dt <- read.csv(quad)
write.csv(quad.dt, file = 'quad_attributes.csv', eol = "\r")

unzip(zipfile = 'biomass.zip', files = 'data/doi_10.18739_A2R785Q5B-METADATA.pdf', junkpaths = TRUE) #metadata

unlink('biomass.zip')

# END SCRIPT =================================================================================================