# SCRIPT METADATA ==========================================================================================
# Description: R script to download biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Susan Natali (Woods Hole Research Center)
# Script Author: Melissa Rose 
# Script Date: 2022-05-18
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(utils)
setwd('data/synthesis_database/2_contributed_data_raw/')

# create download directory
download_directory <- 'dataset_07_natali_usa_alaska/'
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
url <-'https://arcticdata.io/metacat/d1/mn/v2/packages/application%2Fbagit-097/resource_map_urn%3Auuid%3A1ab62fd5-0758-46cf-b883-a8646e188fee'
download.file(url, destfile = 'understory.zip', mode = 'wb')

coord <- unz(description = 'understory.zip', filename = 'data/ViPER_US_biomass_coordinates_2015.csv')
coord.dt <- read.csv(coord)
coord.dt <- coord.dt[!is.na(coord.dt$Site),]
write.csv(coord.dt, file = 'ViPER_US_biomass_coordinates_2015.csv', eol = "\r")

pft_us <- unz(description = 'understory.zip', filename = 'data/ViPER_US_biomass_2015.csv')
pft_us.dt <- read.csv(pft_us)
pft_us.dt <- pft_us.dt[!is.na(pft_us.dt$Site),]
write.csv(pft_us.dt, file = 'ViPER_US_biomass_2015.csv', eol = "\r")
  
species_us <- unz(description = 'understory.zip', filename = 'data/ViPER_US_biomass_shrub_bd_2015.csv')
species_us.dt <- read.csv(species_us)
species_us.dt <- species_us.dt[!is.na(species_us.dt$Site),]
write.csv(species_us.dt, file = 'ViPER_US_biomass_shrub_bd_2015.csv', eol = "\r")

unzip(zipfile = 'understory.zip', files = 'data/doi_10.18739_A2S756K6M-METADATA.pdf', junkpaths = TRUE) #metadata

unlink('understory.zip')


# download site and tree biomass data
url <- 'https://arcticdata.io/metacat/d1/mn/v2/packages/application%2Fbagit-097/resource_map_urn%3Auuid%3A247a1021-65a7-4bb3-81ea-5fea40e6f037'
download.file(url, destfile = 'tree.zip', mode = 'wb')

site <- unz(description = 'tree.zip', filename = 'data/ViPER_tree_biomass_site_information_2015.csv')
site.dt <- read.csv(site)
write.csv(site.dt, file = 'ViPER_tree_biomass_site_information_2015.csv', eol = "\r")

species_tr <- unz(description = 'tree.zip', filename = 'data/ViPER_tree_biomass_2015.csv')
species_tr.dt <- read.csv(species_tr)
species_tr.dt <- species_tr.dt[!is.na(species_tr.dt$Site),]
write.csv(species_tr.dt, file = 'ViPER_tree_biomass_2015.csv', eol = "\r")

## check that plot locations are the same as understory
# coord_tr <- unz(description = 'tree.zip', filename = 'data/ViPER_tree_biomass_coordinates_2015.csv')
# coord_tr.dt <- read.csv(coord_tr)
# coord_tr.dt <- coord_tr.dt[!is.na(coord_tr.dt$Site),]
# identical(coord.dt, coord_tr.dt)

unzip(zipfile = 'tree.zip', files = 'data/doi_10.18739_A2DZ0324M-METADATA.pdf', junkpaths = TRUE) #metadata

unlink('tree.zip')



