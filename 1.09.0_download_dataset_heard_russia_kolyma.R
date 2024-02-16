# SCRIPT METADATA ==========================================================================================
# Description: R script to download biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Robert Holmes (Woodwell Climate Research Center)
# Script Author: Melissa Rose 
# Script Date: 2022-05-19
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(utils)
require(readxl)
setwd('data/synthesis_database/2_contributed_data_raw/')

# create download directory 
download_directory <- 'dataset_09_heard_russia_kolyma/'
if (file.exists(download_directory)){
  print(paste('directory already exists:', download_directory))
  setwd(download_directory)
} else {
  print(paste('creating directory:', download_directory))
  dir.create(download_directory)
  setwd(download_directory)
}

# DOWNLOAD DATASET ==============================================================================================

# download master biomass spreadsheet and metadata
url <- 'https://arcticdata.io/metacat/d1/mn/v2/packages/application%2Fbagit-097/resource_map_doi%3A10.5065%2FD6NG4NP0'
download.file(url, destfile = 'data.zip', mode = 'wb')

unzip(zipfile = 'data.zip', files = 'data/TS_2012_2013_Master.xlsx', junkpaths = TRUE)
unzip(zipfile = 'data.zip', files = 'data/doi_10.5065_D6NG4NP0-METADATA.pdf', junkpaths = TRUE) 

unlink('data.zip')

