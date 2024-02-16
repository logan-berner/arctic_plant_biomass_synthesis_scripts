# SCRIPT METADATA ==========================================================================================
# Description: R script to download biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Skip Walker (UAF)
# Script Author: Logan Berner
# Script Date: 2023-03-20
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(utils)
require(readxl)

# create download directory 
download_directory <- 'data/synthesis_database/2_contributed_data_raw/dataset_18_walker_northamerica_arctic/'
dir.create(download_directory)

# DOWNLOAD DATASET ==============================================================================================
url <- 'https://doi.pangaea.de/10.1594/PANGAEA.837761?format=zip'
download.file(url, destfile = paste0(download_directory,'data.zip'), mode = 'wb')

unzip(zipfile = paste0(download_directory,'data.zip'), 
      files = 'datasets/NA-Artic_vegetation2.tab', 
      exdir = download_directory, 
      junkpaths = TRUE)

unzip(zipfile = paste0(download_directory,'data.zip'), 
      files = 'datasets/NA-Artic_envir-soil.tab', 
      exdir = download_directory, 
      junkpaths = TRUE)

unlink(paste0(download_directory, 'data.zip'))
