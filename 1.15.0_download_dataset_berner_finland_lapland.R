# SCRIPT METADATA ==========================================================================================
# Description: R script to download biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Logan BErner (NAU)
# Script Author: Logan Berner
# Script Date: 2023-02-02
# Dataset Notes: 
# - 

# SET UP ====================================================================================================
rm(list=ls())
require(utils)
require(readxl)

# create download directory 
download_directory <- 'data/synthesis_database/2_contributed_data_raw/dataset_15_berner_finland_lapland/'
dir.create(download_directory)

# DOWNLOAD DATASET ==============================================================================================
url <- 'https://arcticdata.io/metacat/d1/mn/v2/packages/application%2Fbagit-1.0/resource_map_doi%3A10.18739%2FA2QV3C526'
download.file(url, destfile = paste0(download_directory,'data.zip'), mode = 'wb')

unzip(zipfile = paste0(download_directory,'data.zip'), 
      files = 'data/1_plot_metadata.csv', 
      exdir = download_directory, 
      junkpaths = TRUE)

unzip(zipfile = paste0(download_directory,'data.zip'), 
      files = 'data/2_plot_biomass_summaries.csv', 
      exdir = download_directory, 
      junkpaths = TRUE)

unlink(paste0(download_directory, 'data.zip'))

