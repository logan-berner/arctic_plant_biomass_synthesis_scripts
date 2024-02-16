# Description: This R script summarizes different aspects of the synthesis data set
# Date: 2023-08-14
# Author: Logan Berner, NAU
# Notes: 

# SET UP ================================================================================
rm(list=ls())
require(data.table)
require(readxl)

pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')
contributor.dt <- data.table(read_excel('data/synthesis_database/Coauthor information.xlsx', sheet = 1))

# CREATE TABLE SUMMARIZING INDIVIDUAL DATASETS =============================
dataset.smry.table <- pft.dt[, .(country = paste(sort(unique(country)), collapse = '/'),
                                 lat.avg = round(mean(latitude),2),
                                 lon.avg = round(mean(longitude),2),
                                 n.plots = length(unique(plot_code)),
                                 year.rng = paste(min(year), max(year), sep = '-')),
                                 #years = paste(sort(unique(year)), collapse = '/')),
                             by = c('citation_short')]

setorder(dataset.smry.table, country, citation_short)

dataset.smry.table

fwrite(dataset.smry.table, 'output/table1_data_source_smry.csv')


# total number of datasets
length(unique(pft.dt$citation_short))

# SUMMARY METRICS ========================================================= 

# number of unique measurements
nrow(pft.dt[method != 'unmeasured'])

# number of unique missing measurements
nrow(pft.dt[method == 'unmeasured'])

# number of unique sample plots
length(unique(pft.dt$plot_code))

# number of unique field sites
length(unique(pft.dt$site_code))

# number of unique countries
sort(unique(pft.dt$country))
length(unique(pft.dt$country))

# country of sample plots
contry.smry.dt <- pft.dt[, .(country = first(country)), by = c('plot_code')]
contry.smry.dt <- contry.smry.dt[, .N, by = 'country']
contry.smry.dt[, pcnt := round(N/sum(N)*100)]

# number of unique sample plots where every PFT was measured
length(unique(pft.dt[community_measured == T]$plot_code)) 
length(unique(pft.dt[community_measured == T]$plot_code)) / length(unique(pft.dt$plot_code))
 

# number of plots with unmeasured PFTs
pft.dt[method == 'unmeasured', .N, by = 'pft']

# number of plots where each PFT was measured
n.plots <- length(unique(pft.dt$plot_code))
pft.n.smry.dt <- pft.dt[method != 'unmeasured', .N, by = 'pft']
pft.n.smry.dt[, N.pcnt := round(N/n.plots*100)]
pft.n.smry.dt

# fraction of sample plots with plot- vs site-level coordinates 
round(table(pft.dt$coord_type)/nrow(pft.dt),2)

# year of sampling 
yr.smry.dt <- pft.dt[, .(year = first(year)), by = c('plot_code')]
fivenum(yr.smry.dt$year)

# SUMMARIZE DATA CONTRIBUTORS ==================================================

# number of data contributors
length(unique(contributor.dt$`First Name`)) + 1 # for Logan

# number of institutions that contributors are from
sort(unique(contributor.dt$Institution))
length(unique(contributor.dt$Institution))

# countries that contributors are from
sort(unique(contributor.dt$Country))
length(unique(contributor.dt$Country))

# SUMMARIZE COORDINATES (FOR ARCTIC DATA CENTER SUBMISSION) ====================
fivenum(pft.dt$latitude)
fivenum(pft.dt$longitude)


# END SCRIPT ===================================================================  


# pft.dt$biomass_dry_weight_g
# pft.dt$biomass_density_gm2
# pft.dt[, biomass_density_gm2 := round(biomass_dry_weight_g / plot_area_m2, 1)]
# pft.dt[biomass_dry_weight_g > 0 & biomass_dry_weight_g < 0.5 & biomass_density_gm2 == 0]

n.row.tot <- nrow(pft.dt[method != 'unmeasured'])

nrow(pft.dt[biomass_dry_weight_g > 0 & biomass_dry_weight_g < 0.5 & biomass_density_gm2 == 0]) / nrow(pft.dt)*100

table(pft.dt$method)
allometry.dt <- pft.dt[biomass_dry_weight_g > 0 & (method == 'survey' | method == 'harvest + survey') & (pft == 'tree' | pft == 'shrub') & citation_short != 'Siewert et al. 2015']

nrow(allometry.dt)/n.row.tot*100

table(allometry.dt$citation_short)

allometry.dt[citation_short == 'Walker et al. 2012']


pft.dt[, doy := yday(paste(year,month,day,sep='-'))]
pft.dt <- pft.dt[method != 'unmeasured']


nrow(pft.dt[doy > 195 & doy < 244])/nrow(pft.dt)*100
nrow(pft.dt[doy > 195])/nrow(pft.dt)*100
