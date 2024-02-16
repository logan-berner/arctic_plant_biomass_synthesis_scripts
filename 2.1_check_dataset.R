# Description: This R script performs some basic checks of the synthesis dataset. 
# Date: 2023-08-14
# Author: Logan Berner, NAU
# Notes: 

# SET UP ================================================================================
rm(list=ls())
require(data.table)
require(readxl)

pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')

# SEPARATE COLUMNS BASED ON DATA TYPE (CHARACTOR, NUMERIC, OR LOGICAL) =============
str(pft.dt)

names(pft.dt)

pft.chr.dt <- pft.dt[ , .SD, .SDcols = is.character]
ncol(pft.chr.dt)

pft.num.dt <- pft.dt[ , .SD, .SDcols = is.numeric]
ncol(pft.num.dt)

pft.log.dt <- pft.dt[ , .SD, .SDcols = is.logical]
ncol(pft.log.dt)


# CHECK UNIQUE VALUES FOR ALL CATEGORICAL COLUMNS =====================================
pft.chr.lst <- list()
for(i in 1:ncol(pft.chr.dt)){
  pft.chr.lst[[i]] <- as.vector(unique(pft.chr.dt[, ..i]))
}

pft.chr.lst[c(1:5)]
pft.chr.lst[c(6:10)]
pft.chr.lst[c(11:16)]


# CALCULATE THE RANGE OF VALUES FOR ALL NUMERIC COLUMNS ===============================
pft.num.dt[ , .(range = lapply(.SD, function(x){paste0(min(x, na.rm = T), " - ", max(x, na.rm = T))})), .SDcols = names(pft.num.dt)]


# CHECK BIOMASS DENSITY OUTLIERS USING Z SCORES ======================================================
pft.dt[, biomass_density_zscore := scale(biomass_density_gm2), by = 'pft']

pft.anom.dt <- pft.dt[biomass_density_zscore > 3]

pft.anom.dt <- pft.anom.dt[, c('contributor','plot_code','pft','biomass_density_gm2','biomass_density_zscore')]
pft.anom.dt


# ADDITIONAL QUALITY CHECKS ==========================================================

# check that every plot has data for 5 pfts (expect for one dataset with total AGB)
n.pfts <- pft.dt[, .(n.pfts = .N), by = 'plot_code']
n.pfts[n.pfts < 5]
setorder(n.pfts, n.pfts)
table(n.pfts$n.pfts)


# summary of sample plot areas 
pft.dt[method != 'unmeasured', .(plot.area.min.m2 = min(plot_area_m2), 
                                 plot.area.med.m2 = median(plot_area_m2),
                                 plot.area.max.m2 = max(plot_area_m2)), by = pft]
table(pft.dt$plot_area_m2)


unmeas.dt <- pft.dt[method == 'unmeasured']
table(unmeas.dt$contributor)


# SPOT CHECKS ===================================================================
pft.dt[notes == 'lichen cover 5%']

# END SCRIPT ====================================================================