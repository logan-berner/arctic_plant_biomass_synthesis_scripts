# Description: Thes R script compares  
# Date: 2023-10-30
# Author: Logan Berner, NAU
# Notes: 

# SET UP ================================================================================
rm(list=ls())
require(data.table)

pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')


# BIOMASS BY PFT FOR EACH BIOCLIMATE ZONE ======================================
plot.tot.dt <- pft.dt[community_measured == TRUE & pft != 'total', .(biomass_density_gm2 = sum(biomass_density_gm2)), by = c('site_code', 'plot_code','bioclim_zone')]
site.tot.dt <- plot.tot.dt[, .(pft = 'total', biomass_density_gm2 = mean(biomass_density_gm2)), by = c('site_code', 'bioclim_zone')]
site.avg.dt <- pft.dt[method != 'unmeasured', .(biomass_density_gm2 = mean(biomass_density_gm2)), by = c('site_code','pft','bioclim_zone')]
site.smry.dt <- rbind(site.avg.dt, site.tot.dt, fill = T)

# site.smry.dt <- site.smry.dt[biomass_density_gm2 > 0]

# set factors
site.smry.dt[, pft := factor(pft, levels = c('bryophyte','lichen','herb','shrub','tree','total'))]
site.smry.dt[, bioclim_zone := factor(bioclim_zone, levels = c('High Arctic','Low Arctic','Oroarctic','Subarctic'))]

# summarize biomass density
bioclim.smry.dt <- site.smry.dt[, .(n_sites = .N,
                                   biomass_density_gm2_median = round(median(biomass_density_gm2)),
                                   biomass_density_gm2_q25 = round(quantile(biomass_density_gm2, 0.25)),
                                   biomass_density_gm2_q75 = round(quantile(biomass_density_gm2, 0.75))),
                               by = c('bioclim_zone','pft')]

bioclim.smry.dt <- bioclim.smry.dt[, biomass_smry := paste0(biomass_density_gm2_median, ' (', biomass_density_gm2_q25, '-',biomass_density_gm2_q75,', n=',n_sites,')')]

setorder(bioclim.smry.dt, bioclim_zone, pft)
bioclim.smry.wide.dt <- dcast(bioclim.smry.dt, pft ~ bioclim_zone, value.var = 'biomass_smry')
bioclim.smry.wide.dt
fwrite(bioclim.smry.wide.dt, file = 'output/biomass_by_bioclim_zone_summary.csv')

# summarize biomass fraction
site.avg.dt <- site.avg.dt[pft != 'tree' & pft != 'total']
site.avg.dt <- site.avg.dt[, biomass_density_frac := biomass_density_gm2 / sum(biomass_density_gm2), by = 'site_code']

bioclim.frac.smry.dt <- site.avg.dt[, .(n_sites = .N,
                                   biomass_density_frac_median = round(median(biomass_density_frac, na.rm = T),2),
                                   biomass_density_frac_q25 = round(quantile(biomass_density_frac, 0.25, na.rm = T),2),
                                   biomass_density_frac_q75 = round(quantile(biomass_density_frac, 0.75, na.rm = T),2)),
                               by = c('bioclim_zone','pft')]

setorder(bioclim.frac.smry.dt, bioclim_zone, pft)
bioclim.frac.smry.wide.dt <- dcast(bioclim.frac.smry.dt, pft ~ bioclim_zone, value.var = 'biomass_density_frac_median')
bioclim.frac.smry.wide.dt
fwrite(bioclim.frac.smry.wide.dt, file = 'output/biomass_by_bioclim_zone_summary.csv')

bioclim.frac.wide.dt <- dcast(bioclim.frac.smry.dt, pft ~ bioclim_zone, value.var = 'biomass_density_frac_median')
bioclim.frac.wide.dt

# COMPUTE TOTAL AGB FROM SITES IN NORTH AMERICA AND GREENLAND FOR COMPARISON WITH GILMANOV AND OLCHEL 1995 ==================
plot.tot.dt <- pft.dt[community_measured == TRUE & country %in% c('USA','Canada','Greenland'), .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm = T)), by = c('site_code', 'plot_code')]
site.avg.dt <- plot.tot.dt[, .(biomass_density_gm2 = mean(biomass_density_gm2, na.rm=T)), by = site_code]
setorder(site.avg.dt, biomass_density_gm2)

site.avg.dt

length(site.avg.dt$biomass_density_gm2)
mean(site.avg.dt$biomass_density_gm2)
fivenum(site.avg.dt$biomass_density_gm2)


# RANGE IN TOTAT AGB ACROSS ALL FIELD SITES ====================================
plot.tot.dt <- pft.dt[community_measured == TRUE & pft != 'total', .(biomass_density_gm2 = sum(biomass_density_gm2)), by = c('site_code', 'plot_code','bioclim_zone')]
site.tot.dt <- plot.tot.dt[, .(pft = 'total', biomass_density_gm2 = mean(biomass_density_gm2)), by = c('site_code', 'bioclim_zone')]
setorder(site.tot.dt, biomass_density_gm2)
fivenum(site.tot.dt$biomass_density_gm2)
site.tot.dt
xx <- ecdf(site.tot.dt$biomass_density_gm2)
xx(6000)
site.tot.dt[biomass_density_gm2 > 6000]

# COMPUTE TOTAL AGB FOR SITES IN THE LOW AND HIGH ARCTIC TO COMPARE WITH WALKER ET AL. 2012 =================================
plot.tot.dt <- pft.dt[community_measured == TRUE & bioclim_zone %in% c("High Arctic","Low Arctic"), .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm = T)), by = c('site_code', 'plot_code')]
site.avg.dt <- plot.tot.dt[, .(biomass_density_gm2 = mean(biomass_density_gm2, na.rm=T)), by = site_code]
setorder(site.avg.dt, biomass_density_gm2)
site.avg.dt

length(site.avg.dt$biomass_density_gm2)
mean(site.avg.dt$biomass_density_gm2)
fivenum(site.avg.dt$biomass_density_gm2)

# Walker et al. 2012 Table S4 of total AGB
walker.agb <- data.table(agb = c(171,150,303,332,434,563,751,721,108,442,610,513,815),
                         zone = c('High','High','High','Low','Low','Low','Low','Low','High','High','Low','Low','Low'))
fivenum(walker.agb$agb)
mean(walker.agb$agb)
nrow(walker.agb)

walker.agb[, .(agb.min = min(agb), agb.max = max(agb), n = .N), by = zone]


# COMPUTE TOTAL NON-TREE BIOMASS TO COMPARE WITH WIELGOLASKI 1997 --------------
plot.tot.dt <- pft.dt[community_measured == TRUE & pft != 'tree' & country %in% c('Russia','Norway'), .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm = T)), by = c('site_code', 'plot_code')]
site.avg.dt <- plot.tot.dt[, .(biomass_density_gm2 = mean(biomass_density_gm2, na.rm=T)), by = site_code]
setorder(site.avg.dt, biomass_density_gm2)
site.avg.dt

length(site.avg.dt$biomass_density_gm2)
mean(site.avg.dt$biomass_density_gm2)
fivenum(site.avg.dt$biomass_density_gm2)

# Wielgolaski 1977
wiel.totb <- c(3374,1486,785,748,5878,2878,2786,1877,768,1744,1594,1254,696,158,2210,2265,821,485,205,910)
wiel.bgb <- c(2141,453,186,322,3716,2387,2307,1370,353,750,940,720,511,29,687,2105,673,216,148,561)

### only USSR AND NORWAY
wiel.totb <- c(5878,2878,2786,1877,1744,1594,1254,696,158,2210,2265,821,485,205)
wiel.bgb <- c(3716,2387,2307,1370,750,940,720,511,29,687,2105,673,216,148)
wiel.agb <- wiel.totb - wiel.bgb
fivenum(wiel.agb)
mean(wiel.agb)
length(wiel.agb)


# RAYNOLDS ET AL 2012 -----------------------------------------------------------
high.arctic.area <- c(100,446,1159)
high.arctic.agb <- c(106, 157,257)
sum((high.arctic.area/sum(high.arctic.area)) * (high.arctic.agb))

low.arctic.area <- c(1470,1804)
low.arctic.agb <- c(417,564)
sum((low.arctic.area/sum(low.arctic.area)) * (low.arctic.agb))



# END SCRIPT ===================================================================  
# plot.sum.dt <- pft.dt[community_measured == TRUE, .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm = T)), by = c('site_code', 'plot_code','gdd_degC','bioclim_zone')]
# site.avg.dt <- plot.sum.dt[, .(biomass_density_gm2 = mean(biomass_density_gm2, na.rm=T)), by = c('site_code','gdd_degC','bioclim_zone')]
# ggplot(site.avg.dt[bioclim_zone != "Subarctic"], aes(x = gdd_degC, y = biomass_density_gm2)) + 
#   geom_point()

# # summary for each PFT across sample plots
# pft.smry.dt <- pft.dt[pft != 'total' & method != 'unmeasured', 
#                       .(n.plots = .N, 
#                         min = min(biomass_density_gm2, na.rm = T),
#                         q10 = quantile(biomass_density_gm2, 0.10, na.rm = T),
#                         median = quantile(biomass_density_gm2, 0.5, na.rm = T),
#                         avg = mean(biomass_density_gm2, na.rm = T),
#                         q90 = quantile(biomass_density_gm2, 0.90, na.rm = T),
#                         max = max(biomass_density_gm2, na.rm = T)), 
#                       by = 'pft']
# 
# 
# tot.dt <- pft.dt[community_measured == TRUE, .(biomass_density_gm2 = sum(biomass_density_gm2)), by = plot_code]
# tot.smry.dt <- tot.dt[, .(pft = 'total', 
#                           n.plots = .N, 
#                           min = min(biomass_density_gm2, na.rm = T),
#                           q10 = quantile(biomass_density_gm2, 0.10, na.rm = T),
#                           median = quantile(biomass_density_gm2, 0.5, na.rm = T),
#                           avg = mean(biomass_density_gm2, na.rm = T),
#                           q90 = quantile(biomass_density_gm2, 0.90, na.rm = T),
#                           max = max(biomass_density_gm2, na.rm = T))] 
# 
# pft.smry.dt <- rbind(pft.smry.dt, tot.smry.dt)
# 
# pft.smry.dt[,2:6] <- round(pft.smry.dt[,2:6])
# 
# pft.smry.dt
# 
# fwrite(pft.smry.dt, 'output/biomass_by_pft_summary.csv')
# 
# pft.dt$vascular.measured
# 
# # vascular plant biomass
# vasc.plot.dt <- pft.dt[community_measured == TRUE & (pft == 'shrub' | pft == 'herb'), .(biomass_density_gm2 = sum(biomass_density_gm2)), by = c('site_code','plot_code')]
# vasc.smry.dt <- vasc.plot.dt[, .(min = min(biomass_density_gm2, na.rm = T),
#                                  median = median(biomass_density_gm2, na.rm = T),
#                                  avg = mean(biomass_density_gm2, na.rm = T),
#                                  q95 = quantile(biomass_density_gm2, 0.95, na.rm = T),
#                                  max = max(biomass_density_gm2, na.rm = T),
#                                  n.plots = .N)]
# 
# vasc.smry.dt
# 
# # non vascular plant biomass
# nonvasc.plot.dt <- pft.dt[community_measured == TRUE & (pft == 'bryophyte' | pft == 'moss'), .(biomass_density_gm2 = sum(biomass_density_gm2)), by = c('site_code','plot_code')]
# nonvasc.smry.dt <- nonvasc.plot.dt[, .(min = min(biomass_density_gm2, na.rm = T),
#                                        median = median(biomass_density_gm2, na.rm = T),
#                                        avg = mean(biomass_density_gm2, na.rm = T),
#                                        q95 = quantile(biomass_density_gm2, 0.95, na.rm = T),
#                                        max = max(biomass_density_gm2, na.rm = T),
#                                        n.plots = .N)]
# 
# nonvasc.smry.dt
# 
# 
# 
# vasc.site.dt <- vasc.plot.dt[, .(biomass_density_gm2_avg = mean(biomass_density_gm2)), by  = 'site_code']
# setorder(vasc.site.dt, biomass_density_gm2_avg)
# fivenum(vasc.site.dt$biomass_density_gm2)


# bioclim.smry.dt <- site.avg.dt[, .(n_sites = .N,
#                                    biomass_density_gm2_avg = round(median(biomass_density_gm2, na.rm=T)),
#                                    biomass_density_gm2_sd = round(sd(biomass_density_gm2, na.rm=T)), 
#                                    biomass_density_gm2_min = round(min(biomass_density_gm2, na.rm=T)),
#                                    biomass_density_gm2_max = round(max(biomass_density_gm2, na.rm=T))),
#                                by = c('bioclim_zone','pft')]
# 
# bioclim.smry.dt[, biomass_density_gm2_se := round(biomass_density_gm2_sd / sqrt(n_sites))]
# 
# bioclim.smry.dt <- bioclim.smry.dt[, biomass_combo := paste0(biomass_density_gm2_avg, "±", biomass_density_gm2_se, 
#                                                              ' (', biomass_density_gm2_min, '-',biomass_density_gm2_max,')')]


# # # SUMMARIZE PFT AGB FOR EACH BIOCLIMATIC ZONE ==================================

# 
# pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')
# plot.tot.dt <- pft.dt[community_measured == TRUE & pft != 'total', .(pft = 'total', biomass_density_gm2 = sum(biomass_density_gm2)), by = c('site_code', 'plot_code','bioclim_zone')]
# plot.dt <- rbind(pft.dt[method != 'unmeasured'], plot.tot.dt, fill=T)
# 
# # set factors
# plot.dt[, pft := factor(pft, levels = c('bryophyte','lichen','herb','shrub','tree','total'))]
# plot.dt[, bioclim_zone := factor(bioclim_zone, levels = c('High Arctic','Low Arctic','Oroarctic','Subarctic'))]
# 
# # plot.dt <- plot.dt[biomass_density_gm2 > 0]
# 
# # summarize
# bioclim.smry.dt <- plot.dt[, .(n_plots = .N,
#                                biomass_density_gm2_median = round(median(biomass_density_gm2)),
#                                biomass_density_gm2_q25 = round(quantile(biomass_density_gm2, 0.25)),
#                                biomass_density_gm2_q75 = round(quantile(biomass_density_gm2, 0.75))),
#                            by = c('bioclim_zone','pft')]
# 
# bioclim.smry.dt <- bioclim.smry.dt[, biomass_smry := paste0(biomass_density_gm2_median, ' (', biomass_density_gm2_q25, '-',biomass_density_gm2_q75,', n=',n_plots,')')]
# 
# setorder(bioclim.smry.dt, bioclim_zone, pft)
# bioclim.smry.wide.dt <- dcast(bioclim.smry.dt, pft ~ bioclim_zone, value.var = 'biomass_smry')
# bioclim.smry.wide.dt
# fwrite(bioclim.smry.wide.dt, file = 'output/biomass_by_bioclim_zone_summary.csv')
