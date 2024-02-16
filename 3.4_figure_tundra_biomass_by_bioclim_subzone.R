# SET UP =======================================================================
rm(list=ls())
require(data.table)
require(terra)
require(sf)
require(smoothr)
require(ggplot2)
require(ggnewscale)
require(ggpubr)


# LOAD SPATIAL DATA SETS ======================================================
pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')

pft.dt <- pft.dt[pft != 'total']

pft.dt <- pft.dt[community_measured == T]

# average across sample plots at each field site
site.pft.dt <- pft.dt[, .(biomass_density_gm2 = mean(biomass_density_gm2)), by = c('site_code','pft','bioclim_zone')]

site.tot.dt <- pft.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm=T)), by = c('plot_code','site_code','bioclim_zone')]
site.tot.dt <- site.tot.dt[,  .(biomass_density_gm2 = sum(biomass_density_gm2, na.rm=T)), by = c('site_code','bioclim_zone')]

# create boxplots 
ggplot(site.pft.dt, aes(x = bioclim_zone, y = biomass_density_gm2, fill = pft)) + 
  geom_boxplot(outlier.shape = 1) +
  ylim(0,1000)

ggplot(site.tot.dt, aes(x = bioclim_zone, y = biomass_density_gm2)) + 
  geom_boxplot(outlier.shape = 1) +
  ylim(0,1500)


# fraction of biomass in each PFT ----------------------------------------------
site.pft.dt[, plot_biomass_total := sum(biomass_density_gm2), by = 'site_code']
site.pft.dt[, biomass_frac := biomass_density_gm2 / plot_biomass_total]

ggplot(site.pft.dt, aes(x = bioclim_zone, y = biomass_frac, fill = pft)) + 
  geom_boxplot(outlier.shape = 1)



# -------------------------------------------------------------------------------
# count number of plots in each CAVM vegetation type
plot.dt <- pft.dt[is.na(biomass_density_gm2) == F, .(bioclim_zone = first(bioclim_zone)), by = c('citation_short','plot_code')]

ggplot(plot.dt, aes(x = bioclim_zone, fill = citation_short)) +
  geom_histogram(color = 'gray20', binwidth = 1, stat = 'count') +
  labs(x = expression("Year of sampling"), 
       y = 'Number of sample plots', 
       fill = 'Data source') + 
  theme_bw() +  theme(legend.position = 'bottom', 
                      legend.direction="horizontal",
                      legend.text = element_text(size = 7),
                      legend.title=element_text(size=14),
                      axis.text=element_text(size=12), 
                      axis.title=element_text(size=14)) +  
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))

# ------------------------------------------------------------------------------

# summarize 
pft.smry.dt <- siet.pft.dt[, .(n_sites = .N,
                          biomass_density_gm2_avg = round(mean(biomass_density_gm2, na.rm=T)),
                          biomass_density_gm2_sd = round(sd(biomass_density_gm2, na.rm=T)),
                          biomass_frac_avg = round(mean(biomass_frac, na.rm=T),2),
                          biomass_frac_sd = round(sd(biomass_frac, na.rm=T),2)), by = c('bioclim_zone','pft')]
pft.smry.dt

ggplot(pft.smry.dt, aes(x = bioclim_zone, y = biomass_density_gm2_avg, fill = pft)) + 
  geom_bar(stat = "identity", color = 'black')
