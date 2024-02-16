# This R script creates histograms of tundar biomass by plant functional type
# Author: Logan Berner (NAU)
# Date: 2023-03-28

# SET UP ================================================================================
rm(list=ls())
require(data.table)
require(ggplot2)
require(ggnewscale)
require(ggpubr)
require(dplyr)

# LOAD DATA ================================================================================
pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')
tot.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_total_plant_biomass_synthesis_dataset.csv')

# PREPARE DATA FOR PLOTTING ====================================================
pft.dt <- pft.dt[method != 'unmeasured', c('bioclim_zone','site_code','pft','biomass_density_gm2')]
tot.dt <- tot.dt[, c('bioclim_zone','site_code','plant_biomass_density_gm2')]
setnames(tot.dt, 'plant_biomass_density_gm2', 'biomass_density_gm2')
tot.dt[, pft := 'total']

pft.dt <- rbind(pft.dt, tot.dt)

# set factor levels 
pft.dt[, pft := factor(pft, levels = c('bryophyte','lichen','herb','shrub','tree','total'))]
pft.dt[, bioclim_zone := factor(bioclim_zone, levels = c('High Arctic','Low Arctic','Oroarctic','Subarctic'))]
pft.dt$bioclim_zone <- droplevels(pft.dt$bioclim_zone)

# determine total number of samples plots by PFT, and number of sample plots with 0 g/m2
pft.n.dt <- pft.dt[, .(n.plots = .N), by = 'pft']
pft.n.dt[, n.plots.0gm2 := pft.dt[biomass_density_gm2 == 0, .(n.plots = .N), by = 'pft'][,2]]

pft.n.sites.dt <- pft.dt %>% data.frame() %>% group_by(pft) %>% summarise(n.sites = n_distinct(site_code))

pft.dt <- pft.dt %>% data.table()

# identify and trim upper quantile
pft.dt[, q.thresh := quantile(biomass_density_gm2, 0.95, na.rm=T), by = pft]
pft.dt <- pft.dt[biomass_density_gm2 < q.thresh]


#  HISTOGRAM OF BIOMASS DENSITY FOR EACH PFT ===================================
fig <- ggplot() +
  geom_histogram(data = pft.dt[biomass_density_gm2 != 0], aes(x = biomass_density_gm2, fill = bioclim_zone), color = 'gray20') +
  scale_fill_viridis_d() + 
  facet_wrap(~pft, scales = 'free') + 
  labs(x = expression("Aboveground biomass (g m"^-2*")"), 
       y = 'Number of sample plots', 
       fill = 'Bioclimatic zone') + 
  theme_bw() +  theme(legend.position = 'bottom', 
                      legend.direction="horizontal",
                      legend.text = element_text(size=12),
                      legend.title=element_text(size=14),
                      axis.text=element_text(size=12), 
                      axis.title=element_text(size=16),
                      strip.text.x = element_text(size = 14)) + 
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))

fig 

# add number of sample plots and field sites for each PFT
# -- after updating ggplot2, this section started adding an NA value to the legend
# -- so this is a workaround... 
bioclim.legend <- get_legend(fig)
fig <- fig + theme(legend.position = "none")
fig

fig <- fig + geom_text(data = pft.n.dt, aes(x = Inf, y = Inf, fill = NA, label = paste0('n = ', n.plots, ' sample plots')), 
                       color = 'black', size = 4, vjust = 1.7, hjust = 1.05) + 
  geom_text(data = pft.n.sites.dt, aes(x = Inf, y = Inf, fill = NA, label = paste0('n = ', n.sites, ' field sites')),
            color = 'black', size = 4, vjust = 3.5, hjust = 1.2)

fig

fig.wlegend <- ggarrange(bioclim.legend, fig, nrow = 2, heights = c(0.15, 0.85))
fig.wlegend

ggsave('figures/fig4_tundra_biomass_density_pft_hist.jpg', 
       width = 20, height = 15, units = 'cm', dpi = 400)

# END SCRIPT =================================================================== 
