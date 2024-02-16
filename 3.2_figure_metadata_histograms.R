# This R script creates histograms of showing year and country of sampling 
# Author: Logan Berner (NAU)
# Date: 2023-09-18

# SET UP ================================================================================
rm(list=ls())
require(data.table)
require(ggplot2)
require(ggpubr)
require(ggnewscale)
require(sf)

# LOAD DATA ================================================================================
pft.dt <- fread('data/synthesis_database/4_synthesis_dataset/arctic_tundra_biomass_synthesis_dataset.csv')

# PREPARE DATA FOR PLOTTING ====================================================
# calculate DOY
pft.dt[, doy := yday(paste(year,month,day,sep='-'))]

plot.dt <- pft.dt[method != 'unmeasured', .(year = min(year),
                                            doy = min(doy),
                                            country = first(country), 
                                            bioclim_zone = first(bioclim_zone)), by = c('citation_short','plot_code')]

plot.dt[, bioclim_zone := factor(bioclim_zone, levels = c('High Arctic','Low Arctic','Oroarctic','Subarctic'))]


n.plot.dt <- plot.dt[, .(n.plots = .N), by = bioclim_zone]
n.plot.dt[, n.plots.tot := sum(n.plots)]
n.plot.dt[, frac.plots := round(n.plots / n.plots.tot, 2)]
n.plot.dt

#  HISTOGRAM OF YEAR OF SAMPLING ==========================================
fig.yr <- ggplot(plot.dt, aes(x = year, fill = bioclim_zone)) +
  geom_histogram(color = 'gray20', binwidth = 1) +
  scale_fill_viridis_d(direction = -1) + 
  labs(x = expression("Year"), 
       y = 'Number of sample plots', 
       fill = 'Bioclimatic zone') + 
  theme_bw() +  theme(legend.position = c(0.5, 0.6), 
                      legend.direction="horizontal",
                      legend.text = element_text(size=12),
                      legend.title=element_text(size=14),
                      axis.text=element_text(size=12), 
                      axis.title=element_text(size=14)) +  
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))

fig.yr


#  HISTOGRAM OF DAY OF YEAR OF SAMPLING ==========================================
monthly.first.doy <- c(152,182,213,244,274)
months <- c('June','July','August','September')

fig.doy <- ggplot(plot.dt, aes(x = doy, fill = bioclim_zone)) +
  geom_histogram(color = 'gray20', binwidth = 5) +
  scale_fill_viridis_d(direction = -1) + 
  geom_vline(xintercept = monthly.first.doy, linetype="dotted", color = "gray50", size=1) + 
  annotate(geom = "text", x = monthly.first.doy[1:4]+15, y = 300, label = months, color = "gray50", angle = 0) + 
  labs(x = expression("Day of year"), 
       y = 'Number of sample plots', 
       fill = 'Bioclimatic zone') + 
  theme_bw() +  theme(axis.text=element_text(size=12), 
                      axis.title=element_text(size=14)) +  
  guides(fill = 'none')

fig.doy


#  HISTOGRAM OF COUNTRY OF SAMPLING ============================================
fig.country <- ggplot(plot.dt, aes(x = country, fill = bioclim_zone)) +
  geom_histogram(color = 'gray20', stat='count') +
  scale_fill_viridis_d(direction = -1) + 
  coord_flip() + 
  labs(x = expression("Country"), 
       y = 'Number of sample plots', 
       fill = 'Data source') + 
  theme_bw() +  theme(axis.text.x=element_text(size=12), 
                      axis.text.y=element_text(size=12),
                      axis.title=element_text(size=14)) + 
  guides(fill = 'none')

fig.country


#  HISTOGRAM OF BIOCLIMATIC ZONE OF SAMPLING ===================================
# set factor
plot.fact.dt <- plot.dt[, bioclim_zone := factor(bioclim_zone, rev(c('High Arctic','Low Arctic','Oroarctic','Subarctic')))]

fig.zone <- ggplot(plot.fact.dt, aes(x = bioclim_zone, fill = bioclim_zone)) +
  geom_histogram(color = 'gray20', stat='count') +
  scale_fill_viridis_d(direction = -1) + 
  coord_flip() + 
  labs(x = expression("Bioclimatic zone"), 
       y = 'Number of sample plots', 
       fill = 'Data source') + 
  theme_bw() +  theme(axis.text.x=element_text(size=12), 
                      axis.text.y=element_text(size=12),
                      axis.title=element_text(size=14)) + 
  guides(fill = 'none')

fig.zone

# COMBINE FIGURES ==============================================================
bioclim.legend <- get_legend(fig.yr)
fig.yr <- fig.yr + theme(legend.position = "none")

top.fig <- ggarrange(fig.zone, fig.country, labels=c('a','b'), align = 'h', 
                        ncol = 2, nrow = 1, label.x = 0.1, label.y = 1.05, 
                        font.label = list(size=18))


bottom.fig <- ggarrange(fig.yr, fig.doy, labels = c('c','d'), label.x = 0.1, label.y = 0.97, 
                        font.label = list(size=18), nrow = 2)

combo.fig <- ggarrange(top.fig, bottom.fig, align = 'v', heights = c(0.35,0.65), nrow = 2)

combo.fig

combo.fig.wlegend <- ggarrange(bioclim.legend, combo.fig,
                               nrow = 2, heights = c(0.10,0.90))

combo.fig.wlegend

ggsave('figures/fig2_sample_metadata.jpg', 
       width = 20, height = 20, units = 'cm', dpi = 400)


# END SCRIPT =================================================================== 