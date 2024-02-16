# DESCRIPTION ==================================================================
# This R script generates a two-panel figure showing tundra harvest field sites
# in (1) geographic space and (2) climate space. 
# Author: Logan Berner, NAU
# Date: 2022-06-08
# Notes: 

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
arctic.sf <- read_sf('data/gis_data/boundaries/arctic_oroarctic_laea.shp')
bioclim.sf <- read_sf('data/gis_data/boundaries/northern_bioclimatic_zones_laea.shp')
land.sf <- read_sf('data/gis_data/boundaries/land_45n_wgs84.shp')
site.sf <- read_sf('data/gis_data/field_locations/arctic_tundra_biomass_synthesis_sites.gpkg')
ice.sf <-  read_sf('A:/research/data/landcover/Arctic_Vegetation_Maps_1323/data/aga_circumpolar_geobotanical_2003/aga_circumpolar_geobotanical_2003.shp')

gdd.r <- rast('data/gis_data/climate/CHELSA_gdd0_1981-2010_arctic_laea_V.2.1.tif')
mat.r <- rast('data/gis_data/climate/CHELSA_bio1_1981-2010_arctic_laea_V.2.1.tif')
ppt.r <- rast('data/gis_data/climate/CHELSA_bio12_1981-2010_arctic_laea_V.2.1.tif')
  
# PREPARE SPATIAL DATASETS ====================================================
sf_use_s2(F)
laea <- ("+proj=laea +x_0=0 +y_0=0 +lon_0=180 +lat_0=90")
bbox <- c(xmin = -180, xmax = 180, ymin = 55, ymax = 83.62313)

ice.sf <- ice.sf %>% filter(LAND == 1)

site.sf <- site.sf %>% st_transform(laea)

arctic.sf <- arctic.sf %>% 
  st_transform(4326) %>% 
  st_crop(bbox) %>%
  st_transform(laea)

bioclim.sf <- bioclim.sf %>%
  st_transform(4326) %>%
  st_crop(bbox) %>%
  smoothr::densify(max_distance = 10) %>% # add vertices so projection works
  st_transform(laea)

land.sf <- land.sf %>%
  st_geometry() %>%
  st_crop(bbox) %>%
  smoothr::densify(max_distance = 10) %>% # add vertices so projection works
  st_transform(crs = laea)


# convert sf to vectors for raster data extraction
arctic.vec <- vect(st_geometry(arctic.sf))

arctic.dt <- data.table(terra::extract(mat.r, arctic.vec),
                        terra::extract(gdd.r, arctic.vec)[,2], 
                        terra::extract(ppt.r, arctic.vec)[,2])

names(arctic.dt) <- c('cell_id','mat_degC','gdd_degC','map_mm')

arctic.dt <- arctic.dt[complete.cases(arctic.dt),]
# arctic.dt <- arctic.dt[bioclim_zone != 4]

# CREATE MAP ==================================================================
site.map <- ggplot() + 
  geom_sf(data=land.sf, fill = "gray70", color = NA) +
  geom_sf(data=ice.sf, fill = "snow2", color = "gray50", size = NA) +
  geom_sf(data=bioclim.sf, aes(fill = Zone), color = NA, size = 0.05) +
  scale_fill_viridis_d() + 
  geom_sf(data=site.sf, fill = 'magenta', color = 'white', size = 2, pch = 21, alpha = 0.75) +
  coord_sf(expand = F, ndiscr = 10, label_axes = waiver()) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=14),
        legend.position="bottom",
        legend.direction="horizontal") + 
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5, 
                             override.aes = list(size = 5))) + 
  labs(fill = 'Bioclimatic zone', color = 'Bioclimatic zone')

site.map

ggsave('figures/fig1_tundra_site_map.jpg', width = 20, height = 20, units = 'cm', dpi = 400)


# CREATE CLIMATE SPACE FIGURE =======================================
xlims <- c(quantile(arctic.dt$gdd_degC, 0), quantile(arctic.dt$gdd_degC, 0.975))
ylims <- c(quantile(arctic.dt$map_mm, 0), quantile(arctic.dt$map_mm, 0.975))

clim.fig <- ggplot(arctic.dt, aes(gdd_degC, map_mm)) +
  geom_hex(bins = 30) + 
  scale_fill_gradient(low="snow2", high="gray30", limits=c(1000, NA), na.value = NA) +
  labs(x = expression('Mean annual growing degree days ('*degree*'C)'),
       y = 'Mean annual precipitation (mm)',
       fill = expression('Area (km'^2*')')) +
  new_scale_fill() + 
  geom_point(data = site.sf, mapping = aes(gdd_degC, map_mm, fill = bioclim_zone), 
             pch = 21, size = 2, color = 'white') +
  scale_fill_viridis_d(alpha = 0.75) + 
  # scale_color_viridis_d() + 
  guides(fill = 'none', color = 'none') +
  lims(x = xlims, y=ylims) + 
  theme_bw() + 
  theme(legend.position = c(0.65, 0.90),
        legend.title = element_text(size=8), 
        legend.text = element_text(size=6),
        legend.direction="horizontal",
        legend.key.height = unit(0.5,"cm"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

clim.fig

ggsave('figures/fig1b_tundra_site_climate.jpg', width = 7, height = 6, units = 'in', dpi = 400)


# COMBINE SITE MAP AND CLIMATE FIGURES =========================================
bioclim.legend <- get_legend(site.map)

site.map <- site.map + theme(legend.position = "none")

combo.fig <- ggarrange(site.map, clim.fig, labels=c('a','b'), align = 'h', 
                       ncol = 2, nrow = 1, label.x = c(0.24,0.22), label.y = 0.97, 
                       font.label = list(size=18))

combo.wlegend.fig <- ggarrange(bioclim.legend, combo.fig, nrow = 2, heights = c(0.15,0.85))
combo.wlegend.fig

ggsave('figures/fig1_tundra_sites.jpg', width = 21, height = 12, units = 'cm', dpi = 400)

# SUMMARIZE CLIMATE FOR THE BIOME AND SAMPLE SITES =============================

# temperature
fivenum(arctic.dt$mat_degC)
mean(arctic.dt$mat_degC)
sd(arctic.dt$mat_degC)

fivenum(site.clim.dt$mat_degC)
mean(site.clim.dt$mat_degC)
sd(site.clim.dt$mat_degC)

# precipitation
fivenum(arctic.dt$map_mm)
mean(arctic.dt$map_mm)
sd(arctic.dt$map_mm)

fivenum(site.clim.dt$map_mm)
mean(site.clim.dt$map_mm)
sd(site.clim.dt$map_mm)

# END SCRIPT ===================================================================