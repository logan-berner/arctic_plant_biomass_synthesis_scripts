# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Miguel Villoslada (Univ. of Eastern Finland)
# Script Author: Melissa Rose, updated by Logan Berner
# Script Date: 2022-06-09, 2023-03-10
# Dataset Notes: 
# - update citation when paper published

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(data.table)
require(sf)
require(leaflet)
require(tidyr)
require(dplyr)
require(stringr)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
biomass.sf <- st_read('data/synthesis_database/2_contributed_data_raw/dataset_11_villoslada_finland_lapland/UEF_biomass_plots_lapland_2021/UEF_biomass_plots_lapland_2021.shp')
nonvasc.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_11_villoslada_finland_lapland/Miguel_Moss_biomass_Jauris104plots_2021_20220227.xlsx', sheet = 2))

# STANDARDIZE SITE DATA ======================================================================================

# convert northing and easting to latitude and longitude in decimal degrees
biomass.sf <- st_transform(biomass.sf, crs = "+proj=longlat +datum=WGS84") %>% 
  mutate(longitude = st_coordinates(st_centroid(.))[,1],
         latitude = st_coordinates(st_centroid(.))[,2])

# subset site and biomass data to relevant columns
biomass.dt <- data.table(biomass.sf)
site.dt <- biomass.dt[, c('plot', 'Country', 'Site', 'latitude', 'longitude', 'Date', 'MissingDat','Class')]
biomass.dt <- biomass.dt[, c('plot', 'totBetu_na', 'totSalix', 'totDecShru', 'totEvergSh', 'totEmpePhy', 'Forb', 'Graminoids')]

# subset non vascular data
nonvasc.dt <- nonvasc.dt[, c(1,5,6)]
names(nonvasc.dt) <- c('plot','lichen','moss')
nonvasc.dt <- nonvasc.dt[plot != 91 & plot != 100] # these two plots are missing vascular data

# round coordinates to 6 decimals
site.dt[, latitude := round(latitude, 6)]
site.dt[, longitude := round(longitude, 6)]

# add year, month, and day
site.dt <- separate(site.dt, 'Date', c('year', 'month', 'day'), sep = '-') %>% data.table()

# drop plot where all data is missing
unique(site.dt$MissingDat)
site.dt <- site.dt[is.na(MissingDat)]
site.dt <- site.dt[, MissingDat := NULL]

# change column names
setnames(site.dt, c('plot','Country','Site'), c('plot_id','country','site_id'))

# add dataset_id, contributor, country, locale, plot_area_m2, coord_type, citation, citation_short, and notes
site.dt[, ':='(dataset_id = 'ds11',
               contributor = 'Villoslada M, Kumpula T, Juutinen S, Ylanne H',
               locale = 'Javrresduottar',
               coord_type = 'plot',
               method = 'harvest',
               site_description = 'none',
               citation = 'Villoslada et al. 2023. Reindeer control over shrubification in subarctic wetlands: spatial analysis based on unoccupied aerial vehicle imagery. Remote Sensing in Ecology and Conservation 9:687-706',
               citation_short = 'Villoslada et al. 2023',
               notes = 'none')]

# change country name
site.dt[country == 'FIN', country := 'Finland']
site.dt[country == 'NOR', country := 'Norway']

# fill in missing country
site.dt[plot_id == 79, country := 'Finland']

# remove country abbreviation from site_id
site.dt[, site_id := sapply(strsplit(site.dt$site_id, split = '-'), getElement, 1)]

# subdivide each site using vegetation type
unique(site.dt$Class)
site.dt[Class == 'hummock top', Class := 'hummock']
site.dt[, Class := str_to_title(Class)]
site.dt[, site_id := paste(site_id, Class, sep = '-')]

site.dt[, vegetation_description := paste(Class, 'tundra', sep = ' ')]
site.dt <- site.dt[, Class := NULL]

# create site and plot codes
site.dt[, site_code := paste(country, locale, site_id, sep='.')]
site.dt[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]

# STANDARDIZE PFT DATA =======================================================================================

# pivot non vascular data to long format
nonvasc.dt <- pivot_longer(nonvasc.dt, -plot, names_to = 'pft', values_to = 'biomass_density_gm2')
nonvasc.dt <- data.table(nonvasc.dt)
nonvasc.dt[, biomass_dry_weight_g := biomass_density_gm2 * 0.00694]
nonvasc.dt[, biomass_density_gm2 := NULL]

# pivot vascular data to long format
harv.dt <- pivot_longer(biomass.dt, -plot, names_to = 'pft', values_to = 'biomass_dry_weight_g')
harv.dt <- data.table(harv.dt)

# combine vasc and non-vasc biomass
harv.dt <- rbind(harv.dt, nonvasc.dt)

# subset to plots in site.dt
dim(harv.dt)
harv.dt <- harv.dt[plot %in% site.dt$plot_id]
dim(harv.dt)

# standardize pfts
harv.dt[, pft := tolower(pft)]
unique(harv.dt$pft)

# standardizing pft names
harv.dt[pft == 'totbetu_na', pft := 'shrub']
harv.dt[pft == 'totsalix', pft := 'shrub'] 
harv.dt[pft == 'totdecshru', pft := 'shrub'] 
harv.dt[pft == 'totevergsh', pft := 'shrub'] 
harv.dt[pft == 'forb', pft := 'herb']
harv.dt[pft == 'graminoids', pft := 'herb']
harv.dt[pft == 'totempephy', pft := 'herb']
harv.dt[pft == 'moss', pft := 'bryophyte']
unique(harv.dt$pft)
check_pfts(harv.dt)

# compute biomass density for each pft  
value.cols <- 'biomass_dry_weight_g'

grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

pft.dt <- harv.dt[, .(biomass_dry_weight_g = sum(biomass_dry_weight_g)),
                  by = eval(grouping.cols)]
dim(harv.dt)
dim(pft.dt)

# change column names
setnames(pft.dt, 'plot','plot_id')

# combine site.dt and pft.dt
pft.dt <- site.dt[pft.dt, on = 'plot_id']
dim(pft.dt)

# specify plot area
pft.dt[pft == 'shrub' | pft == 'herb', plot_area_m2 := 0.25]
pft.dt[pft == 'bryophyte' | pft == 'lichen', plot_area_m2 := 0.00694]

# compute biomass density
pft.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]

# expand data table so every pft occurs in every plot (even if zero or NA)
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set tree biomass to 0 
pft.dt[pft == 'tree', c('biomass_density_gm2', 'biomass_dry_weight_g') := 0]
pft.dt[pft == 'tree', ':='(plot_area_m2 = 0.25, method = 'survey')]

# expand vegetation_description to all pfts in each plot
pft.dt <- pft.dt[, vegetation_description := unique(na.omit(vegetation_description)),
       by = 'plot_code']

# expand notes to all pfts in each plot
pft.dt <- fill_missing_metadata(pft.dt, 'notes', 'plot_code')

# lichen and moss data were missing from plots, so set these to unmeasured
pft.dt[is.na(biomass_dry_weight_g), method := 'unmeasured']

# specify that moss and lichens were harvested in sept, later than vasc harvest
pft.dt[pft == 'bryophyte' | pft == 'lichen', month := 9]

pft.dt[method == 'unmeasured', plot_area_m2 := NA]


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_code']

pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_code)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_code),
                      labelOptions = labelOptions(textsize = '20pxl',
                                                  noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_11_villoslada_finland_lapland_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================

View(pft.dt)
