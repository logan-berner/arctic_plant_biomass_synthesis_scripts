# SCRIPT METADATA ==========================================================================================
# Description: R script to standardize biomass harvest dataset for inclusion in synthesis database
# Dataset Contact: Ryan Danby (Queens University)
# Script Author: Logan Berner
# Script Date: 2023-06-26
# Dataset Notes: 
# - All sites have a 50x50cm “A” quadrat. 
# - Some sites also have an “S” quadrat. The S quadrat was for harvesting only tall shrubs (considered >30cm tall) when they were an important component of the vegetation at a site. These were Betula glandulosa, various species of tall willow (frequently Salix glauca and Salix pulchra), and occasionally Potentilla fruticosa. We split biomass from the S plots into leaves, live wood, and dead wood. 
# - The S quadrats were 100x100cm in 2007, but were shrunk to 50x50cm in 2008.

# SET UP ====================================================================================================
rm(list=ls())
require(readxl)
require(utils)
require(R.utils)
require(data.table)
require(tidyr)
require(dplyr)
require(sf); sf_use_s2(FALSE)
require(leaflet)
source('scripts/synthesis_database_functions.R')

# LOAD DATASET ==============================================================================================
database.tmplt.dt <- data.table(read_excel('data/synthesis_database/Arctic biomass database template v2.xlsx', sheet = 1))
harv.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_23_danby_canada_kluane/STEP_biomass_dryweights.xlsx', sheet = 1))
meta.dt <- data.table(read_excel('data/synthesis_database/2_contributed_data_raw/dataset_23_danby_canada_kluane/Danby_Arctic biomass data intake form.xlsx', sheet = 2))

# STANDARDIZE METADATA ======================================================================================

# grab select columns
meta.dt <- meta.dt[, c(3,4,5,7,9:15,18,19)]

meta.dt[, ":="(dataset_id = 'ds23',
               contributor = 'Danby R, Hik D, Saewan K',
               citation = 'Danby et al. Unpublished',
               citation_short = 'Danby et al. Unpub.',
               site_description = 'none',
               coord_type = 'plot')]

# set vegetation_description to none for a few plots where the info is missing
meta.dt[vegetation_description == 'NA', vegetation_description := 'none']

# stratify plots w/in sites by shrub tundra or mixed tundra
shrub.tundras <- c('Shrub birch - Willow','Low closed birch-willow shurb','Tall open willow scrub',
                   'Tall closed willow scrub','Tall closed willow scrub (with shrub birch)',
                   'Southwest facing shrub tundra at treeline','Dwarf birch thicket','Low open willow scrub',
                   'Low open willow scrub','Closed low shrub birch-willow shrub','North facing tundra at treeline')

meta.dt[vegetation_description %in% shrub.tundras, plot_veg := 'ShrubTundra']
meta.dt[vegetation_description %in% shrub.tundras == F, plot_veg := 'MixedTundra']
meta.dt[vegetation_description == 'none' & site_id == 'R22', plot_veg := 'ShrubTundra'] # this has to be shrubby for such high AGB

# drop meta data for the 'tall shrub' plots that are the same as 'all' plots, expect plot area
meta.dt <- meta.dt[plot_id != 'S']
  
# adjust site_id, plot_id, and locale
meta.dt <- data.table(separate(meta.dt, col = site_id, sep = 1, into = c('site_letter','plot_number')))

site.id.dt <- meta.dt[, .(site_id = unique(locale))]
site.id.dt[, site_letter := substr(site_id, 0,1)]

meta.dt <- site.id.dt[meta.dt, on = 'site_letter']
meta.dt[, plot_id := plot_number]
meta.dt[, plot_number := NULL]
meta.dt[, site_letter := NULL]

meta.dt[, locale := 'Kluane Lake']

# create site and plot codes
meta.dt[, site_code := paste(country, 'KluaneLake', paste(capitalize(toCamelCase(meta.dt$site_id)), plot_veg, sep='-'), sep='.')]
meta.dt[, plot_code := paste(country, 'KluaneLake', paste(capitalize(toCamelCase(meta.dt$site_id)), plot_veg, sep='-'), plot_id, sep='.')]

unique(meta.dt$site_code)
unique(meta.dt$plot_code)

meta.dt[, plot_veg := NULL]

# sort 
setorder(meta.dt, site_id, plot_id)

# STANDARDIZE BIOMASS MEASUREMENTS =============================================

# adjust site_id, plot_id, etc. 
harv.dt <- data.table(separate(harv.dt, col = Site, sep = 1, into = c('site_letter','plot_number')))
harv.dt <- site.id.dt[harv.dt, on = 'site_letter']
harv.dt[, site_letter := NULL]

harv.dt[, plot_id := plot_number]
harv.dt[, plot_number := NULL]

setnames(harv.dt, c('Plot Type','Measure'), c('plot_type','measure'))
harv.dt[, Quadrat := NULL]

setorder(harv.dt, site_id, plot_id)

# melt dataset into long format 
harv.dt <- melt(harv.dt, id.vars = c('site_id','plot_id','plot_type','measure'), variable.name = 'pft', value.name = 'biomass_dry_weight_g')

# standardize plant functional types 
harv.dt[, pft := tolower(as.character(pft))]
unique(harv.dt$pft)

harv.dt[pft == 'moss', pft := 'bryophyte']
harv.dt[pft %in% c('sedges','grasses','equisitum spp.','dominant forb','other forbs'), pft := 'herb']
harv.dt[pft %in% c('salix reticulata','salix polaris','salix arctica','salix short shrub','salix tall shrub','dryas octopetala',
                   'dryas integrifolia','cassiope tetragona','ledum palustre','rhododendron lapponicum','empetrum nigrum','arctostaphylos uva-ursi',
                   'arctostaphylos rubra','vaccinium uliginosum','vaccinium vitis-idaea','betula glandulosa','potentilla fruticosa'),
        pft := 'shrub']

harv.dt <- harv.dt[pft %in% c('mushroom','litter','standing dead') == F]

unique(harv.dt$pft)
check_pfts(harv.dt)

# join harvest and metadata
harv.dt <- meta.dt[harv.dt, on = c('site_id','plot_id')]

# adjust plot area for tall shrub plots that were 1 x 1 m in 2007, but all others were 0.5 x 0.5 m
harv.dt[plot_type == 'S' & year == 2007, plot_area_m2 := 1]
harv.dt[, plot_type := NULL]

# compute biomass density 
harv.dt[, biomass_density_gm2 := biomass_dry_weight_g / plot_area_m2]
dim(harv.dt)

# drop dead wood
harv.dt <- harv.dt[measure != 'Dead Wood']

# drop 'measurement' type column
harv.dt <- harv.dt[, measure := NULL]

# drop dry weight and re-calculate after b/c nested plots were used
harv.dt <- harv.dt[biomass_dry_weight_g != 0]
harv.dt <- harv.dt[, biomass_dry_weight_g := NULL]


# compute biomass density for each PFT while maintaining meta data  
value.cols <- paste(c('vegetation_description','biomass_density_gm2','plot_area_m2'), collapse='|')
grouping.cols <- grep(value.cols, colnames(harv.dt), invert = T, value = T)

setorder(harv.dt, site_id, plot_id, pft, plot_area_m2)

pft.dt <- harv.dt[, .(biomass_density_gm2 = sum(biomass_density_gm2),
                      vegetation_description = unique(na.omit(vegetation_description)),
                      plot_area_m2 = concat_rows(plot_area_m2)),
                  by = eval(grouping.cols)]

# deal with nested tall shrub and standard plot areas 
unique(pft.dt$plot_area_m2)
pft.dt[grep('1', plot_area_m2), ':='(plot_area_m2 = 1, notes = 'tall shrub harvest on 1 x 1 m plot and low shrub harvest on 0.5 x 0.5 m plot')]
pft.dt[plot_area_m2 != 1, plot_area_m2 := 0.25]
pft.dt[, plot_area_m2 := as.numeric(plot_area_m2)]

pft.dt[plot_code == 'Canada.KluaneLake.Outpost.06']

# back calculate biomass_dry_weight
pft.dt[, biomass_dry_weight_g := biomass_density_gm2 * plot_area_m2]

# expand data table so every pft occurs in every plot
pft.dt <- expand_missing_pfts(pft.dt)
dim(pft.dt)

# set tree biomass to zero on all plots (treeless tundra) and copy plot metadata
pft.dt[is.na(biomass_dry_weight_g), ':='(biomass_dry_weight_g = 0, biomass_density_gm2 = 0)]
pft.dt[pft == 'tree', method := 'survey']

fill_missing_metadata(pft.dt, fill.cols = 'vegetation_description', by = 'plot_code')
fill_missing_metadata(pft.dt, fill.cols = 'site_description', by = 'plot_code')
pft.dt[is.na(notes), notes := 'none']

pft.dt
dim(pft.dt)


# CHECK SITE LOCATIONS USING A SIMPLE MAP ======================================
pts.dt <- pft.dt[, .(lat = first(latitude), lon = first(longitude)), by = 'plot_code']
pts.sf <- st_as_sf(pts.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
pts.sf <- pts.sf %>% st_cast('POINT')

leaflet(pts.sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircleMarkers(label = ~as.character(plot_code)) %>% 
  addLabelOnlyMarkers(label = ~as.character(plot_code),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T)) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# CHECK THAT DATASET MATCHES SYNTHESIS DATABASE FORMAT ======================================================
check_columns(pft.dt, database.tmplt.dt)

# sort column order to match synthesis database 
setcolorder(pft.dt, colnames(database.tmplt.dt)) 

# WRITE OUTPUT ===============================================================================================
out.file <- 'data/synthesis_database/3_contributed_data_processed/dataset_23_danby_canada_kluane_standardized.csv'
fwrite(pft.dt, out.file)

# END SCRIPT =================================================================================================

smry.dt <- pft.dt[pft != 'lichen', .(biomass_density_gm2 = sum(biomass_density_gm2),
                      vegetation_description = first(vegetation_description)), 
                  by = c('site_code','plot_id')]

setorder(smry.dt, site_code, biomass_density_gm2)
smry.dt

unique(smry.dt$vegetation_description)


