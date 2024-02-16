# Description: These are custom functions used for creating the synthesis database

# CHECK WHETHER DATASET PFTS MATCH DATABASE ============================================
database.pfts <- c('herb','lichen', 'bryophyte', 'shrub', 'tree')

check_pfts <- function(dt){
  dt.pfts <- unique(dt$pft)
  if (setequal(dt.pfts, database.pfts) == T){
    print('Dataset PFTs match synthesis database')
  } else {
    pft.missing <- paste0(database.pfts[database.pfts %in% dt.pfts == F], collapse = ", ")
    print(paste0('*** Warning: Dataset missing ', pft.missing))
  }
}


# FILL MISSING PLANT FUNCTIONAL TYPES ===================================================
expand_missing_pfts <- function(dt){
  require(dplyr)
  require(data.table)
  require(zoo)
  
  # identify all factorial combinations of plot_code and pft
  all.combos <- expand.grid(unique(dt$plot_code), database.pfts)
  colnames(all.combos) <- c('plot_code','pft')
  
  # join missing plot_code x pft combos to data.table
  dt <- dt %>% as.data.frame %>% dplyr::right_join(all.combos) %>% as.data.table
  
  # sort
  dt <- setorder(dt, plot_code, pft)
  
  # identify columns to fill
  nonfill.cols <- c('notes','biomass_dry_weight_g','vegetation_description','biomass_density_gm2')
  fill.cols <- grep(paste(nonfill.cols, collapse = '|'), colnames(dt), invert = T, value = T)
  
  # fill first to last and then last to first within each group
  dt <- dt[, (fill.cols) := lapply(.SD, na.locf, na.rm = FALSE), by = plot_code, .SDcols = fill.cols]
  dt <- dt[, (fill.cols) := lapply(.SD, na.locf, fromLast = T, na.rm = FALSE), by = plot_code, .SDcols = fill.cols]
  
  # return data.table
  dt
}

# FILLS MISSING METADATA USING OTHER VALUES WITH A GROUP  ========================
fill_missing_metadata <- function(dt, fill.cols, by){
  require(zoo)
  dt <- dt[, (fill.cols) := lapply(.SD, na.locf, na.rm = FALSE), by = by, .SDcols = fill.cols]
  dt <- dt[, (fill.cols) := lapply(.SD, na.locf, fromLast = T, na.rm = FALSE), by = by, .SDcols = fill.cols]
  dt
}

# CONCATONATE ROWS FOR CREATING COMPOSITE VEGETATION DESCRIPTIONS ========================
concat_rows <- function(x){
  require(dplyr)
  gsub(', NA','', paste(first(x), nth(x, 2), nth(x, 3), nth(x, 4), nth(x, 5), sep = ', '))
}

# CHECK WHETHER DATASET COLUMNS MATCH DATABASE =======================================
check_columns <- function(dataset, database){
  dataset.cols <- colnames(dataset)
  database.cols <- colnames(database)
  if (setequal(dataset.cols, database.cols) == T){
    print('Dataset columns match synthesis database')
  } else {
    cols.missing <- paste0(database.cols[database.cols %in% dataset.cols == F], collapse = ", ")
    print('*** Warning: dataset columns do not match synthesis database ***')  
    print(paste0('dataset missing columns ', cols.missing))
  }
}

# CONVERT COLUMNS TO LOWERCASE ============================================================
columns_tolower <- function(dataset){
  dataset$dataset_id=tolower(dataset$dataset_id)
  dataset$site_id=tolower(dataset$site_id)
  dataset$plot_id=tolower(dataset$plot_id)
  dataset$coord_type=tolower(dataset$coord_type)
  dataset$pft=tolower(dataset$pft)
  dataset$method=tolower(dataset$method)
  dataset$vegetation_description=tolower(dataset$vegetation_description)
  dataset$site_description=tolower(enc2utf8(dataset$site_description))
  dataset$notes=tolower(dataset$notes)
  
  dataset[, site_code := paste(country, locale, site_id, sep='.')]
  dataset[, plot_code := paste(country, locale, site_id, plot_id, sep='.')]
  
  dataset
}

# END SCRIPT ==========================================================================