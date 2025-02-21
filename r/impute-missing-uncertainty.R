# filename: impute-missing-uncertainty.R
# created:     18 December 2024
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file imputes missing grid cells based on uncertainty data for GHG responses
#              only. This data is used when estimating global mitigation potential.
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(sf)
library(stringr)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES, FUNCTIONS & FILES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
source('r/impute-functions.R')
data_p = paste(dir, 'data/daycent-post-processed', sep ='/')
#-----------------------------------------------------------------------------------------
# LOAD DT & RASTER & SHP
#-----------------------------------------------------------------------------------------
{
  ## input table ##
load(paste(data_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep WB_NAME
main_table      = main_table[, c('gridid', 'WB_NAME')]
main_table      = unique(main_table)
  ## crop area dt ##
ipcc_region_dt  = readRDS(paste(data_p, 'cover-crop-crop-area-country-ipcc-region.rds', sep = '/'))
  ## cropland area raster 
crop_r          = rast(paste(data_p, 'msw-masked-cropland-rf-ir-area.tif', sep = '/'))
  ## shp file ##
country.sf      = st_read(paste(data_p, 'shp/WB_countries_Admin0_10m.shp', sep = '/'))
}
{
### These should be loaded and executed one at a time because of file size ###
  ## uncertainty dt ##
# uncertainty_dt = loadRData(paste(data_p, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
# uncertainty_dt = loadRData(paste(data_p, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
# uncertainty_dt = loadRData(paste(data_p, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
uncertainty_dt = loadRData(paste(data_p, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))
}
#-----------------------------------------------------------------------------------------
# ADD REGIONS
#-----------------------------------------------------------------------------------------
uncertainty_dt = add_region(uncertainty_dt, main_table, ipcc_region_dt)
gc()
missing_dt     = add_region_missing(country.sf, crop_r)
gc()
setorder(missing_dt, gridid)
#-----------------------------------------------------------------------------------------
# IMPUTE
#-----------------------------------------------------------------------------------------
scen     = unique(uncertainty_dt[, scenario])
regions  = unique(missing_dt[,IPCC_NAME])
crops    = unique(uncertainty_dt[, crop])
irrs     = unique(uncertainty_dt[, irr])
y_blocks = unique(uncertainty_dt[, y_block])
gc()

for (r in regions) {
  for (c in crops) {
    for (ir in irrs) {
      known_locs  = unique(uncertainty_dt[crop %in% c & irr %in% ir & IPCC_NAME %in% r, gridid])
      print(paste0('Imputing GHG for ', r, ' ', c, ' ', ir, '.'))
      if(c %in% 'maiz' & ir %in% 0) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   maize_rainfed_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else if (c %in% 'maiz' & ir %in% 1) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   maize_irrigated_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else if (c %in% 'soyb' & ir %in% 0) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   soybean_rainfed_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else if (c %in% 'soyb' & ir %in% 1) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   soybean_irrigated_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else if (c %in% 'swht' & ir %in% 0) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   wheat_rainfed_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else if (c %in% 'swht' & ir %in% 1) {
        impute_locs = missing_dt[!gridid %in% known_locs & IPCC_NAME %in% r & 
                                   wheat_irrigated_2015 > 0, gridid]
        print(paste0('Length of missing locations is ', length(impute_locs)))
      } else {
        print('This is not an acceptable entry for identifying missing locations. Stopping')
        stop()
      }
        for (y in y_blocks) {
        # impute location
        # same value across entire region for each y_block
        dt_known     = uncertainty_dt[crop %in% c & irr %in% ir & y_block %in% y &
                                        IPCC_NAME %in% r,]
        imputation   = impute_location(dt_known, 500, 11162024)
        imputation   = round(imputation, digits = 2)
          for (g in impute_locs) {
            imputed_dt_g = data.table(gridid = g,
                                      crop   = c,
                                      irr    = ir,
                                      scenario = scen,
                                      ssp      = 'ssp370',
                                      gcm      = 'imputed',
                                      y_block  = y,
                                      IPCC_NAME = r,
                                      rep      = as.vector(outer('unc_rep_', 1:500,paste0)),
                                      d_s_GHG  = imputation)
            print(paste0('Writing to file for ',r, ' ', c, ' ', ir, ' ', g, '.'))
            fwrite(imputed_dt_g, file = paste0(data_p, '/', scen, '-ghg-flux-imputed.csv'), append = TRUE)
        }
      }
    }
  }
}
