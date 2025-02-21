# filename: impute-missing-uncertainty.R
# created:     18 December 2024
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file imputes missing grid cells based on climate variant data for yield responses
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
  ## shp file
country.sf      = st_read(paste(data_p, 'shp/WB_countries_Admin0_10m.shp', sep = '/'))
}
{
### These should be loaded and executed one at a time because of file size ###
  ## uncertainty dt ##
# gcm_dt = loadRData(paste(data_p, 'ccg-res-yield.RData', sep = '/'))
# gcm_dt = loadRData(paste(data_p, 'ccl-res-yield.RData', sep = '/'))
# gcm_dt = loadRData(paste(data_p, 'ccg-ntill-yield.RData', sep = '/'))
gcm_dt = loadRData(paste(data_p, 'ccl-ntill-yield.RData', sep = '/'))
}
#-----------------------------------------------------------------------------------------
# ADD REGIONS
#-----------------------------------------------------------------------------------------
gcm_dt     = add_region(gcm_dt, main_table, ipcc_region_dt)
gc()
missing_dt = add_region_missing(country.sf, crop_r)
gc()
setorder(missing_dt, gridid)
#-----------------------------------------------------------------------------------------
# IMPUTE
#-----------------------------------------------------------------------------------------
scen     = unique(gcm_dt[, scenario])
regions  = unique(missing_dt[,IPCC_NAME])
crops    = unique(gcm_dt[, crop])
irrs     = unique(gcm_dt[, irr])
y_blocks = unique(gcm_dt[, y_block])
gc()

for (r in regions) {
  for (c in crops) {
    for (ir in irrs) {
      known_locs  = unique(gcm_dt[crop %in% c & irr %in% ir & IPCC_NAME %in% r, gridid])
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
        dt_known     = gcm_dt[crop %in% c & irr %in% ir & y_block %in% y &
                                        IPCC_NAME %in% r,]
        imputation   = impute_location_gcm(dt_known, 24, 11162024)
        imputation   = round(imputation, digits = 2)
          for (g in impute_locs) {
          imputed_dt_g = data.table(gridid = g,
                                    crop   = c,
                                    irr    = ir,
                                    scenario = scen,
                                    ssp      = 'ssp370',
                                    gcm      = 'imputed',
                                    y_block  = y,
                                    IPCC_NAME   = r,
                                    rep         = as.vector(outer('gcm_rep_', 1:24,paste0)),
                                    d_s_cgrain  = imputation)
          print(paste0('Writing to file for ',r, ' ', c, ' ', ir, ' ', g, '.'))
          fwrite(imputed_dt_g, file = paste0(data_p, '/', scen, '-yield-imputed.csv'), append = TRUE)
        }
      }
    }
  }
}
#-----------------------------------------------------------------------------------------
# ADD MISSING GCM ITERATIONS
#-----------------------------------------------------------------------------------------
# computes estimate for gridid, crop, irr with less than 24 gcm
# estimate is the mean of gridid, crop, irr responses

# add iteration
gcm_dt[, iter := .GRP, by = gcm]

# add counts
gcm_dt[, count   := 1]
gcm_dt[, count_s := lapply(.SD, sum),
   .SDcols = 'count', 
   by = .(gridid, crop, scenario, irr, ssp, y_block)]

# create dt with < 24 gcm
c_gcm_dt = gcm_dt[count_s < 24]

# estimate mean response from actual gcm
m_gcm_dt = c_gcm_dt[, lapply(.SD, mean),
                    .SDcols = 'd_s_cgrain',
                    by = .(gridid, crop, scenario, irr, ssp, y_block, IPCC_NAME)]
setorder(m_gcm_dt, gridid, crop, irr, y_block)
# identify missing iterations / gcm
  # add gcms to header dt
h_gcm_dt = unique(c_gcm_dt[, c('gridid', 'crop', 'irr','y_block')])
h_gcm_dt[, key_r := 1]
g        = data.table(gcm = unique(gcm_dt[,gcm]),
                      key_r = 1)
h_gcm_dt = h_gcm_dt[g, on = "key_r", allow.cartesian = TRUE][, key_r := NULL]
setorder(h_gcm_dt, gridid, crop, irr, y_block)

# join to return missing gcm names
m_rep_gcm_dt = h_gcm_dt[!c_gcm_dt, on = .(gridid = gridid,
                               crop    = crop,
                               irr     = irr,
                               gcm     = gcm,
                               y_block = y_block)]
# join with mean estimates and other cols
m_gcm_dt = m_gcm_dt[m_rep_gcm_dt, on = .(gridid = gridid,
                                         crop = crop,
                                         irr = irr,
                                         y_block = y_block)]
# join with iteration number
m_gcm_dt = m_gcm_dt[unique(gcm_dt[, c('gcm','iter')]), on = 'gcm']
setorder(m_gcm_dt, gridid, crop, irr, y_block)

# make column for rep name
m_gcm_dt[, rep  := paste0('gcm_rep_',iter)]
m_gcm_dt[, iter := NULL]
m_gcm_dt[, gcm  := 'imputed']
# order columns to append
setcolorder(m_gcm_dt, c('gridid', 'crop', 'irr', 'scenario', 'ssp', 'gcm', 'y_block',  
                        'IPCC_NAME', 'rep'))

### Check that selected line matches scenario ###
# SAVE AND APPEND to imputed dt
# fwrite(m_gcm_dt, file = paste0(data_p, '/', 'ccg-res', '-yield-imputed.csv'), append = TRUE)
# fwrite(m_gcm_dt, file = paste0(data_p, '/', 'ccl-res', '-yield-imputed.csv'), append = TRUE)
# fwrite(m_gcm_dt, file = paste0(data_p, '/', 'ccg-ntill', '-yield-imputed.csv'), append = TRUE)
fwrite(m_gcm_dt, file = paste0(data_p, '/', 'ccl-ntill', '-yield-imputed.csv'), append = TRUE)
