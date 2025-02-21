# filename:    analysis-potential.R
# created:     13 February 2025
# updated:     13 February 2025
# author:      S.C. McClelland
# description: This file conducts a one-sided bootstrap significance test for ccg-ntill
#              and ccl-ntill GHG response for the near-term (2016-2050.)
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(sf)
library(stats)
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
source('r/analysis-functions.R')
data_p = paste(dir, 'data/daycent-post-processed', sep ='/')
out_p  = paste(dir, 'data/analysis-output', sep = '/')
#-----------------------------------------------------------------------------------------
# LOAD DT & RASTER & SHP
#-----------------------------------------------------------------------------------------
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
  ## GHG estimates ##
{ ## ccg-ntill ##
  load(paste(data_p, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
  d_ccg_ntill_dt   = d_ccg_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O','s_GHG', 'd_s_SOC', 'd_s_dN2O', 
                                     'd_s_iN2O', 'd_s_N2O')]
  gc()
  d_ccg_ntill_i_dt = fread(paste(data_p, 'ccg-ntill-ghg-flux-imputed.csv', sep = '/'))
  d_ccg_ntill_i_dt = d_ccg_ntill_i_dt[, -c('IPCC_NAME')]
  d_ccg_ntill_dt   = rbind(d_ccg_ntill_dt, d_ccg_ntill_i_dt)
  rm(d_ccg_ntill_i_dt)
  d_ccg_ntill_dt[, rep := gsub("adj_", "", rep)]
  d_ccg_ntill_dt[, rep := gsub("unc_", "", rep)]
  gc()
}
{ ## ccl-ntill ##
  load(paste(data_p, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))
  d_ccl_ntill_dt   = d_ccl_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O','s_GHG', 'd_s_SOC', 'd_s_dN2O', 
                                         'd_s_iN2O', 'd_s_N2O')]
  gc()
  d_ccl_ntill_i_dt = fread(paste(data_p, 'ccl-ntill-ghg-flux-imputed.csv', sep = '/'))
  d_ccl_ntill_i_dt = d_ccl_ntill_i_dt[, -c('IPCC_NAME')]
  d_ccl_ntill_dt   = rbind(d_ccl_ntill_dt, d_ccl_ntill_i_dt)
  rm(d_ccl_ntill_i_dt)
  d_ccl_ntill_dt[, rep := gsub("adj_", "", rep)]
  d_ccl_ntill_dt[, rep := gsub("unc_", "", rep)]
  gc()
}
#-----------------------------------------------------------------------------------------
# ADD REGION & COMBINE DT
#-----------------------------------------------------------------------------------------
crop_area_dt = create_crop_dt(country.sf, crop_r)
#wide to long
crop_area_dt = melt(crop_area_dt,
     id.vars = c("gridid", "WB_NAME", "WB_REGION", "IPCC_NAME"),
     measure.vars = patterns("2015"),
     variable.name = "crop_type",
     value.name = c("hectares"))
setorder(crop_area_dt, gridid)
# align for join
crop_area_dt[crop_type %in% 'maize_rainfed_2015', crop := 'maiz'][crop_type %in% 'maize_rainfed_2015', irr := 0]
crop_area_dt[crop_type %in% 'maize_irrigated_2015', crop := 'maiz'][crop_type %in% 'maize_irrigated_2015', irr := 1]
crop_area_dt[crop_type %in% 'soybean_rainfed_2015', crop := 'soyb'][crop_type %in% 'soybean_rainfed_2015', irr := 0]
crop_area_dt[crop_type %in% 'soybean_irrigated_2015', crop := 'soyb'][crop_type %in% 'soybean_irrigated_2015', irr := 1]
crop_area_dt[crop_type %in% 'wheat_rainfed_2015', crop := 'swht'][crop_type %in% 'wheat_rainfed_2015', irr := 0]
crop_area_dt[crop_type %in% 'wheat_irrigated_2015', crop := 'swht'][crop_type %in% 'wheat_irrigated_2015', irr := 1]
crop_area_dt = crop_area_dt[!crop_type %in% 'sum_crop_2015']
crop_area_dt[, crop_type := NULL]
# join cropland area & IPCC Name
{ ## ccg-ntill
  d_ccg_ntill_dt   = d_ccg_ntill_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                   crop   = crop,
                                                                                   irr    = irr)]
  d_ccg_ntill_dt   = d_ccg_ntill_dt[!is.na(scenario),]
  gc()
}
{ ## ccl-ntill
  d_ccl_ntill_dt   = d_ccl_ntill_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                       crop   = crop,
                                                                                       irr    = irr)]
  d_ccl_ntill_dt   = d_ccl_ntill_dt[!is.na(scenario),]
  gc()
}
#-----------------------------------------------------------------------------------------
# MULTIPLY BY CROPLAND AREA, Mg CO2-eq
#-----------------------------------------------------------------------------------------
  ## ccg-ntill ##
d_ccg_ntill_dt[, t_d_s_GHG := d_s_GHG*hectares]
  ## ccl-ntill ##
d_ccl_ntill_dt[, t_d_s_GHG := d_s_GHG*hectares]
#-----------------------------------------------------------------------------------------
# ESTIMATE GHG & YIELD POTENTIAL
#-----------------------------------------------------------------------------------------
  ## global ##
ss   = 24 # number of climate variants
ci_l = 1.96 # 95% CI level
{ ## ccg-ntill ##
  # GHG
  global_ccg_ntill_r_dt  = d_ccg_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                      by = .(scenario, y_block, rep)]
}
{ ## ccl-ntill ##
  # GHG
  global_ccl_ntill_r_dt  = d_ccl_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(scenario, y_block, rep)]
}
#-----------------------------------------------------------------------------------------
# ESTIMATE BOOSTRAP MEAN DIFFERENCE
#-----------------------------------------------------------------------------------------
ss = 24 # independent samples
# create vectors
ccg_2050 = global_ccg_ntill_r_dt[y_block == 2050, t_d_s_GHG]
ccl_2050 = global_ccl_ntill_r_dt[y_block == 2050, t_d_s_GHG]

ccg_ccl_2050 = ccg_2050 - ccl_2050
ccg_ccl_2050 = data.table(diffs = ccg_ccl_2050)

B = 1001
mean_diff = rep(NA, B)
n = nrow(ccg_ccl_2050)
set.seed(11162024)
for(i in 1:B){
  w = sample(n,24, replace = TRUE)
  # this generates the indices we are selecting during the sample with replacement
  X_BT = ccg_ccl_2050$diffs[w]
  mean_diff[i] = mean(X_BT)
}
# One-sided 95% CI
quantile(mean_diff, 0.95)