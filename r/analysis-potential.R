# filename:    analysis-potential.R
# created:     06 March 2023
# updated:     17 December 2024
# author:      S.C. McClelland
# description: This file estimates cumulative mean and standard deviations GHG mitigation 
#              and crop yield potential from DayCent uncertainty and climate variance output,
#              respectively. These data included imputed estimates for arriving at total potentials
#              over all cropland area.
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
{ ## ccg-res ##
  load(paste(data_p, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
  d_ccg_res_dt   = d_ccg_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O','s_GHG', 'd_s_SOC', 'd_s_dN2O', 
                                     'd_s_iN2O', 'd_s_N2O')]
  gc()
  d_ccg_res_i_dt = fread(paste(data_p, 'ccg-res-ghg-flux-imputed.csv', sep = '/'))
  d_ccg_res_i_dt = d_ccg_res_i_dt[, -c('IPCC_NAME')]
  d_ccg_res_dt   = rbind(d_ccg_res_dt, d_ccg_res_i_dt)
  rm(d_ccg_res_i_dt)
  d_ccg_res_dt[, rep := gsub("adj_", "", rep)]
  d_ccg_res_dt[, rep := gsub("unc_", "", rep)]
  gc()
}
{ ## ccl-res ##
  load(paste(data_p, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
  d_ccl_res_dt   = d_ccl_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O','s_GHG', 'd_s_SOC', 'd_s_dN2O', 
                                     'd_s_iN2O', 'd_s_N2O')]
  gc()
  d_ccl_res_i_dt = fread(paste(data_p, 'ccl-res-ghg-flux-imputed.csv', sep = '/'))
  d_ccl_res_i_dt = d_ccl_res_i_dt[, -c('IPCC_NAME')]
  d_ccl_res_dt   = rbind(d_ccl_res_dt, d_ccl_res_i_dt)
  rm(d_ccl_res_i_dt)
  d_ccl_res_dt[, rep := gsub("adj_", "", rep)]
  d_ccl_res_dt[, rep := gsub("unc_", "", rep)]
  gc()
}
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
  ## Yield estimates ##
{ ## ccg-res ##
  load(paste(data_p, 'ccg-res-yield.RData', sep = '/'))
  d_ccg_res_y_dt    = d_ccg_res_y_dt[, -c('s_cgrain')]
  d_ccg_res_y_dt[, gcm_n := gcm]
  d_ccg_res_y_dt[, gcm_n := as.integer(as.factor(gcm_n))]
  d_ccg_res_y_dt[, rep   := paste0('rep_', gcm_n)]
  d_ccg_res_y_dt[, gcm_n := NULL]
  setcolorder(d_ccg_res_y_dt, c('gridid', 'crop', 'irr', 'ssp', 'gcm', 'y_block', 'rep'))
  gc()
  d_ccg_res_y_i_dt   = fread(paste(data_p, 'ccg-res-yield-imputed.csv', sep = '/'))
  d_ccg_res_y_i_dt   = d_ccg_res_y_i_dt[, -c('IPCC_NAME')]
  d_ccg_res_y_i_dt[, rep := gsub("gcm_", "", rep)]
  d_ccg_res_y_dt     = rbind(d_ccg_res_y_dt, d_ccg_res_y_i_dt)
  rm(d_ccg_res_y_i_dt)
  gc()
}
{ ## ccl-res ##
  load(paste(data_p, 'ccl-res-yield.RData', sep = '/'))
  d_ccl_res_y_dt    = d_ccl_res_y_dt[, -c('s_cgrain')]
  d_ccl_res_y_dt[, gcm_n := gcm]
  d_ccl_res_y_dt[, gcm_n := as.integer(as.factor(gcm_n))]
  d_ccl_res_y_dt[, rep   := paste0('rep_', gcm_n)]
  d_ccl_res_y_dt[, gcm_n := NULL]
  setcolorder(d_ccl_res_y_dt, c('gridid', 'crop', 'irr', 'ssp', 'gcm', 'y_block', 'rep'))
  gc()
  d_ccl_res_y_i_dt   = fread(paste(data_p, 'ccl-res-yield-imputed.csv', sep = '/'))
  d_ccl_res_y_i_dt   = d_ccl_res_y_i_dt[, -c('IPCC_NAME')]
  d_ccl_res_y_i_dt[, rep := gsub("gcm_", "", rep)]
  d_ccl_res_y_dt     = rbind(d_ccl_res_y_dt, d_ccl_res_y_i_dt)
  rm(d_ccl_res_y_i_dt)
  gc()
}
{ ## ccg-ntill ##
  load(paste(data_p, 'ccg-ntill-yield.RData', sep = '/'))
  d_ccg_ntill_y_dt    = d_ccg_ntill_y_dt[, -c('s_cgrain')]
  d_ccg_ntill_y_dt[, gcm_n := gcm]
  d_ccg_ntill_y_dt[, gcm_n := as.integer(as.factor(gcm_n))]
  d_ccg_ntill_y_dt[, rep   := paste0('rep_', gcm_n)]
  d_ccg_ntill_y_dt[, gcm_n := NULL]
  setcolorder(d_ccg_ntill_y_dt, c('gridid', 'crop', 'irr', 'ssp', 'gcm', 'y_block', 'rep'))
  gc()
  d_ccg_ntill_y_i_dt   = fread(paste(data_p, 'ccg-ntill-yield-imputed.csv', sep = '/'))
  d_ccg_ntill_y_i_dt   = d_ccg_ntill_y_i_dt[, -c('IPCC_NAME')]
  d_ccg_ntill_y_i_dt[, rep := gsub("gcm_", "", rep)]
  d_ccg_ntill_y_dt     = rbind(d_ccg_ntill_y_dt, d_ccg_ntill_y_i_dt)
  rm(d_ccg_ntill_y_i_dt)
  gc()
}
{ ## ccl-ntill ##
  load(paste(data_p, 'ccl-ntill-yield.RData', sep = '/'))
  d_ccl_ntill_y_dt    = d_ccl_ntill_y_dt[, -c('s_cgrain')]
  d_ccl_ntill_y_dt[, gcm_n := gcm]
  d_ccl_ntill_y_dt[, gcm_n := as.integer(as.factor(gcm_n))]
  d_ccl_ntill_y_dt[, rep   := paste0('rep_', gcm_n)]
  d_ccl_ntill_y_dt[, gcm_n := NULL]
  setcolorder(d_ccl_ntill_y_dt, c('gridid', 'crop', 'irr', 'ssp', 'gcm', 'y_block', 'rep'))
  gc()
  d_ccl_ntill_y_i_dt   = fread(paste(data_p, 'ccl-ntill-yield-imputed.csv', sep = '/'))
  d_ccl_ntill_y_i_dt   = d_ccl_ntill_y_i_dt[, -c('IPCC_NAME')]
  d_ccl_ntill_y_i_dt[, rep := gsub("gcm_", "", rep)]
  d_ccl_ntill_y_dt     = rbind(d_ccl_ntill_y_dt, d_ccl_ntill_y_i_dt)
  rm(d_ccl_ntill_y_i_dt)
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
{ ## ccg-res
  d_ccg_res_dt   = d_ccg_res_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                 crop   = crop,
                                                                                 irr    = irr)]
  d_ccg_res_dt   = d_ccg_res_dt[!is.na(scenario),]
  gc()
  d_ccg_res_y_dt = d_ccg_res_y_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                    crop   = crop,
                                                                                    irr    = irr)]
  d_ccg_res_y_dt = d_ccg_res_y_dt[!is.na(scenario)]
  gc()
}
{ ## ccl-res
  d_ccl_res_dt   = d_ccl_res_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                   crop   = crop,
                                                                                   irr    = irr)]
  d_ccl_res_dt   = d_ccl_res_dt[!is.na(scenario),]
  gc()
  d_ccl_res_y_dt = d_ccl_res_y_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                     crop   = crop,
                                                                                     irr    = irr)]
  d_ccl_res_y_dt = d_ccl_res_y_dt[!is.na(scenario)]
  gc()
}
{ ## ccg-ntill
  d_ccg_ntill_dt   = d_ccg_ntill_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                   crop   = crop,
                                                                                   irr    = irr)]
  d_ccg_ntill_dt   = d_ccg_ntill_dt[!is.na(scenario),]
  gc()
  d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                     crop   = crop,
                                                                                     irr    = irr)]
  d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[!is.na(scenario)]
  gc()
}
{ ## ccl-ntill
  d_ccl_ntill_dt   = d_ccl_ntill_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                       crop   = crop,
                                                                                       irr    = irr)]
  d_ccl_ntill_dt   = d_ccl_ntill_dt[!is.na(scenario),]
  gc()
  d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                         crop   = crop,
                                                                                         irr    = irr)]
  d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[!is.na(scenario)]
  gc()
}
#-----------------------------------------------------------------------------------------
# MULTIPLY BY CROPLAND AREA, Mg CO2-eq
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_dt[, t_d_s_GHG := d_s_GHG*hectares]
d_ccg_res_y_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
  ## ccl-res ##
d_ccl_res_dt[, t_d_s_GHG := d_s_GHG*hectares]
d_ccl_res_y_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
  ## ccg-ntill ##
d_ccg_ntill_dt[, t_d_s_GHG := d_s_GHG*hectares]
d_ccg_ntill_y_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
  ## ccl-ntill ##
d_ccl_ntill_dt[, t_d_s_GHG := d_s_GHG*hectares]
d_ccl_ntill_y_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
#-----------------------------------------------------------------------------------------
# ESTIMATE GHG & YIELD POTENTIAL
#-----------------------------------------------------------------------------------------
  ## global ##
{ ## ccg-res ##
# GHG
  global_ccg_res_r_dt  = d_ccg_res_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                       by = .(scenario, y_block, rep)]
  global_ccg_res_m_dt  = global_ccg_res_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                            by = .(scenario, y_block)]
  global_ccg_res_sd_dt = global_ccg_res_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                            by = .(scenario, y_block)]
  setnames(global_ccg_res_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(global_ccg_res_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  global_ccg_res_dt = global_ccg_res_m_dt[global_ccg_res_sd_dt, on = .(scenario, y_block)]
  global_ccg_res_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccg_res_dt, c('scenario', 'y_block', 'IPCC_NAME'))
# YIELD
  global_ccg_res_y_r_dt  = d_ccg_res_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                      by = .(scenario, y_block, rep)]
  global_ccg_res_y_m_dt  = global_ccg_res_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(scenario, y_block)]
  global_ccg_res_y_sd_dt = global_ccg_res_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(scenario, y_block)]
  setnames(global_ccg_res_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(global_ccg_res_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  global_ccg_res_y_dt = global_ccg_res_y_m_dt[global_ccg_res_y_sd_dt, on = .(scenario, y_block)]
  global_ccg_res_y_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccg_res_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccl-res ##
  # GHG
  global_ccl_res_r_dt  = d_ccl_res_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                      by = .(scenario, y_block, rep)]
  global_ccl_res_m_dt  = global_ccl_res_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block)]
  global_ccl_res_sd_dt = global_ccl_res_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block)]
  setnames(global_ccl_res_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(global_ccl_res_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  global_ccl_res_dt = global_ccl_res_m_dt[global_ccl_res_sd_dt, on = .(scenario, y_block)]
  global_ccl_res_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccl_res_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  global_ccl_res_y_r_dt  = d_ccl_res_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                          by = .(scenario, y_block, rep)]
  global_ccl_res_y_m_dt  = global_ccl_res_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block)]
  global_ccl_res_y_sd_dt = global_ccl_res_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block)]
  setnames(global_ccl_res_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(global_ccl_res_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  global_ccl_res_y_dt = global_ccl_res_y_m_dt[global_ccl_res_y_sd_dt, on = .(scenario, y_block)]
  global_ccl_res_y_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccl_res_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccg-ntill ##
  # GHG
  global_ccg_ntill_r_dt  = d_ccg_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                      by = .(scenario, y_block, rep)]
  global_ccg_ntill_m_dt  = global_ccg_ntill_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block)]
  global_ccg_ntill_sd_dt = global_ccg_ntill_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block)]
  setnames(global_ccg_ntill_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(global_ccg_ntill_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  global_ccg_ntill_dt = global_ccg_ntill_m_dt[global_ccg_ntill_sd_dt, on = .(scenario, y_block)]
  global_ccg_ntill_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccg_ntill_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  global_ccg_ntill_y_r_dt  = d_ccg_ntill_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                          by = .(scenario, y_block, rep)]
  global_ccg_ntill_y_m_dt  = global_ccg_ntill_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block)]
  global_ccg_ntill_y_sd_dt = global_ccg_ntill_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block)]
  setnames(global_ccg_ntill_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(global_ccg_ntill_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  global_ccg_ntill_y_dt = global_ccg_ntill_y_m_dt[global_ccg_ntill_y_sd_dt, on = .(scenario, y_block)]
  global_ccg_ntill_y_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccg_ntill_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccl-ntill ##
  # GHG
  global_ccl_ntill_r_dt  = d_ccl_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(scenario, y_block, rep)]
  global_ccl_ntill_m_dt  = global_ccl_ntill_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                                 by = .(scenario, y_block)]
  global_ccl_ntill_sd_dt = global_ccl_ntill_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                                 by = .(scenario, y_block)]
  setnames(global_ccl_ntill_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(global_ccl_ntill_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  global_ccl_ntill_dt = global_ccl_ntill_m_dt[global_ccl_ntill_sd_dt, on = .(scenario, y_block)]
  global_ccl_ntill_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccl_ntill_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  global_ccl_ntill_y_r_dt  = d_ccl_ntill_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                              by = .(scenario, y_block, rep)]
  global_ccl_ntill_y_m_dt  = global_ccl_ntill_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                     by = .(scenario, y_block)]
  global_ccl_ntill_y_sd_dt = global_ccl_ntill_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                     by = .(scenario, y_block)]
  setnames(global_ccl_ntill_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(global_ccl_ntill_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  global_ccl_ntill_y_dt = global_ccl_ntill_y_m_dt[global_ccl_ntill_y_sd_dt, on = .(scenario, y_block)]
  global_ccl_ntill_y_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ccl_ntill_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
  ## regional
{ ## ccg-res ##
  # GHG
  region_ccg_res_r_dt  = d_ccg_res_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                     by = .(scenario, y_block, IPCC_NAME, rep)]
  region_ccg_res_m_dt  = region_ccg_res_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                            by = .(scenario, y_block, IPCC_NAME)]
  region_ccg_res_sd_dt = region_ccg_res_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccg_res_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_ccg_res_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  region_ccg_res_dt = region_ccg_res_m_dt[region_ccg_res_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccg_res_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  region_ccg_res_y_r_dt  = d_ccg_res_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                          by = .(scenario, y_block, rep, IPCC_NAME)]
  region_ccg_res_y_m_dt  = region_ccg_res_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  region_ccg_res_y_sd_dt = region_ccg_res_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccg_res_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_ccg_res_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  region_ccg_res_y_dt = region_ccg_res_y_m_dt[region_ccg_res_y_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccg_res_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccl-res ##
  # GHG
  region_ccl_res_r_dt  = d_ccl_res_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                      by = .(scenario, y_block, IPCC_NAME, rep)]
  region_ccl_res_m_dt  = region_ccl_res_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block, IPCC_NAME)]
  region_ccl_res_sd_dt = region_ccl_res_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccl_res_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_ccl_res_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  region_ccl_res_dt = region_ccl_res_m_dt[region_ccl_res_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccl_res_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  region_ccl_res_y_r_dt  = d_ccl_res_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                          by = .(scenario, y_block, rep, IPCC_NAME)]
  region_ccl_res_y_m_dt  = region_ccl_res_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  region_ccl_res_y_sd_dt = region_ccl_res_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccl_res_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_ccl_res_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  region_ccl_res_y_dt = region_ccl_res_y_m_dt[region_ccl_res_y_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccl_res_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccg-ntill ##
  # GHG
  region_ccg_ntill_r_dt  = d_ccg_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                      by = .(scenario, y_block, IPCC_NAME, rep)]
  region_ccg_ntill_m_dt  = region_ccg_ntill_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block, IPCC_NAME)]
  region_ccg_ntill_sd_dt = region_ccg_ntill_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                             by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccg_ntill_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_ccg_ntill_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  region_ccg_ntill_dt = region_ccg_ntill_m_dt[region_ccg_ntill_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccg_ntill_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  region_ccg_ntill_y_r_dt  = d_ccg_ntill_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                          by = .(scenario, y_block, rep, IPCC_NAME)]
  region_ccg_ntill_y_m_dt  = region_ccg_ntill_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  region_ccg_ntill_y_sd_dt = region_ccg_ntill_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccg_ntill_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_ccg_ntill_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  region_ccg_ntill_y_dt = region_ccg_ntill_y_m_dt[region_ccg_ntill_y_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccg_ntill_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
{ ## ccl-ntill ##
  # GHG
  region_ccl_ntill_r_dt  = d_ccl_ntill_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(scenario, y_block, IPCC_NAME, rep)]
  region_ccl_ntill_m_dt  = region_ccl_ntill_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  region_ccl_ntill_sd_dt = region_ccl_ntill_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
                                                 by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccl_ntill_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_ccl_ntill_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
  region_ccl_ntill_dt = region_ccl_ntill_m_dt[region_ccl_ntill_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccl_ntill_dt, c('scenario', 'y_block', 'IPCC_NAME'))
  # YIELD
  region_ccl_ntill_y_r_dt  = d_ccl_ntill_y_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                              by = .(scenario, y_block, rep, IPCC_NAME)]
  region_ccl_ntill_y_m_dt  = region_ccl_ntill_y_r_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                     by = .(scenario, y_block, IPCC_NAME)]
  region_ccl_ntill_y_sd_dt = region_ccl_ntill_y_r_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                     by = .(scenario, y_block, IPCC_NAME)]
  setnames(region_ccl_ntill_y_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_ccl_ntill_y_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
  region_ccl_ntill_y_dt = region_ccl_ntill_y_m_dt[region_ccl_ntill_y_sd_dt, on = .(scenario, y_block, IPCC_NAME)]
  setcolorder(region_ccl_ntill_y_dt, c('scenario', 'y_block', 'IPCC_NAME'))
}
#-----------------------------------------------------------------------------------------
# SAVE
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
ccg_res_dt  = rbind(global_ccg_res_dt, region_ccg_res_dt)
fwrite(ccg_res_dt, file = paste(out_p, 'ccg-res-cumulative-ghg-potential.csv', sep = '/'))
ccg_res_y_dt = rbind(global_ccg_res_y_dt, region_ccg_res_y_dt)
fwrite(ccg_res_y_dt, file = paste(out_p, 'ccg-res-cumulative-yield-potential.csv', sep = '/'))

  ## ccl-res ##
ccl_res_dt  = rbind(global_ccl_res_dt, region_ccl_res_dt)
fwrite(ccl_res_dt, file = paste(out_p, 'ccl-res-cumulative-ghg-potential.csv', sep = '/'))
ccl_res_y_dt = rbind(global_ccl_res_y_dt, region_ccl_res_y_dt)
fwrite(ccl_res_y_dt, file = paste(out_p, 'ccl-res-cumulative-yield-potential.csv', sep = '/'))

  ## ccg-ntill ##
ccg_ntill_dt  = rbind(global_ccg_ntill_dt, region_ccg_ntill_dt)
fwrite(ccg_ntill_dt, file = paste(out_p, 'ccg-ntill-cumulative-ghg-potential.csv', sep = '/'))
ccg_ntill_y_dt = rbind(global_ccg_ntill_y_dt, region_ccg_ntill_y_dt)
fwrite(ccg_ntill_y_dt, file = paste(out_p, 'ccg-ntill-cumulative-yield-potential.csv', sep = '/'))

  ## ccl-ntill ##
ccl_ntill_dt  = rbind(global_ccl_ntill_dt, region_ccl_ntill_dt)
fwrite(ccl_ntill_dt, file = paste(out_p, 'ccl-ntill-cumulative-ghg-potential.csv', sep = '/'))
ccl_ntill_y_dt = rbind(global_ccl_ntill_y_dt, region_ccl_ntill_y_dt)
fwrite(ccl_ntill_y_dt, file = paste(out_p, 'ccl-ntill-cumulative-yield-potential.csv', sep = '/'))
