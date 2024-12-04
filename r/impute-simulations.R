# file name:    impute-simulations.R
# created:      18 November 2024
# last updated: 20 November 2024

# description: This file imputes cumulative grain yield (s_cgrain) for missing grid cells.
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
m_data   = paste(dir, 'data/daycent-simulations', sep = '/')
source('r/impute-functions.R')
#-----------------------------------------------------------------------------------------
# DATA: load and prep
#-----------------------------------------------------------------------------------------
# raster
raster = rast(paste(m_data,'msw-masked-cropland-rf-ir-area.tif', sep = '/'))

# simulation results | run through script one at a time
# load(paste(m_data, 'conv-grain-covariates-ssp370.Rdata', sep = '/'))
# load(paste(m_data, 'ccg-res-grain-covariates-ssp370.Rdata', sep = '/'))
# load(paste(m_data, 'ccl-res-grain-covariates-ssp370.Rdata', sep = '/'))
# load(paste(m_data, 'ccg-ntill-grain-covariates-ssp370.Rdata', sep = '/'))
load(paste(m_data, 'ccl-ntill-grain-covariates-ssp370.Rdata', sep = '/'))

gc()
grain_cv_rda_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","y_block",  
                                      "time","run_yrs","cgrain")]
gc()

# header info
load(paste(m_data, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
main_table = unique(main_table[, c('gridid', 'x', 'y')])

# join with simulation results
grain_cv_rda_dt = grain_cv_rda_dt[main_table, on = .(gridid = gridid)]
setcolorder(grain_cv_rda_dt, c('gridid', 'x', 'y'))
gc()

# estimate cumulative cgrain
grain_cv_rda_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, x, y, crop, scenario, irr, ssp, gcm)]
gc()
grain_cv_rda_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
grain_cv_rda_dt = grain_cv_rda_dt[time %in% c(2030, 2050, 2100)]
gc()
#-----------------------------------------------------------------------------------------
# cumulative cgrain (s_cgrain) by crop and water management
#-----------------------------------------------------------------------------------------
# MAIZE, RAINFED
imp_scgrain_m0_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'maiz' &
                                                          irr %in% 0,], raster[[1]],
                                        's_cgrain', 'maize_rainfed')
# add y_blocks
imp_scgrain_m0_dt[time %in% 2030, y_block := 2030]
imp_scgrain_m0_dt[time %in% 2050, y_block := 2050]
imp_scgrain_m0_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_m0_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                                 'y_block', 'time', 'type','s_cgrain'))

# MAIZE, IRRIGATED
imp_scgrain_m1_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'maiz' &
                                                irr %in% 1,], raster[[2]],
                                        's_cgrain', 'maize_irri')
# add y_blocks
imp_scgrain_m1_dt[time %in% 2030, y_block := 2030]
imp_scgrain_m1_dt[time %in% 2050, y_block := 2050]
imp_scgrain_m1_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_m1_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                            'y_block', 'time', 'type','s_cgrain'))

# SOYBEAN, RAINFED
imp_scgrain_s0_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'soyb' &
                                                          irr %in% 0,], raster[[3]],
                                        's_cgrain', 'soyb_rainfed')
# add y_blocks
imp_scgrain_s0_dt[time %in% 2030, y_block := 2030]
imp_scgrain_s0_dt[time %in% 2050, y_block := 2050]
imp_scgrain_s0_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_s0_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                                 'y_block', 'time', 'type','s_cgrain'))

# SOYBEAN, IRRIGATED
imp_scgrain_s1_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'soyb' &
                                                          irr %in% 1,], raster[[4]],
                                        's_cgrain', 'soyb_irri')
# add y_blocks
imp_scgrain_s1_dt[time %in% 2030, y_block := 2030]
imp_scgrain_s1_dt[time %in% 2050, y_block := 2050]
imp_scgrain_s1_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_s0_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                                 'y_block', 'time', 'type','s_cgrain'))
# WHEAT, RAINFED
imp_scgrain_w0_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'swht' &
                                                          irr %in% 0,], raster[[5]],
                                        's_cgrain', 'wht_rainfed')

# add y_blocks
imp_scgrain_w0_dt[time %in% 2030, y_block := 2030]
imp_scgrain_w0_dt[time %in% 2050, y_block := 2050]
imp_scgrain_w0_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_w0_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                                 'y_block', 'time', 'type','s_cgrain'))
# WHEAT, IRRIGATED
imp_scgrain_w1_dt = impute_gcm_response(grain_cv_rda_dt[crop %in% 'swht' &
                                                          irr %in% 1,], raster[[6]],
                                        's_cgrain', 'wht_irri')

# add y_blocks
imp_scgrain_w1_dt[time %in% 2030, y_block := 2030]
imp_scgrain_w1_dt[time %in% 2050, y_block := 2050]
imp_scgrain_w1_dt[time %in% 2100, y_block := 2100]

setcolorder(imp_scgrain_w1_dt, c('gridid', 'x', 'y', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                                 'y_block', 'time', 'type','s_cgrain'))
# bind all imputed
imp_scgrain_dt = rbind(imp_scgrain_m0_dt, imp_scgrain_m1_dt, imp_scgrain_s0_dt, imp_scgrain_s1_dt,
                       imp_scgrain_w0_dt, imp_scgrain_w1_dt)
# bind to simulated
grain_cv_rda_dt = rbind(grain_cv_rda_dt, imp_scgrain_dt, fill = TRUE)
# add type
grain_cv_rda_dt[is.na(type), type := 'simulated']
# drop run years, time
grain_cv_rda_dt[, run_yrs := NULL]
grain_cv_rda_dt[, time := NULL]
# order
setorder(grain_cv_rda_dt, gridid, gcm, y_block)
#-----------------------------------------------------------------------------------------
# save csv
#-----------------------------------------------------------------------------------------
# conv
# fwrite(grain_cv_rda_dt, paste0(m_data, '/conv-simulated-imputed-scgrain-ssp370.csv'))
# ccg-res
# fwrite(grain_cv_rda_dt, paste0(m_data, '/ccg-res-simulated-imputed-scgrain-ssp370.csv'))
# ccl-res
# fwrite(grain_cv_rda_dt, paste0(m_data, '/ccl-res-simulated-imputed-scgrain-ssp370.csv'))
# ccg-ntill
# fwrite(grain_cv_rda_dt, paste0(m_data, '/ccg-ntill-simulated-imputed-scgrain-ssp370.csv'))
# ccl-ntill
fwrite(grain_cv_rda_dt, paste0(m_data, '/ccl-ntill-simulated-imputed-scgrain-ssp370.csv'))
