# filename:    analysis-hectare-gridid.R
# created:     06 March 2023
# updated:     17 December 2024
# author:      S.C. McClelland
# description: This file estimates cumulative weighted mean and standard deviations for soil
#              GHG and crop yield responses from DayCent uncertainty and climate variance output,
#              respectively. Saved values are grid cell (gridid) responses for each crop, irr for GHG
#              and for crop yield.
#              These data are used for (1) explainable ML and (2) visualization.
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
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
# LOAD DT & RASTER
#-----------------------------------------------------------------------------------------
  ## ghg responses ##
# load(paste(data_p, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))
# drop absolute responses
d_ccg_res_dt   = d_ccg_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
gc()
d_ccl_res_dt   = d_ccl_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
gc()
d_ccg_ntill_dt = d_ccg_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
gc()
d_ccl_ntill_dt = d_ccl_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
gc()
  ## crop responses ##
load(paste(data_p, 'ccg-res-yield.RData', sep = '/'))
load(paste(data_p, 'ccl-res-yield.RData', sep = '/'))
load(paste(data_p, 'ccg-ntill-yield.RData', sep = '/'))
load(paste(data_p, 'ccl-ntill-yield.RData', sep = '/'))
# drop absolute responses
d_ccg_res_y_dt   = d_ccg_res_y_dt[, -c('s_cgrain')]
gc()
d_ccl_res_y_dt   = d_ccl_res_y_dt[, -c('s_cgrain')]
gc()
d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[, -c('s_cgrain')]
gc()
d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[, -c('s_cgrain')]
gc()
#-----------------------------------------------------------------------------------------
# ESTIMATE GRIDID RESPONSES - GHG
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_gl_m  = d_ccg_res_dt[, lapply(.SD, mean), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
d_ccg_res_gl_sd = d_ccg_res_dt[, lapply(.SD, sd), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccg_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_res_gl = d_ccg_res_gl_m[d_ccg_res_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccl-res ##
d_ccl_res_gl_m  = d_ccl_res_dt[, lapply(.SD, mean), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
d_ccl_res_gl_sd = d_ccl_res_dt[, lapply(.SD, sd), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccl_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_res_gl = d_ccl_res_gl_m[d_ccl_res_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccg-ntill ##
d_ccg_ntill_gl_m  = d_ccg_ntill_dt[, lapply(.SD, mean), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
d_ccg_ntill_gl_sd = d_ccg_ntill_dt[, lapply(.SD, sd), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                               by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccg_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_ntill_gl = d_ccg_ntill_gl_m[d_ccg_ntill_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccl-ntill ##
d_ccl_ntill_gl_m  = d_ccl_ntill_dt[, lapply(.SD, mean), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
d_ccl_ntill_gl_sd = d_ccl_ntill_dt[, lapply(.SD, sd), .SDcols = c('d_s_SOC','d_s_N2O','d_s_GHG'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccl_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_ntill_gl = d_ccl_ntill_gl_m[d_ccl_ntill_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
#-----------------------------------------------------------------------------------------
# ESTIMATE GRIDID RESPONSE - Yield
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_y_gl_m  = d_ccg_res_y_dt[, lapply(.SD, mean), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block, crop, irr, gridid)]
d_ccg_res_y_gl_sd = d_ccg_res_y_dt[, lapply(.SD, sd), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccg_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_res_y_gl = d_ccg_res_y_gl_m[d_ccg_res_y_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccl-res ##
d_ccl_res_y_gl_m  = d_ccl_res_y_dt[, lapply(.SD, mean), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
d_ccl_res_y_gl_sd = d_ccl_res_y_dt[, lapply(.SD, sd), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccl_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_res_y_gl = d_ccl_res_y_gl_m[d_ccl_res_y_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccg-ntill ##
d_ccg_ntill_y_gl_m  = d_ccg_ntill_y_dt[, lapply(.SD, mean), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
d_ccg_ntill_y_gl_sd = d_ccg_ntill_y_dt[, lapply(.SD, sd), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccg_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_ntill_y_gl = d_ccg_ntill_y_gl_m[d_ccg_ntill_y_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
  ## ccl-ntill ##
d_ccl_ntill_y_gl_m  = d_ccl_ntill_y_dt[, lapply(.SD, mean), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block, crop, irr, gridid)]
d_ccl_ntill_y_gl_sd = d_ccl_ntill_y_dt[, lapply(.SD, sd), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block, crop, irr, gridid)]
setnames(d_ccl_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_ntill_y_gl = d_ccl_ntill_y_gl_m[d_ccl_ntill_y_gl_sd, on = .(scenario, y_block, crop, irr, gridid)]
#-----------------------------------------------------------------------------------------
# SAVE DT
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
fwrite(ccg_res_gl, file   = paste(out_p, 'ccg-res-gridid-ghg-responses.csv', sep = '/'))
fwrite(ccg_res_y_gl, file = paste(out_p, 'ccg-res-gridid-yield-responses.csv', sep = '/'))

  ## ccl-res ##
fwrite(ccl_res_gl, file   = paste(out_p, 'ccl-res-gridid-ghg-responses.csv', sep = '/'))
fwrite(ccl_res_y_gl, file = paste(out_p, 'ccl-res-gridid-yield-responses.csv', sep = '/'))

  ## ccg-ntill ##
fwrite(ccg_ntill_gl, file   = paste(out_p, 'ccg-ntill-gridid-ghg-responses.csv', sep = '/'))
fwrite(ccg_ntill_y_gl, file = paste(out_p, 'ccg-ntill-gridid-yield-responses.csv', sep = '/'))

  ## ccl-ntill ##
fwrite(ccl_ntill_gl, file   = paste(out_p, 'ccl-ntill-gridid-ghg-responses.csv', sep = '/'))
fwrite(ccl_ntill_y_gl, file = paste(out_p, 'ccl-ntill-gridid-yield-responses.csv', sep = '/'))
