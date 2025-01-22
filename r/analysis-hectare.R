# filename:    analysis-hectare.R
# created:     06 March 2023
# updated:     17 December 2024
# author:      S.C. McClelland
# description: This file estimates cumulative weighted mean and standard deviations for soil
#              GHG and crop yield responses from DayCent uncertainty and climate variance output,
#              respectively.
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
# LOAD DT & RASTER
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
# convert to dt
crop_area_dt = grid_crop_area(crop_r)
rm(crop_r)
  ## ghg responses ##
# load(paste(data_p, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
load(paste(data_p, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))
# drop absolute responses
d_ccg_res_dt   = d_ccg_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O', 's_GHG')]
gc()
d_ccl_res_dt   = d_ccl_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O', 's_GHG')]
gc()
d_ccg_ntill_dt = d_ccg_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O', 's_GHG')]
gc()
d_ccl_ntill_dt = d_ccl_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_N2O', 's_GHG')]
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
# ADD REGION NAMES
#-----------------------------------------------------------------------------------------
d_ccg_res_dt    = add_region(d_ccg_res_dt, main_table, ipcc_region_dt)
d_ccl_res_dt    = add_region(d_ccl_res_dt, main_table, ipcc_region_dt)
d_ccg_ntill_dt  = add_region(d_ccg_ntill_dt, main_table, ipcc_region_dt)
d_ccl_ntill_dt  = add_region(d_ccl_ntill_dt, main_table, ipcc_region_dt)

d_ccg_res_y_dt    = add_region(d_ccg_res_y_dt, main_table, ipcc_region_dt)
d_ccl_res_y_dt    = add_region(d_ccl_res_y_dt, main_table, ipcc_region_dt)
d_ccg_ntill_y_dt  = add_region(d_ccg_ntill_y_dt, main_table, ipcc_region_dt)
d_ccl_ntill_y_dt  = add_region(d_ccl_ntill_y_dt, main_table, ipcc_region_dt)
#-----------------------------------------------------------------------------------------
# ADD CROP AREA WEIGHTS
#-----------------------------------------------------------------------------------------
d_ccg_res_dt = add_weights(d_ccg_res_dt, crop_area_dt)
gc()
d_ccl_res_dt = add_weights(d_ccl_res_dt, crop_area_dt)
gc()
d_ccg_ntill_dt = add_weights(d_ccg_ntill_dt, crop_area_dt)
gc()
d_ccl_ntill_dt = add_weights(d_ccl_ntill_dt, crop_area_dt)
gc()

d_ccg_res_y_dt = add_weights(d_ccg_res_y_dt, crop_area_dt)
gc()
d_ccl_res_y_dt = add_weights(d_ccl_res_y_dt, crop_area_dt)
gc()
d_ccg_ntill_y_dt = add_weights(d_ccg_ntill_y_dt, crop_area_dt)
gc()
d_ccl_ntill_y_dt = add_weights(d_ccl_ntill_y_dt, crop_area_dt)
gc()
#-----------------------------------------------------------------------------------------
# ESTIMATE GLOBAL RESPONSE - GHG
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_gl_m  = d_ccg_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                            'd_s_N2O',
                                                                            'd_s_GHG'),
                               by = .(scenario, y_block)]
d_ccg_res_gl_sd = d_ccg_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                               by = .(scenario, y_block)]
setnames(d_ccg_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_res_gl = d_ccg_res_gl_m[d_ccg_res_gl_sd, on = .(scenario, y_block)]
ccg_res_gl[, IPCC_NAME := 'GLB']
setcolorder(ccg_res_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccl-res ##
d_ccl_res_gl_m  = d_ccl_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                           'd_s_N2O',
                                                                           'd_s_GHG'),
                                                                by = .(scenario, y_block)]
d_ccl_res_gl_sd = d_ccl_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                                                                by = .(scenario, y_block)]
setnames(d_ccl_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_res_gl = d_ccl_res_gl_m[d_ccl_res_gl_sd, on = .(scenario, y_block)]
ccl_res_gl[, IPCC_NAME := 'GLB']
setcolorder(ccl_res_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccg-ntill ##
d_ccg_ntill_gl_m  = d_ccg_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                            'd_s_N2O',
                                                                            'd_s_GHG'),
                               by = .(scenario, y_block)]
d_ccg_ntill_gl_sd = d_ccg_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                               by = .(scenario, y_block)]
setnames(d_ccg_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_ntill_gl = d_ccg_ntill_gl_m[d_ccg_ntill_gl_sd, on = .(scenario, y_block)]
ccg_ntill_gl[, IPCC_NAME := 'GLB']
setcolorder(ccg_ntill_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccl-ntill ##
d_ccl_ntill_gl_m  = d_ccl_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                                'd_s_N2O',
                                                                                'd_s_GHG'),
                                   by = .(scenario, y_block)]
d_ccl_ntill_gl_sd = d_ccl_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                              'd_s_N2O',
                                                                              'd_s_GHG'),
                                   by = .(scenario, y_block)]
setnames(d_ccl_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_ntill_gl = d_ccl_ntill_gl_m[d_ccl_ntill_gl_sd, on = .(scenario, y_block)]
ccl_ntill_gl[, IPCC_NAME := 'GLB']
setcolorder(ccl_ntill_gl, c('scenario', 'y_block', 'IPCC_NAME'))
#-----------------------------------------------------------------------------------------
# ESTIMATE GLOBAL RESPONSE - Yield
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_y_gl_m  = d_ccg_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block)]
d_ccg_res_y_gl_sd = d_ccg_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block)]
setnames(d_ccg_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_res_y_gl = d_ccg_res_y_gl_m[d_ccg_res_y_gl_sd, on = .(scenario, y_block)]
ccg_res_y_gl[, IPCC_NAME := 'GLB']
setcolorder(ccg_res_y_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccl-res ##
d_ccl_res_y_gl_m  = d_ccl_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block)]
d_ccl_res_y_gl_sd = d_ccl_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block)]
setnames(d_ccl_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_res_y_gl = d_ccl_res_y_gl_m[d_ccl_res_y_gl_sd, on = .(scenario, y_block)]
ccl_res_y_gl[, IPCC_NAME := 'GLB']
setcolorder(ccl_res_y_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccg-ntill ##
d_ccg_ntill_y_gl_m  = d_ccg_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block)]
d_ccg_ntill_y_gl_sd = d_ccg_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block)]
setnames(d_ccg_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_ntill_y_gl = d_ccg_ntill_y_gl_m[d_ccg_ntill_y_gl_sd, on = .(scenario, y_block)]
ccg_ntill_y_gl[, IPCC_NAME := 'GLB']
setcolorder(ccg_ntill_y_gl, c('scenario', 'y_block', 'IPCC_NAME'))
  ## ccl-ntill ##
d_ccl_ntill_y_gl_m  = d_ccl_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block)]
d_ccl_ntill_y_gl_sd = d_ccl_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block)]
setnames(d_ccl_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_ntill_y_gl = d_ccl_ntill_y_gl_m[d_ccl_ntill_y_gl_sd, on = .(scenario, y_block)]
ccl_ntill_y_gl[, IPCC_NAME := 'GLB']
setcolorder(ccl_ntill_y_gl, c('scenario', 'y_block', 'IPCC_NAME'))
#-----------------------------------------------------------------------------------------
# ESTIMATE REGIONAL RESPONSE - GHG
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_rg_m  = d_ccg_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                            'd_s_N2O',
                                                                            'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
d_ccg_res_rg_sd = d_ccg_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccg_res_rg_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_res_rg = d_ccg_res_rg_m[d_ccg_res_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccl-res
d_ccl_res_rg_m  = d_ccl_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                            'd_s_N2O',
                                                                            'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
d_ccl_res_rg_sd = d_ccl_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccl_res_rg_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_res_rg = d_ccl_res_rg_m[d_ccl_res_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccg-ntill ##
d_ccg_ntill_rg_m  = d_ccg_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                            'd_s_N2O',
                                                                            'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
d_ccg_ntill_rg_sd = d_ccg_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                          'd_s_N2O',
                                                                          'd_s_GHG'),
                               by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccg_ntill_rg_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccg_ntill_rg = d_ccg_ntill_rg_m[d_ccg_ntill_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccl-ntill
d_ccl_ntill_rg_m  = d_ccl_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                                'd_s_N2O',
                                                                                'd_s_GHG'),
                                   by = .(scenario, y_block, IPCC_NAME)]
d_ccl_ntill_rg_sd = d_ccl_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                              'd_s_N2O',
                                                                              'd_s_GHG'),
                                   by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccl_ntill_rg_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_ntill_rg = d_ccl_ntill_rg_m[d_ccl_ntill_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
#-----------------------------------------------------------------------------------------
# ESTIMATE REGIONAL RESPONSE - Yield
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_y_rg_m  = d_ccg_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block, IPCC_NAME)]
d_ccg_res_y_rg_sd = d_ccg_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                               by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccg_res_y_rg_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_res_y_rg = d_ccg_res_y_rg_m[d_ccg_res_y_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccl-res ##
d_ccl_res_y_rg_m  = d_ccl_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, IPCC_NAME)]
d_ccl_res_y_rg_sd = d_ccl_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccl_res_y_rg_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_res_y_rg = d_ccl_res_y_rg_m[d_ccl_res_y_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccg-ntill ##
d_ccg_ntill_y_rg_m  = d_ccg_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, IPCC_NAME)]
d_ccg_ntill_y_rg_sd = d_ccg_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                   by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccg_ntill_y_rg_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccg_ntill_y_rg = d_ccg_ntill_y_rg_m[d_ccg_ntill_y_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
  ## ccg-ntill ##
d_ccl_ntill_y_rg_m  = d_ccl_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block, IPCC_NAME)]
d_ccl_ntill_y_rg_sd = d_ccl_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
                                       by = .(scenario, y_block, IPCC_NAME)]
setnames(d_ccl_ntill_y_rg_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
ccl_ntill_y_rg = d_ccl_ntill_y_rg_m[d_ccl_ntill_y_rg_sd, on = .(scenario, y_block, IPCC_NAME)]
#-----------------------------------------------------------------------------------------
# SAVE DT
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
ccg_res = rbind(ccg_res_gl, ccg_res_rg)
fwrite(ccg_res, file = paste(out_p, 'ccg-res-hectare-ghg-responses.csv', sep = '/'))
ccg_res_y = rbind(ccg_res_y_gl, ccg_res_y_rg)
fwrite(ccg_res_y, file = paste(out_p, 'ccg-res-hectare-yield-responses.csv', sep = '/'))

  ## ccl-res ##
ccl_res = rbind(ccl_res_gl, ccl_res_rg)
fwrite(ccl_res, file = paste(out_p, 'ccl-res-hectare-ghg-responses.csv', sep = '/'))
ccl_res_y = rbind(ccl_res_y_gl, ccl_res_y_rg)
fwrite(ccl_res_y, file = paste(out_p, 'ccl-res-hectare-yield-responses.csv', sep = '/'))

## ccg-ntill ##
ccg_ntill = rbind(ccg_ntill_gl, ccg_ntill_rg)
fwrite(ccg_ntill, file = paste(out_p, 'ccg-ntill-hectare-ghg-responses.csv', sep = '/'))
ccg_ntill_y = rbind(ccg_ntill_y_gl, ccg_ntill_y_rg)
fwrite(ccg_ntill_y, file = paste(out_p, 'ccg-ntill-hectare-yield-responses.csv', sep = '/'))

## ccl-ntill ##
ccl_ntill = rbind(ccl_ntill_gl, ccl_ntill_rg)
fwrite(ccl_ntill, file = paste(out_p, 'ccl-ntill-hectare-ghg-responses.csv', sep = '/'))
ccl_ntill_y = rbind(ccl_ntill_y_gl, ccl_ntill_y_rg)
fwrite(ccl_ntill_y, file = paste(out_p, 'ccl-ntill-hectare-yield-responses.csv', sep = '/'))