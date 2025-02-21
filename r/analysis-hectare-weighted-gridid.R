# filename:    analysis-hectare-weighted-gridid.R
# created:     06 March 2023
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file estimates cumulative weighted mean and standard deviations for soil
#              GHG and crop yield responses from DayCent uncertainty and climate variance output,
#              respectively. Output data are for use in maps of hectare GHG and crop response.
#              These data are used for map figures.
# note:        Run each scenario x response separately (large amount of data)
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

### These should be loaded and executed one at a time because of file size ###  
### Uncomment as required to run each scenario x response type (GHG or yield) ###
  ## ghg responses ##
load(paste(data_p, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
load(paste(data_p, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
load(paste(data_p, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
# load(paste(data_p, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))

# drop absolute responses
# d_ccg_res_dt       = d_ccg_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
# gc()
# d_ccl_res_dt   = d_ccl_res_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
# gc()
# d_ccg_ntill_dt = d_ccg_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
# gc()
d_ccl_ntill_dt = d_ccl_ntill_dt[, -c('s_SOC', 's_dN2O', 's_iN2O', 's_GHG')]
gc()

  ## crop responses ##
# load(paste(data_p, 'ccg-res-yield.RData', sep = '/'))
# load(paste(data_p, 'ccl-res-yield.RData', sep = '/'))
# load(paste(data_p, 'ccg-ntill-yield.RData', sep = '/'))
# load(paste(data_p, 'ccl-ntill-yield.RData', sep = '/'))

# drop absolute responses
# d_ccg_res_y_dt   = d_ccg_res_y_dt[, -c('s_cgrain')]
# gc()
# d_ccl_res_y_dt   = d_ccl_res_y_dt[, -c('s_cgrain')]
# gc()
# d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[, -c('s_cgrain')]
# gc()
# d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[, -c('s_cgrain')]
# gc()
#-----------------------------------------------------------------------------------------
# ADD CROP AREA WEIGHTS
#-----------------------------------------------------------------------------------------
### Uncomment as required to run each scenario x response type (GHG or yield) ###

# d_ccg_res_dt   = add_l_weights(d_ccg_res_dt, crop_area_dt)
# gc()
# d_ccl_res_dt   = add_l_weights(d_ccl_res_dt, crop_area_dt)
# gc()
# d_ccg_ntill_dt = add_l_weights(d_ccg_ntill_dt, crop_area_dt)
# gc()
d_ccl_ntill_dt = add_l_weights(d_ccl_ntill_dt, crop_area_dt)
gc()

# d_ccg_res_y_dt   = add_l_weights(d_ccg_res_y_dt, crop_area_dt)
# gc()
# d_ccl_res_y_dt   = add_l_weights(d_ccl_res_y_dt, crop_area_dt)
# gc()
# d_ccg_ntill_y_dt = add_l_weights(d_ccg_ntill_y_dt, crop_area_dt)
# gc()
# d_ccl_ntill_y_dt = add_l_weights(d_ccl_ntill_y_dt, crop_area_dt)
# gc()
#-----------------------------------------------------------------------------------------
# ESTIMATE GRIDID RESPONSE - GHG
#-----------------------------------------------------------------------------------------
### Uncomment as required to run each scenario x response type (GHG or yield) ###
{
  ## ccg-res ##
# d_ccg_res_gl_m  = d_ccg_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
#                                                                             'd_s_N2O',
#                                                                             'd_s_GHG'),
#                                by = .(scenario, y_block, gridid)]
# d_ccg_res_gl_sd = d_ccg_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
#                                                                           'd_s_N2O',
#                                                                           'd_s_GHG'),
#                                by = .(scenario, y_block, gridid)]
# setnames(d_ccg_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
# ccg_res_gl = d_ccg_res_gl_m[d_ccg_res_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccg_res_gl = ccg_res_gl[!is.na(d_s_GHG)]
}
{
  ## ccl-res ##
# d_ccl_res_gl_m  = d_ccl_res_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
#                                                                            'd_s_N2O',
#                                                                            'd_s_GHG'),
#                                                                 by = .(scenario, y_block, gridid)]
# d_ccl_res_gl_sd = d_ccl_res_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
#                                                                           'd_s_N2O',
#                                                                           'd_s_GHG'),
#                                                                 by = .(scenario, y_block, gridid)]
# setnames(d_ccl_res_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
# ccl_res_gl = d_ccl_res_gl_m[d_ccl_res_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccl_res_gl = ccl_res_gl[!is.na(d_s_GHG)]
}
{
  ## ccg-ntill ##
# d_ccg_ntill_gl_m  = d_ccg_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
#                                                                             'd_s_N2O',
#                                                                             'd_s_GHG'),
#                                by = .(scenario, y_block, gridid)]
# d_ccg_ntill_gl_sd = d_ccg_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
#                                                                           'd_s_N2O',
#                                                                           'd_s_GHG'),
#                                by = .(scenario, y_block, gridid)]
# setnames(d_ccg_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
# ccg_ntill_gl = d_ccg_ntill_gl_m[d_ccg_ntill_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccg_ntill_gl = ccg_ntill_gl[!is.na(d_s_GHG)]
}
{
  ## ccl-ntill ##
d_ccl_ntill_gl_m  = d_ccl_ntill_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_SOC',
                                                                                'd_s_N2O',
                                                                                'd_s_GHG'),
                                   by = .(scenario, y_block, gridid)]
d_ccl_ntill_gl_sd = d_ccl_ntill_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_SOC',
                                                                              'd_s_N2O',
                                                                              'd_s_GHG'),
                                   by = .(scenario, y_block, gridid)]
setnames(d_ccl_ntill_gl_sd, c('d_s_SOC', 'd_s_N2O', 'd_s_GHG'), c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG'))
ccl_ntill_gl = d_ccl_ntill_gl_m[d_ccl_ntill_gl_sd, on = .(scenario, y_block, gridid)]
# remove NA (from 0 crop area and resulting 0 weights)
ccl_ntill_gl = ccl_ntill_gl[!is.na(d_s_GHG)]
}
#-----------------------------------------------------------------------------------------
# ESTIMATE GRIDID RESPONSE - Yield
#-----------------------------------------------------------------------------------------
### Uncomment as required to run each scenario x response type (GHG or yield) ###
{
  ## ccg-res ##
# d_ccg_res_y_gl_m  = d_ccg_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
#                                by = .(scenario, y_block, gridid)]
# d_ccg_res_y_gl_sd = d_ccg_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
#                                by = .(scenario, y_block, gridid)]
# setnames(d_ccg_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
# ccg_res_y_gl = d_ccg_res_y_gl_m[d_ccg_res_y_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccg_res_y_gl = ccg_res_y_gl[!is.na(d_s_cgrain)]
}
{
  ## ccl-res ##
# d_ccl_res_y_gl_m  = d_ccl_res_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
#                                    by = .(scenario, y_block, gridid)]
# d_ccl_res_y_gl_sd = d_ccl_res_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
#                                    by = .(scenario, y_block, gridid)]
# setnames(d_ccl_res_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
# ccl_res_y_gl = d_ccl_res_y_gl_m[d_ccl_res_y_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccl_res_y_gl = ccl_res_y_gl[!is.na(d_s_cgrain)]
}
{
  ## ccg-ntill ##
# d_ccg_ntill_y_gl_m  = d_ccg_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
#                                    by = .(scenario, y_block, gridid)]
# d_ccg_ntill_y_gl_sd = d_ccg_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
#                                    by = .(scenario, y_block, gridid)]
# setnames(d_ccg_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
# ccg_ntill_y_gl = d_ccg_ntill_y_gl_m[d_ccg_ntill_y_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccg_ntill_y_gl = ccg_ntill_y_gl[!is.na(d_s_cgrain)]
}
{
  ## ccl-ntill ##
# d_ccl_ntill_y_gl_m  = d_ccl_ntill_y_dt[, lapply(.SD, weighted.mean, w), .SDcols = c('d_s_cgrain'),
#                                        by = .(scenario, y_block, gridid)]
# d_ccl_ntill_y_gl_sd = d_ccl_ntill_y_dt[, lapply(.SD, weighted_sd, w), .SDcols = c('d_s_cgrain'),
#                                        by = .(scenario, y_block, gridid)]
# setnames(d_ccl_ntill_y_gl_sd, c('d_s_cgrain'), c('sd_s_cgrain'))
# ccl_ntill_y_gl = d_ccl_ntill_y_gl_m[d_ccl_ntill_y_gl_sd, on = .(scenario, y_block, gridid)]
# # remove NA (from 0 crop area and resulting 0 weights)
# ccl_ntill_y_gl = ccl_ntill_y_gl[!is.na(d_s_cgrain)]
}
#-----------------------------------------------------------------------------------------
# SAVE DT
#-----------------------------------------------------------------------------------------
### Uncomment as required to run each scenario x response type (GHG or yield) ###
#   ## ccg-res ##
# fwrite(ccg_res_gl, file = paste(out_p, 'ccg-res-weighted-gridid-ghg-responses.csv', sep = '/'))
# fwrite(ccg_res_y_gl, file = paste(out_p, 'ccg-res-weighted-gridid-yield-responses.csv', sep = '/'))
# 
#   ## ccl-res ##
# fwrite(ccl_res_gl, file = paste(out_p, 'ccl-res-weighted-gridid-ghg-responses.csv', sep = '/'))
# fwrite(ccl_res_y_gl, file = paste(out_p, 'ccl-res-weighted-gridid-yield-responses.csv', sep = '/'))
# 
# ## ccg-ntill ##
# fwrite(ccg_ntill_gl, file = paste(out_p, 'ccg-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))
# fwrite(ccg_ntill_y_gl, file = paste(out_p, 'ccg-ntill-weighted-gridid-yield-responses.csv', sep = '/'))
#
## ccl-ntill ##
fwrite(ccl_ntill_gl, file = paste(out_p, 'ccl-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))
# fwrite(ccl_ntill_y_gl, file = paste(out_p, 'ccl-ntill-weighted-gridid-yield-responses.csv', sep = '/'))
