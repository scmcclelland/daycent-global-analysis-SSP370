# file name:    scenario-crop-estimates.R
# created:      14 December 2024
# last updated: 17 December 2024

# description: This file combines crop output for each scenario, gcm (absolute responses) and
#              estimates differences in responses between scenario and continued practice (conv).
#              Units are converted from g C m-2 to Mg ha-1.
# note:        Run each scenario + conv separately (large amount of data)
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# CONSTANTS
#-----------------------------------------------------------------------------------------
Mg_ha = 100L # g C m-2 to Mg ha-1
C_gr  = 0.42 # Ma et al. 2018 | value for 'reproductive organs'
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FILES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
data_p   = paste(dir, 'data/daycent-simulations', sep = '/')
out_p    = paste(dir, 'data/daycent-post-processed', sep = '/')
#-----------------------------------------------------------------------------------------
# LOAD FILES & ESTIMATE CUMULATIVE GRAIN
#-----------------------------------------------------------------------------------------
  ## conv ##
load(paste(data_p, 'conv-grain-covariates-ssp370.Rdata', sep ='/'))
gc()
conv_y_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","time","y_block","cgrain")]
rm(grain_cv_rda_dt)
gc()
# estimate cumulative cgrain
conv_y_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, crop, scenario, irr, ssp, gcm)]
gc()
conv_y_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
conv_y_dt = conv_y_dt[time %in% c(2030, 2050, 2100)]
conv_y_dt[, time   := NULL]
gc()
  ## ccg-res ##
load(paste(data_p, 'ccg-res-grain-covariates-ssp370.Rdata', sep ='/'))
gc()
ccg_res_y_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","time","y_block","cgrain")]
rm(grain_cv_rda_dt)
gc()
# estimate cumulative cgrain
ccg_res_y_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, crop, scenario, irr, ssp, gcm)]
gc()
ccg_res_y_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
ccg_res_y_dt = ccg_res_y_dt[time %in% c(2030, 2050, 2100)]
ccg_res_y_dt[, time   := NULL]
gc()
  ## ccl-res ##
load(paste(data_p, 'ccl-res-grain-covariates-ssp370.Rdata', sep ='/'))
gc()
ccl_res_y_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","time","y_block","cgrain")]
rm(grain_cv_rda_dt)
gc()
# estimate cumulative cgrain
ccl_res_y_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, crop, scenario, irr, ssp, gcm)]
gc()
ccl_res_y_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
ccl_res_y_dt = ccl_res_y_dt[time %in% c(2030, 2050, 2100)]
ccl_res_y_dt[, time   := NULL]
gc()
  ## ccg-ntill ##
load(paste(data_p, 'ccg-ntill-grain-covariates-ssp370.Rdata', sep ='/'))
gc()
ccg_ntill_y_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","time","y_block","cgrain")]
rm(grain_cv_rda_dt)
gc()
# estimate cumulative cgrain
ccg_ntill_y_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, crop, scenario, irr, ssp, gcm)]
gc()
ccg_ntill_y_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
ccg_ntill_y_dt = ccg_ntill_y_dt[time %in% c(2030, 2050, 2100)]
ccg_ntill_y_dt[, time   := NULL]
gc()
  ## ccl-ntill ##
load(paste(data_p, 'ccl-ntill-grain-covariates-ssp370.Rdata', sep ='/'))
gc()
ccl_ntill_y_dt = grain_cv_rda_dt[, c("gridid","crop","scenario","irr","ssp","gcm","time","y_block","cgrain")]
rm(grain_cv_rda_dt)
gc()
# estimate cumulative cgrain
ccl_ntill_y_dt[, s_cgrain := cumsum(cgrain), by = .(gridid, crop, scenario, irr, ssp, gcm)]
gc()
ccl_ntill_y_dt[, cgrain := NULL]
# keep only 2030, 2050, 2100
ccl_ntill_y_dt = ccl_ntill_y_dt[time %in% c(2030, 2050, 2100)]
ccl_ntill_y_dt[, time   := NULL]
gc()
#-----------------------------------------------------------------------------------------
# UNIT CONVERSIONS
#-----------------------------------------------------------------------------------------
  ## conv ##
conv_y_dt[, s_cgrain := (s_cgrain/Mg_ha)/C_gr]
  ## ccg-res ##
ccg_res_y_dt[, s_cgrain := (s_cgrain/Mg_ha)/C_gr]
  ## ccl-res ##
ccl_res_y_dt[, s_cgrain := (s_cgrain/Mg_ha)/C_gr]
  ## ccg-ntill ##
ccg_ntill_y_dt[, s_cgrain := (s_cgrain/Mg_ha)/C_gr]
  ## ccl-ntill ##
ccl_ntill_y_dt[, s_cgrain := (s_cgrain/Mg_ha)/C_gr]
#-----------------------------------------------------------------------------------------
# ESTIMATE DIFFERENCES
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
d_ccg_res_y_dt = ccg_res_y_dt[conv_y_dt[, -c('scenario')], on = .(gridid   = gridid,
                                                            crop     = crop,
                                                            irr      = irr,
                                                            ssp      = ssp,
                                                            gcm      = gcm,
                                                            y_block  = y_block)]
rm(ccg_res_y_dt)
d_ccg_res_y_dt[, d_s_cgrain := s_cgrain - i.s_cgrain]
d_ccg_res_y_dt[, i.s_cgrain := NULL]
# remove incomplete cases
d_ccg_res_y_dt = d_ccg_res_y_dt[!is.na(scenario),]
  ## ccl-res
d_ccl_res_y_dt = ccl_res_y_dt[conv_y_dt[, -c('scenario')], on = .(gridid   = gridid,
                                                                  crop     = crop,
                                                                  irr      = irr,
                                                                  ssp      = ssp,
                                                                  gcm      = gcm,
                                                                  y_block  = y_block)]
rm(ccl_res_y_dt)
d_ccl_res_y_dt[, d_s_cgrain := s_cgrain - i.s_cgrain]
d_ccl_res_y_dt[, i.s_cgrain := NULL]
# remove incomplete cases
d_ccl_res_y_dt = d_ccl_res_y_dt[!is.na(scenario),]
  ## ccg-ntill
d_ccg_ntill_y_dt = ccg_ntill_y_dt[conv_y_dt[, -c('scenario')], on = .(gridid   = gridid,
                                                                  crop     = crop,
                                                                  irr      = irr,
                                                                  ssp      = ssp,
                                                                  gcm      = gcm,
                                                                  y_block  = y_block)]
rm(ccg_ntill_y_dt)
d_ccg_ntill_y_dt[, d_s_cgrain := s_cgrain - i.s_cgrain]
d_ccg_ntill_y_dt[, i.s_cgrain := NULL]
# remove incomplete cases
d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[!is.na(scenario),]
  ## ccl-ntill
d_ccl_ntill_y_dt = ccl_ntill_y_dt[conv_y_dt[, -c('scenario')], on = .(gridid   = gridid,
                                                                      crop     = crop,
                                                                      irr      = irr,
                                                                      ssp      = ssp,
                                                                      gcm      = gcm,
                                                                      y_block  = y_block)]
rm(ccl_ntill_y_dt)
d_ccl_ntill_y_dt[, d_s_cgrain := s_cgrain - i.s_cgrain]
d_ccl_ntill_y_dt[, i.s_cgrain := NULL]
# remove incomplete cases
d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[!is.na(scenario),]
#-----------------------------------------------------------------------------------------
# SAVE OUTPUT, actual emissions & differences
#-----------------------------------------------------------------------------------------
# NOTE: outliers at gridid 103186,164414
  ## conv ##
conv_y_dt = conv_y_dt[!gridid %in% c(103186,164414)]
save(conv_y_dt, file = paste(out_p, 'conv-absolute-yield.RData', sep = '/'))
  ## ccg-res ##
d_ccg_res_y_dt = d_ccg_res_y_dt[!gridid %in% c(103186,164414)]
save(d_ccg_res_y_dt, file = paste(out_p, 'ccg-res-yield.RData', sep = '/'))
  ## ccl-res ##
d_ccl_res_y_dt = d_ccl_res_y_dt[!gridid %in% c(103186,164414)]
save(d_ccl_res_y_dt, file = paste(out_p, 'ccl-res-yield.RData', sep = '/'))
  ## ccg-ntill
d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[!gridid %in% c(103186,164414)]
save(d_ccg_ntill_y_dt, file = paste(out_p, 'ccg-ntill-yield.RData', sep = '/'))
  ## ccl-ntill
d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[!gridid %in% c(103186,164414)]
save(d_ccl_ntill_y_dt, file = paste(out_p, 'ccl-ntill-yield.RData', sep = '/'))
