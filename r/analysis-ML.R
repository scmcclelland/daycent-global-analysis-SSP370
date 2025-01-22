# filename:    analysis-potential.R
# created:     06 March 2023
# updated:     07 January 2025
# author:      S.C. McClelland
# description: This file uses random forest models and SHapley Additive exPlanation values (SHAP)
#              to assess the environmental and management drivers of GHG and yield responses.
#              Models are ...
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(caret)
library(data.table)
library(factoextra)
library(fastshap)
library(ranger)
library(rstudioapi)
library(scales)
library(sf)
library(shapviz)
library(stringr)
library(terra)
library(umap)
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
# LOAD DTs
#-----------------------------------------------------------------------------------------
  ## input table / management data ##
load(paste(data_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep subset of data
main_table = main_table[, .(gridid, crop, irr, fertN.amt, orgN.amt, res.rtrn.amt,
                                 frac_NH4, frac_NO3, frac_Urea)]
  ## site data ##
site_table = fread(paste(data_p, 'input_site_data.csv', sep = '/'))
site_table = site_table[crop == 'wht', crop := 'swht']
# reduce variables
site_table = site_table[, .(gridid, crop, irr, ELEV, MINERL_sum_, NITRAT_sum_,
                            RWCF_sum_, SWCINI_sum_, SOMC_sum_, SLBLKD, SLCLAY,
                            SLCLIM, SLFLDC, SLPH, SLSAND, SLSATC, SLWLTP)]
  ## weather data ##
wth_table  = fread(paste(data_p, 'input_weather_data_ssp370.csv', sep = '/'))

  ## GHG estimates ##
{ 
  ## ccg-res ##
  d_ccg_res_dt = fread(paste(out_p, 'ccg-res-gridid-ghg-responses.csv', sep = '/'))
  d_ccg_res_dt = d_ccg_res_dt[y_block == 2100, -c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG')]
  ## ccl-res ##
  d_ccl_res_dt = fread(paste(out_p, 'ccl-res-gridid-ghg-responses.csv', sep = '/'))
  d_ccl_res_dt = d_ccl_res_dt[y_block == 2100, -c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG')]
  ## ccg-ntill ##
  d_ccg_ntill_dt = fread(paste(out_p, 'ccg-ntill-gridid-ghg-responses.csv', sep = '/'))
  d_ccg_ntill_dt = d_ccg_ntill_dt[y_block == 2100, -c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG')]
  ## ccl-ntill ##
  d_ccl_ntill_dt = fread(paste(out_p, 'ccl-ntill-gridid-ghg-responses.csv', sep = '/'))
  d_ccl_ntill_dt  = d_ccl_ntill_dt[y_block == 2100, -c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG')]
}
  ## Yield estimates ##
{ 
  ## ccg-res ##
  d_ccg_res_y_dt = fread(paste(out_p, 'ccg-res-gridid-yield-responses.csv', sep = '/'))
  d_ccg_res_y_dt = d_ccg_res_y_dt[y_block == 2100, -c('sd_s_cgrain')]
  ## ccl-res ##
  d_ccl_res_y_dt = fread(paste(out_p, 'ccl-res-gridid-yield-responses.csv', sep = '/'))
  d_ccl_res_y_dt = d_ccl_res_y_dt[y_block == 2100, -c('sd_s_cgrain')]
  ## ccg-ntill ##
  d_ccg_ntill_y_dt = fread(paste(out_p, 'ccg-ntill-gridid-yield-responses.csv', sep = '/'))
  d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[y_block == 2100, -c('sd_s_cgrain')]
  ## ccl-ntill ##
  d_ccl_ntill_y_dt = fread(paste(out_p, 'ccl-ntill-gridid-yield-responses.csv', sep = '/'))
  d_ccl_ntill_y_dt  = d_ccl_ntill_y_dt[y_block == 2100, -c('sd_s_cgrain')]
}
#-----------------------------------------------------------------------------------------
# COMBINE DTs BY SCENARIO
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
# responses
d_ccg_res_dt = d_ccg_res_dt[d_ccg_res_y_dt, on = .(gridid   = gridid,
                                                   y_block  = y_block,
                                                   crop     = crop,
                                                   irr      = irr,
                                                   scenario = scenario)]
d_ccg_res_dt = d_ccg_res_dt[!is.na(d_s_GHG)]
# site
d_ccg_res_dt = d_ccg_res_dt[site_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccg_res_dt = d_ccg_res_dt[!is.na(y_block)]
# management
d_ccg_res_dt = d_ccg_res_dt[main_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccg_res_dt = d_ccg_res_dt[!is.na(y_block)]
# weather
d_ccg_res_dt = d_ccg_res_dt[wth_table, on = .(gridid = gridid)]
d_ccg_res_dt = d_ccg_res_dt[!is.na(scenario)]

  ## ccl-res ##
# responses
d_ccl_res_dt = d_ccl_res_dt[d_ccl_res_y_dt, on = .(gridid   = gridid,
                                                   y_block  = y_block,
                                                   crop     = crop,
                                                   irr      = irr,
                                                   scenario = scenario)]
d_ccl_res_dt = d_ccl_res_dt[!is.na(d_s_GHG)]
# site
d_ccl_res_dt = d_ccl_res_dt[site_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccl_res_dt = d_ccl_res_dt[!is.na(y_block)]
# management
d_ccl_res_dt = d_ccl_res_dt[main_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccl_res_dt = d_ccl_res_dt[!is.na(y_block)]
# weather
d_ccl_res_dt = d_ccl_res_dt[wth_table, on = .(gridid = gridid)]
d_ccl_res_dt = d_ccl_res_dt[!is.na(scenario)]

  ## ccg-ntill ##
# responses
d_ccg_ntill_dt = d_ccg_ntill_dt[d_ccg_ntill_y_dt, on = .(gridid   = gridid,
                                                   y_block  = y_block,
                                                   crop     = crop,
                                                   irr      = irr,
                                                   scenario = scenario)]
d_ccg_ntill_dt = d_ccg_ntill_dt[!is.na(d_s_GHG)]
# site
d_ccg_ntill_dt = d_ccg_ntill_dt[site_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccg_ntill_dt = d_ccg_ntill_dt[!is.na(y_block)]
# management
d_ccg_ntill_dt = d_ccg_ntill_dt[main_table, on = .(gridid = gridid,
                                               crop   = crop,
                                               irr    = irr)]
d_ccg_ntill_dt = d_ccg_ntill_dt[!is.na(y_block)]
# weather
d_ccg_ntill_dt = d_ccg_ntill_dt[wth_table, on = .(gridid = gridid)]
d_ccg_ntill_dt = d_ccg_ntill_dt[!is.na(scenario)]

  ## ccl-ntill ##
# responses
d_ccl_ntill_dt = d_ccl_ntill_dt[d_ccl_ntill_y_dt, on = .(gridid   = gridid,
                                                         y_block  = y_block,
                                                         crop     = crop,
                                                         irr      = irr,
                                                         scenario = scenario)]
d_ccl_ntill_dt = d_ccl_ntill_dt[!is.na(d_s_GHG)]
# site
d_ccl_ntill_dt = d_ccl_ntill_dt[site_table, on = .(gridid = gridid,
                                                   crop   = crop,
                                                   irr    = irr)]
d_ccl_ntill_dt = d_ccl_ntill_dt[!is.na(y_block)]
# management
d_ccl_ntill_dt = d_ccl_ntill_dt[main_table, on = .(gridid = gridid,
                                                   crop   = crop,
                                                   irr    = irr)]
d_ccl_ntill_dt = d_ccl_ntill_dt[!is.na(y_block)]
# weather
d_ccl_ntill_dt = d_ccl_ntill_dt[wth_table, on = .(gridid = gridid)]
d_ccl_ntill_dt = d_ccl_ntill_dt[!is.na(scenario)]
#-----------------------------------------------------------------------------------------
# CATEGORIZE RESPONSE & CROP, IRR, and COMBINE fertilizer and organic N
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
# create joint-outcomes
d_ccg_res_dt[d_s_GHG <= 0 & d_s_cgrain >= 0, target := 'w-w'] # w-w
d_ccg_res_dt[d_s_GHG <= 0 & d_s_cgrain < 0,  target := 'w-l'] # w-l
d_ccg_res_dt[d_s_GHG > 0 & d_s_cgrain >= 0,  target := 'l-w'] # l-w
d_ccg_res_dt[d_s_GHG > 0 & d_s_cgrain < 0,   target := 'l-l'] # l-l
# make target a factor
d_ccg_res_dt[, target := as.factor(target)]

# make crop a factor
d_ccg_res_dt[, crop := as.factor(crop)]

# make irr a factor
d_ccg_res_dt[, irr  := as.factor(irr)]

# combine N amounts, drop individual features
d_ccg_res_dt[, Namt := fertN.amt + orgN.amt]
d_ccg_res_dt = d_ccg_res_dt[, -c('fertN.amt', 'orgN.amt')]

  ## ccl-res ##
# create joint-outcomes
d_ccl_res_dt[d_s_GHG <= 0 & d_s_cgrain >= 0, target := 'w-w'] # w-w
d_ccl_res_dt[d_s_GHG <= 0 & d_s_cgrain < 0,  target := 'w-l'] # w-l
d_ccl_res_dt[d_s_GHG > 0 & d_s_cgrain >= 0,  target := 'l-w'] # l-w
d_ccl_res_dt[d_s_GHG > 0 & d_s_cgrain < 0,   target := 'l-l'] # l-l
# make target a factor
d_ccl_res_dt[, target := as.factor(target)]

# make crop a factor
d_ccl_res_dt[, crop := as.factor(crop)]

# make irr a factor
d_ccl_res_dt[, irr  := as.factor(irr)]

# combine N amounts, drop individual features
d_ccl_res_dt[, Namt := fertN.amt + orgN.amt]
d_ccl_res_dt = d_ccl_res_dt[, -c('fertN.amt', 'orgN.amt')]

  ## ccg-ntill ##
# create joint-outcomes
d_ccg_ntill_dt[d_s_GHG <= 0 & d_s_cgrain >= 0, target := 'w-w'] # w-w
d_ccg_ntill_dt[d_s_GHG <= 0 & d_s_cgrain < 0,  target := 'w-l'] # w-l
d_ccg_ntill_dt[d_s_GHG > 0 & d_s_cgrain >= 0,  target := 'l-w'] # l-w
d_ccg_ntill_dt[d_s_GHG > 0 & d_s_cgrain < 0,   target := 'l-l'] # l-l
# make target a factor
d_ccg_ntill_dt[, target := as.factor(target)]

# make crop a factor
d_ccg_ntill_dt[, crop := as.factor(crop)]

# make irr a factor
d_ccg_ntill_dt[, irr  := as.factor(irr)]

# combine N amounts, drop individual features
d_ccg_ntill_dt[, Namt := fertN.amt + orgN.amt]
d_ccg_ntill_dt = d_ccg_ntill_dt[, -c('fertN.amt', 'orgN.amt')]

  ## ccl-ntill ##
# create joint-outcomes
d_ccl_ntill_dt[d_s_GHG <= 0 & d_s_cgrain >= 0, target := 'w-w'] # w-w
d_ccl_ntill_dt[d_s_GHG <= 0 & d_s_cgrain < 0,  target := 'w-l'] # w-l
d_ccl_ntill_dt[d_s_GHG > 0 & d_s_cgrain >= 0,  target := 'l-w'] # l-w
d_ccl_ntill_dt[d_s_GHG > 0 & d_s_cgrain < 0,   target := 'l-l'] # l-l
# make target a factor
d_ccl_ntill_dt[, target := as.factor(target)]

# make crop a factor
d_ccl_ntill_dt[, crop := as.factor(crop)]

# make irr a factor
d_ccl_ntill_dt[, irr  := as.factor(irr)]

# combine N amounts, drop individual features
d_ccl_ntill_dt[, Namt := fertN.amt + orgN.amt]
d_ccl_ntill_dt = d_ccl_ntill_dt[, -c('fertN.amt', 'orgN.amt')]
#-----------------------------------------------------------------------------------------
# Remove MINERL_sum_ and NITRAT_sum_ outliers by quantile
#-----------------------------------------------------------------------------------------
  ## ccg-res ##
# minerl_q     = quantile(d_ccg_res_dt[, MINERL_sum_], probs = seq(0,1, 0.01))
# d_ccg_res_dt = d_ccg_res_dt[MINERL_sum_ <= minerl_q[[100]] & MINERL_sum_ > minerl_q[[1]],]
# 
# nitrate_q = quantile(d_ccg_res_dt[, NITRAT_sum_], probs = seq(0,1, 0.01))
# d_ccg_res_dt = d_ccg_res_dt[NITRAT_sum_ <= nitrate_q[[100]] & NITRAT_sum_ > nitrate_q[[1]],]

  ## ccl-res ##
# minerl_q     = quantile(d_ccl_res_dt[, MINERL_sum_], probs = seq(0,1, 0.01))
# d_ccl_res_dt = d_ccl_res_dt[MINERL_sum_ <= minerl_q[[100]] & MINERL_sum_ > minerl_q[[1]],]
# 
# nitrate_q = quantile(d_ccl_res_dt[, NITRAT_sum_], probs = seq(0,1, 0.01))
# d_ccl_res_dt = d_ccl_res_dt[NITRAT_sum_ <= nitrate_q[[100]] & NITRAT_sum_ > nitrate_q[[1]],]

  ## ccg-ntill ##
# minerl_q       = quantile(d_ccg_ntill_dt[, MINERL_sum_], probs = seq(0,1, 0.01))
# d_ccg_ntill_dt = d_ccg_ntill_dt[MINERL_sum_ <= minerl_q[[100]] & MINERL_sum_ > minerl_q[[1]],]
# 
# nitrate_q     = quantile(d_ccg_ntill_dt[, NITRAT_sum_], probs = seq(0,1, 0.01))
# d_ccg_ntill_dt = d_ccg_ntill_dt[NITRAT_sum_ <= nitrate_q[[100]] & NITRAT_sum_ > nitrate_q[[1]],]

  ## ccl-ntill ##
minerl_q       = quantile(d_ccl_ntill_dt[, MINERL_sum_], probs = seq(0,1, 0.01))
d_ccl_ntill_dt = d_ccl_ntill_dt[MINERL_sum_ <= minerl_q[[100]] & MINERL_sum_ > minerl_q[[1]],]

nitrate_q      = quantile(d_ccl_ntill_dt[, NITRAT_sum_], probs = seq(0,1, 0.01))
d_ccl_ntill_dt = d_ccl_ntill_dt[NITRAT_sum_ <= nitrate_q[[100]] & NITRAT_sum_ > nitrate_q[[1]],]
#-----------------------------------------------------------------------------------------
# RANDOM FOREST & SHAP
#-----------------------------------------------------------------------------------------
# ccg_res_SHAP = rf_shap(d_ccg_res_dt)
# save(ccg_res_SHAP,  file = paste(out_p, "ccg_res_SHAP.Rdata", sep = '/'))
# 
# ccl_res_SHAP = rf_shap(d_ccl_res_dt)
# save(ccl_res_SHAP,  file = paste(out_p, "ccl_res_SHAP.Rdata", sep = '/'))
# 
# ccg_ntill_SHAP = rf_shap(d_ccg_ntill_dt)
# save(ccg_ntill_SHAP,  file = paste(out_p, "ccg_ntill_SHAP.Rdata", sep = '/'))
 
ccl_ntill_SHAP = rf_shap(d_ccl_ntill_dt)
save(ccl_ntill_SHAP,  file = paste(out_p, "ccl_ntill_SHAP.Rdata", sep = '/'))


# test_viz = shapviz(ccg_ntill_SHAP$overall_SHAP)
# sv_importance(test_viz) # overall
# sv_dependence(test_viz, 'crop', color_var = NULL)
# 
# test_ww_viz = shapviz(ccg_ntill_SHAP$class_SHAP[['w-w']])
# sv_importance(test_ww_viz)
# sv_dependence(test_ww_viz, 'NITRAT_sum_', color_var = NULL)
# 
# test_wl_viz = shapviz(ccg_ntill_SHAP$class_SHAP[['w-l']])
# sv_importance(test_wl_viz)
# sv_dependence(test_wl_viz, 'crop', color_var = NULL)
# 
# test_lw_viz = shapviz(ccg_ntill_SHAP$class_SHAP[['l-w']])
# sv_importance(test_lw_viz)
# sv_dependence(test_lw_viz, 'Namt', color_var = NULL)
# 
# test_ll_viz = shapviz(ccg_ntill_SHAP$class_SHAP[['l-l']])
# sv_importance(test_ll_viz)
# sv_dependence(test_ll_viz, 'SLBLKD', color_var = NULL)
