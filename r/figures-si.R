# filename:    figures-si.R
# created:     19 December 2025
# updated:     30 March 2025
# author:      S.C. McClelland
# description: This file creates figures included in the SI of the manuscript.
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(colorspace)
library(cowplot)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(grid)
library(gridtext)
library(gridExtra)
library(maptools)
library(RColorBrewer)
library(rstudioapi)
library(patchwork)
library(scales)
library(sf)
library(shapviz)
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
source('r/figure-functions.R')
source('r/plot_discrete_color_bar.R')
data_p  = paste(dir, 'data/analysis-output', sep = '/')
input_p = paste(dir, 'data/daycent-post-processed', sep = '/')
out_p   = paste(dir, 'figures/si', sep = '/')
#-----------------------------------------------------------------------------------------
# Figure S1-S4. Hectare responses
#-----------------------------------------------------------------------------------------
# NOTE: DUE TO LARGE DATA SIZE NEED TO COMPLETE FOR EACH RESPONSE SEPARATELY
# SOC | uncertainty data
{# ccg-res
soc_u_ccg_res = fread(paste(data_p, 'ccg-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
soc_u_ccg_res = soc_u_ccg_res[, .(scenario, y_block, d_s_SOC, IPCC_NAME)]
gc()
# ccl-res
soc_u_ccl_res = fread(paste(data_p, 'ccl-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
soc_u_ccl_res = soc_u_ccl_res[, .(scenario, y_block, d_s_SOC, IPCC_NAME)]
gc()
# ccg-ntill
soc_u_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
soc_u_ccg_ntill = soc_u_ccg_ntill[, .(scenario, y_block, d_s_SOC, IPCC_NAME)]
gc()
# ccl-ntill
soc_u_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
soc_u_ccl_ntill = soc_u_ccl_ntill[, .(scenario, y_block, d_s_SOC, IPCC_NAME)]
gc()
# combine
soc_u = rbind(soc_u_ccg_res, soc_u_ccl_res, soc_u_ccg_ntill, soc_u_ccl_ntill)
gc()
# rm
rm(soc_u_ccg_res, soc_u_ccl_res, soc_u_ccg_ntill, soc_u_ccl_ntill)
gc()

# flip signs
soc_u = soc_u[, d_s_SOC := ifelse(d_s_SOC < 0, d_s_SOC*-1, d_s_SOC*-1)]
gc()

# annual estimates
soc_u_2050 = soc_u[y_block == 2050, lapply(.SD, function(x) {x/35}), 
                   .SDcols = c('d_s_SOC'),
                   by = .(scenario, y_block, IPCC_NAME)]
gc()
soc_u_2100 = soc_u[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                   .SDcols = c('d_s_SOC'),
                   by = .(scenario, y_block, IPCC_NAME)]
rm(soc_u)
gc()
# create GLB level
soc_u_2050_g = copy(soc_u_2050)
soc_u_2050_g[, IPCC_NAME := 'GLB']
soc_u_2100_g = copy(soc_u_2100)
soc_u_2100_g[, IPCC_NAME := 'GLB']
# combine
soc_u = rbind(soc_u_2050, soc_u_2050_g, soc_u_2100, soc_u_2100_g)
rm(soc_u_2050, soc_u_2050_g, soc_u_2100, soc_u_2100_g)
gc()

# estimate bw data
# median
soc_u_med = soc_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[3]}), .SDcols = 'd_s_SOC',
                  by = .(scenario, y_block, IPCC_NAME)]
setnames(soc_u_med, 'd_s_SOC', 'median_SOC')
# q1
soc_u_q1 = soc_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[2]}), 
                 .SDcols = 'd_s_SOC',
                 by = .(scenario, y_block, IPCC_NAME)]
setnames(soc_u_q1, 'd_s_SOC', 'Q1_SOC')
# q3
soc_u_q3 = soc_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[4]}), 
                 .SDcols = 'd_s_SOC',
                 by = .(scenario, y_block, IPCC_NAME)]
setnames(soc_u_q3, 'd_s_SOC', 'Q3_SOC')
# combine and calculate remaining
soc_u_bw = cbind(soc_u_med, soc_u_q1[,.(Q1_SOC)], soc_u_q3[,.(Q3_SOC)])
rm(soc_u, soc_u_med, soc_u_q1, soc_u_q3)
# iqr
soc_u_bw[, IQR_SOC := Q3_SOC - Q1_SOC]
# min
soc_u_bw[, min_SOC := Q1_SOC - IQR_SOC*1.5]
# max
soc_u_bw[, max_SOC := Q3_SOC + IQR_SOC*1.5]
gc()

# SOC | w mean and SE
soc_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-ghg-responses.csv', sep = '/'))
soc_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-ghg-responses.csv', sep = '/'))
soc_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-responses.csv', sep = '/'))
soc_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-responses.csv', sep = '/'))

# combine
soc = rbind(soc_ccg_res, soc_ccl_res, soc_ccg_ntill, soc_ccl_ntill)
rm(soc_ccg_res, soc_ccl_res, soc_ccg_ntill, soc_ccl_ntill)

# flip signs for soc
soc = soc[, d_s_SOC := ifelse(d_s_SOC < 0, d_s_SOC*-1, d_s_SOC*-1)]

# annual estimates
soc_2050 = soc[y_block == 2050, lapply(.SD, function(x) {x/35}),
               .SDcols = c('d_s_SOC', 'se_s_SOC'),
               by = .(scenario, y_block, IPCC_NAME)]
soc_2100 = soc[y_block == 2100, lapply(.SD, function(x) {x/85}),
               .SDcols = c('d_s_SOC', 'se_s_SOC'),
               by = .(scenario, y_block, IPCC_NAME)]
soc      = rbind(soc_2050, soc_2100)
rm(soc_2050, soc_2100)
gc()

# PLOT
figs1 = bwplot_fig(soc, soc_u_bw, 'SOC')
ggsave(paste(out_p, 'figure1-si.pdf', sep = '/'), figs1$soc, units = 'mm', width = 180, height = 180, device='pdf', dpi=600)
}
# N2O | uncertainty data
{# ccg-res
n2o_u_ccg_res = fread(paste(data_p, 'ccg-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
n2o_u_ccg_res = n2o_u_ccg_res[, .(scenario, y_block, d_s_N2O, IPCC_NAME)]
gc()
# ccl-res
n2o_u_ccl_res = fread(paste(data_p, 'ccl-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
n2o_u_ccl_res = n2o_u_ccl_res[, .(scenario, y_block, d_s_N2O, IPCC_NAME)]
gc()
# ccg-ntill
n2o_u_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
n2o_u_ccg_ntill = n2o_u_ccg_ntill[, .(scenario, y_block, d_s_N2O, IPCC_NAME)]
gc()
# ccl-ntill
n2o_u_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
n2o_u_ccl_ntill = n2o_u_ccl_ntill[, .(scenario, y_block, d_s_N2O, IPCC_NAME)]
gc()
# combine
n2o_u = rbind(n2o_u_ccg_res, n2o_u_ccl_res, n2o_u_ccg_ntill, n2o_u_ccl_ntill)
gc()
# rm
rm(n2o_u_ccg_res, n2o_u_ccl_res, n2o_u_ccg_ntill, n2o_u_ccl_ntill)
gc()

# flip signs
n2o_u = n2o_u[, d_s_N2O := ifelse(d_s_N2O < 0, d_s_N2O*-1, d_s_N2O*-1)]
gc()

# annual estimates
n2o_u_2050 = n2o_u[y_block == 2050, lapply(.SD, function(x) {x/35}), 
                   .SDcols = c('d_s_N2O'),
                   by = .(scenario, y_block, IPCC_NAME)]
gc()
n2o_u_2100 = n2o_u[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                   .SDcols = c('d_s_N2O'),
                   by = .(scenario, y_block, IPCC_NAME)]
rm(n2o_u)
gc()
# create GLB level
n2o_u_2050_g = copy(n2o_u_2050)
n2o_u_2050_g[, IPCC_NAME := 'GLB']
n2o_u_2100_g = copy(n2o_u_2100)
n2o_u_2100_g[, IPCC_NAME := 'GLB']
# combine
n2o_u = rbind(n2o_u_2050, n2o_u_2050_g, n2o_u_2100, n2o_u_2100_g)
rm(n2o_u_2050, n2o_u_2050_g, n2o_u_2100, n2o_u_2100_g)
gc()

# estimate bw data
# median
n2o_u_med = n2o_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[3]}), .SDcols = 'd_s_N2O',
                  by = .(scenario, y_block, IPCC_NAME)]
setnames(n2o_u_med, 'd_s_N2O', 'median_N2O')
# q1
n2o_u_q1 = n2o_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[2]}), 
                 .SDcols = 'd_s_N2O',
                 by = .(scenario, y_block, IPCC_NAME)]
setnames(n2o_u_q1, 'd_s_N2O', 'Q1_N2O')
# q3
n2o_u_q3 = n2o_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[4]}), 
                 .SDcols = 'd_s_N2O',
                 by = .(scenario, y_block, IPCC_NAME)]
setnames(n2o_u_q3, 'd_s_N2O', 'Q3_N2O')
# combine and calculate remaining
n2o_u_bw = cbind(n2o_u_med, n2o_u_q1[,.(Q1_N2O)], n2o_u_q3[,.(Q3_N2O)])
rm(n2o_u, n2o_u_med, n2o_u_q1, n2o_u_q3)
# iqr
n2o_u_bw[, IQR_N2O := Q3_N2O - Q1_N2O]
# min
n2o_u_bw[, min_N2O := Q1_N2O - IQR_N2O*1.5]
# max
n2o_u_bw[, max_N2O := Q3_N2O + IQR_N2O*1.5]
gc()

# N2O | w mean and SE
n2o_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-ghg-responses.csv', sep = '/'))
n2o_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-ghg-responses.csv', sep = '/'))
n2o_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-responses.csv', sep = '/'))
n2o_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-responses.csv', sep = '/'))

# combine
n2o = rbind(n2o_ccg_res, n2o_ccl_res, n2o_ccg_ntill, n2o_ccl_ntill)
rm(n2o_ccg_res, n2o_ccl_res, n2o_ccg_ntill, n2o_ccl_ntill)

# flip signs for n2o
n2o = n2o[, d_s_N2O := ifelse(d_s_N2O < 0, d_s_N2O*-1, d_s_N2O*-1)]

# annual estimates
n2o_2050 = n2o[y_block == 2050, lapply(.SD, function(x) {x/35}),
               .SDcols = c('d_s_N2O', 'se_s_N2O'),
               by = .(scenario, y_block, IPCC_NAME)]
n2o_2100 = n2o[y_block == 2100, lapply(.SD, function(x) {x/85}),
               .SDcols = c('d_s_N2O', 'se_s_N2O'),
               by = .(scenario, y_block, IPCC_NAME)]
n2o      = rbind(n2o_2050, n2o_2100)
rm(n2o_2050, n2o_2100)
gc()

# PLOT
figs2 = bwplot_fig(n2o, n2o_u_bw, 'N2O')
ggsave(paste(out_p, 'figure2-si.pdf', sep = '/'), figs2$n2o, units = 'mm', width = 180, height = 180, device='pdf', dpi=600)
}
# GHG | uncertainty data
{# ccg-res
ghg_u_ccg_res = fread(paste(data_p, 'ccg-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
ghg_u_ccg_res = ghg_u_ccg_res[, .(scenario, y_block, d_s_GHG, IPCC_NAME)]
gc()
# ccl-res
ghg_u_ccl_res = fread(paste(data_p, 'ccl-res-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
ghg_u_ccl_res = ghg_u_ccl_res[, .(scenario, y_block, d_s_GHG, IPCC_NAME)]
gc()
# ccg-ntill
ghg_u_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
ghg_u_ccg_ntill = ghg_u_ccg_ntill[, .(scenario, y_block, d_s_GHG, IPCC_NAME)]
gc()
# ccl-ntill
ghg_u_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-all-uncertainty-responses.csv', sep = '/'))
# simplify cols 
ghg_u_ccl_ntill = ghg_u_ccl_ntill[, .(scenario, y_block, d_s_GHG, IPCC_NAME)]
gc()
# combine
ghg_u = rbind(ghg_u_ccg_res, ghg_u_ccl_res, ghg_u_ccg_ntill, ghg_u_ccl_ntill)
gc()
# rm
rm(ghg_u_ccg_res, ghg_u_ccl_res, ghg_u_ccg_ntill, ghg_u_ccl_ntill)
gc()

# flip signs
ghg_u = ghg_u[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]
gc()

# annual estimates
ghg_u_2050 = ghg_u[y_block == 2050, lapply(.SD, function(x) {x/35}), 
     .SDcols = c('d_s_GHG'),
     by = .(scenario, y_block, IPCC_NAME)]
gc()
ghg_u_2100 = ghg_u[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                   .SDcols = c('d_s_GHG'),
                   by = .(scenario, y_block, IPCC_NAME)]
rm(ghg_u)
gc()
# create GLB level
ghg_u_2050_g = copy(ghg_u_2050)
ghg_u_2050_g[, IPCC_NAME := 'GLB']
ghg_u_2100_g = copy(ghg_u_2100)
ghg_u_2100_g[, IPCC_NAME := 'GLB']
# combine
ghg_u = rbind(ghg_u_2050, ghg_u_2050_g, ghg_u_2100, ghg_u_2100_g)
rm(ghg_u_2050, ghg_u_2050_g, ghg_u_2100, ghg_u_2100_g)
gc()

# estimate bw data
# median
ghg_u_med = ghg_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[3]}), .SDcols = 'd_s_GHG',
                  by = .(scenario, y_block, IPCC_NAME)]
setnames(ghg_u_med, 'd_s_GHG', 'median_GHG')
# q1
ghg_u_q1 = ghg_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[2]}), 
                  .SDcols = 'd_s_GHG',
                  by = .(scenario, y_block, IPCC_NAME)]
setnames(ghg_u_q1, 'd_s_GHG', 'Q1_GHG')
# q3
ghg_u_q3 = ghg_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[4]}), 
                 .SDcols = 'd_s_GHG',
                 by = .(scenario, y_block, IPCC_NAME)]
setnames(ghg_u_q3, 'd_s_GHG', 'Q3_GHG')
# combine and calculate remaining
ghg_u_bw = cbind(ghg_u_med, ghg_u_q1[,.(Q1_GHG)], ghg_u_q3[,.(Q3_GHG)])
rm(ghg_u, ghg_u_med, ghg_u_q1, ghg_u_q3)
# iqr
ghg_u_bw[, IQR_GHG := Q3_GHG - Q1_GHG]
# min
ghg_u_bw[, min_GHG := Q1_GHG - IQR_GHG*1.5]
# max
ghg_u_bw[, max_GHG := Q3_GHG + IQR_GHG*1.5]
gc()

# GHG | w mean and SE
ghg_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-ghg-responses.csv', sep = '/'))
ghg_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-ghg-responses.csv', sep = '/'))
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-responses.csv', sep = '/'))
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-responses.csv', sep = '/'))

# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# flip signs for ghg
ghg = ghg[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]

# annual estimates
ghg_2050 = ghg[y_block == 2050, lapply(.SD, function(x) {x/35}),
                 .SDcols = c('d_s_GHG', 'se_s_GHG'),
                 by = .(scenario, y_block, IPCC_NAME)]
ghg_2100 = ghg[y_block == 2100, lapply(.SD, function(x) {x/85}),
                 .SDcols = c('d_s_GHG', 'se_s_GHG'),
                 by = .(scenario, y_block, IPCC_NAME)]
ghg      = rbind(ghg_2050, ghg_2100)
rm(ghg_2050, ghg_2100)
gc()

# PLOT
figs3 = bwplot_fig(ghg, ghg_u_bw, 'GHG')
ggsave(paste(out_p, 'figure3-si.pdf', sep = '/'), figs3$ghg, units = 'mm', width = 180, height = 180, device='pdf', dpi=600)
}
# YIELD | variance data
{# ccg-res
  yield_u_ccg_res = fread(paste(data_p, 'ccg-res-hectare-yield-all-gcm-responses.csv', sep = '/'))
  # simplify cols 
  yield_u_ccg_res = yield_u_ccg_res[, .(scenario, y_block, d_s_cgrain, IPCC_NAME)]
  gc()
  # ccl-res
  yield_u_ccl_res = fread(paste(data_p, 'ccl-res-hectare-yield-all-gcm-responses.csv', sep = '/'))
  # simplify cols 
  yield_u_ccl_res = yield_u_ccl_res[, .(scenario, y_block, d_s_cgrain, IPCC_NAME)]
  gc()
  # ccg-ntill
  yield_u_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-yield-all-gcm-responses.csv', sep = '/'))
  # simplify cols 
  yield_u_ccg_ntill = yield_u_ccg_ntill[, .(scenario, y_block, d_s_cgrain, IPCC_NAME)]
  gc()
  # ccl-ntill
  yield_u_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-yield-all-gcm-responses.csv', sep = '/'))
  # simplify cols 
  yield_u_ccl_ntill = yield_u_ccl_ntill[, .(scenario, y_block, d_s_cgrain, IPCC_NAME)]
  gc()
  # combine
  yield_u = rbind(yield_u_ccg_res, yield_u_ccl_res, yield_u_ccg_ntill, yield_u_ccl_ntill)
  gc()
  # rm
  rm(yield_u_ccg_res, yield_u_ccl_res, yield_u_ccg_ntill, yield_u_ccl_ntill)
  gc()
  
  # annual estimates
  yield_u_2050 = yield_u[y_block == 2050, lapply(.SD, function(x) {x/35}), 
                     .SDcols = c('d_s_cgrain'),
                     by = .(scenario, y_block, IPCC_NAME)]
  gc()
  yield_u_2100 = yield_u[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                     .SDcols = c('d_s_cgrain'),
                     by = .(scenario, y_block, IPCC_NAME)]
  rm(yield_u)
  gc()
  # create GLB level
  yield_u_2050_g = copy(yield_u_2050)
  yield_u_2050_g[, IPCC_NAME := 'GLB']
  yield_u_2100_g = copy(yield_u_2100)
  yield_u_2100_g[, IPCC_NAME := 'GLB']
  # combine
  yield_u = rbind(yield_u_2050, yield_u_2050_g, yield_u_2100, yield_u_2100_g)
  rm(yield_u_2050, yield_u_2050_g, yield_u_2100, yield_u_2100_g)
  gc()
  
  # estimate bw data
  # median
  yield_u_med = yield_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[3]}), .SDcols = 'd_s_cgrain',
                    by = .(scenario, y_block, IPCC_NAME)]
  setnames(yield_u_med, 'd_s_cgrain', 'median_cgrain')
  # q1
  yield_u_q1 = yield_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[2]}), 
                   .SDcols = 'd_s_cgrain',
                   by = .(scenario, y_block, IPCC_NAME)]
  setnames(yield_u_q1, 'd_s_cgrain', 'Q1_cgrain')
  # q3
  yield_u_q3 = yield_u[, lapply(.SD, function(x){ quantile(x, probs = seq(0,1,0.25))[4]}), 
                   .SDcols = 'd_s_cgrain',
                   by = .(scenario, y_block, IPCC_NAME)]
  setnames(yield_u_q3, 'd_s_cgrain', 'Q3_cgrain')
  # combine and calculate remaining
  yield_u_bw = cbind(yield_u_med, yield_u_q1[,.(Q1_cgrain)], yield_u_q3[,.(Q3_cgrain)])
  rm(yield_u, yield_u_med, yield_u_q1, yield_u_q3)
  # iqr
  yield_u_bw[, IQR_cgrain := Q3_cgrain - Q1_cgrain]
  # min
  yield_u_bw[, min_cgrain := Q1_cgrain - IQR_cgrain*1.5]
  # max
  yield_u_bw[, max_cgrain := Q3_cgrain + IQR_cgrain*1.5]
  gc()
  
  # YIELD | w mean and se
  yield_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-yield-responses.csv', sep = '/'))
  yield_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-yield-responses.csv', sep = '/'))
  yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-yield-responses.csv', sep = '/'))
  yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-yield-responses.csv', sep = '/'))
  
  # combine
  yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
  rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
  
  # annual estimates
  yield_2050 = yield[y_block == 2050, lapply(.SD, function(x) {x/35}),
                 .SDcols = c('d_s_cgrain', 'se_s_cgrain'),
                 by = .(scenario, y_block, IPCC_NAME)]
  yield_2100 = yield[y_block == 2100, lapply(.SD, function(x) {x/85}),
                 .SDcols = c('d_s_cgrain', 'se_s_cgrain'),
                 by = .(scenario, y_block, IPCC_NAME)]
  yield      = rbind(yield_2050, yield_2100)
  rm(yield_2050, yield_2100)
  gc()
  
  # PLOT
  figs4 = bwplot_fig(yield, yield_u_bw, 'yield')
  ggsave(paste(out_p, 'figure4-si.pdf', sep = '/'), figs4$yield, units = 'mm', width = 180, height = 180, device='pdf', dpi=600)
}
#-----------------------------------------------------------------------------------------
# Figure S5-S6. CoV maps (2050, 2100)
#-----------------------------------------------------------------------------------------
# express as % (SD/abs(mean))*100
# plot 0 to > 100

# load data | N.B. does not include imputed values
# GHG
ghg_ccg_res   = fread(paste(data_p, 'ccg-res-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccl_res   = fread(paste(data_p, 'ccl-res-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))

# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# flip signs for ghg
ghg = ghg[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]

# keep only ghg
ghg = ghg[, -c('d_s_SOC', 'sd_s_SOC', 'd_s_N2O', 'sd_s_N2O')]

# add xy coordinates
## input table ##
load(paste(input_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep coordinates
main_table      = main_table[, c('gridid', 'x', 'y')]
main_table      = unique(main_table)
# join
ghg = ghg[main_table, on = .(gridid = gridid)]
ghg = ghg[!is.na(scenario)]

# estimate CoV as percent
ghg[, cov_s_GHG := (sd_s_GHG/abs(d_s_GHG))*100]

  ## 2050 ##
# ccg-res
ccg_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccg-res' &
                                           y_block == 2050])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC', '(a)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccl-res
ccl_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccl-res' &
                                          y_block == 2050])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC', '(e)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccg-ntill' &
                                      y_block == 2050])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC + Ntill', '(c)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccl-ntill' &
                                      y_block == 2050])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC + Ntill', '(g)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))

# YIELD
# load data | N.B. does not include imputed values 
yield_ccg_res   = fread(paste(data_p, 'ccg-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_res   = fread(paste(data_p, 'ccl-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-weighted-gridid-yield-responses.csv', sep = '/'))

# combine
yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)

# add xy coordinates
# join
yield = yield[main_table, on = .(gridid = gridid)]
yield = yield[!is.na(scenario)]

# estimate CoV as percent
yield[, cov_s_cgrain := (sd_s_cgrain/abs(d_s_cgrain))*100]

  ## 2050 ##
# ccg-res
ccg_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccg-res' &
                                                   y_block == 2050,])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(b)') +
  theme(plot.subtitle = element_text(size = 6))
# ccl-res
ccl_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccl-res' & 
                                              y_block == 2050])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(f)') +
  theme(plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccg-ntill' &
                                              y_block == 2050])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(d)') +
  theme(plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccl-ntill' &
                                              y_block == 2050])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(h)') +
  theme(plot.subtitle = element_text(size = 6))
# Multi-panel figure
figs5_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.15), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure5-si.pdf', sep = '/'), figs5_maps,  units = 'mm', width = 180, height = 225, device='pdf', dpi=300)

  ## 2100 ##
# GHG
# ccg-res
ccg_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccg-res' &
                                          y_block == 2100])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC', '(a)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccl-res
ccl_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccl-res' &
                                          y_block == 2100])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC', '(e)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccg-ntill' &
                                          y_block == 2100])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC + Ntill', '(c)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccl-ntill' &
                                          y_block == 2100])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
  #          hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC + Ntill', '(g)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# YIELD
# ccg-res
ccg_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccg-res' &
                                              y_block == 2100,])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(b)') +
  theme(plot.subtitle = element_text(size = 6))
# ccl-res
ccl_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccl-res' & 
                                              y_block == 2100])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(f)') +
  theme(plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccg-ntill' &
                                              y_block == 2100])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(d)') +
  theme(plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccl-ntill' &
                                              y_block == 2100])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
  #          hjust = 0, vjust = 1, size = 4)
  ggtitle('', '(h)') +
  theme(plot.subtitle = element_text(size = 6))

# Multi-panel figure
figs6_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.15), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure6-si.pdf', sep = '/'), figs6_maps,  units = 'mm', width = 180, height = 225, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure S7. Top features, climate-favorable, yield-favorable, neither favorable
#-----------------------------------------------------------------------------------------
# data
load(paste(data_p, 'ccg_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccg_ntill_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_ntill_SHAP.Rdata', sep = '/'))

# feature colors
f_colors = c('Climate'    = "#201158",
             'Management' = "#007054",
             'Site'       = "#C19A1B",
             'Soil'       = "#FFCEF4")

  ## ccg-res ##
# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-l`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Initial Soil Nitrate', 'Nitrogen Inputs', 'Soil Bulk Density',
                'Initial Residue Fraction', 'Cash Crop')
# types (actual order)
ccg_res_t   = c('Management', 'Management', 'Soil', 'Management', 'Site')
# plot
ccg_res_fig_wl = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                        ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_wl = ccg_res_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("Grass CC","(a)")

# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`l-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Water Management', 'Soil Bulk Density', 'Initial Residue Fraction',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccg_res_t   = c('Management', 'Management', 'Management', 'Soil', 'Management')
# plot
ccg_res_fig_lw = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                           ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_lw = ccg_res_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(b)")

# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`l-l`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Initial Residue Fraction', 'Water Management', 'Nitrogen Inputs',
                'Initial Soil Nitrate', 'Soil Bulk Density')
# types (actual order)
ccg_res_t   = c('Soil', 'Site', 'Management', 'Management', 'Management')
# plot
ccg_res_fig_ll = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                           ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_ll = ccg_res_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(c)")

  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-l`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Nitrogen Inputs', 'Soil Bulk Density', 'Initial Soil Nitrate',
                'Initial Residue Fraction', 'Cash Crop')
# types (actual order)
ccg_ntill_t   = c('Management', 'Management', 'Site', 'Soil', 'Management')
# plot
ccg_ntill_fig_wl = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_wl = ccg_ntill_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("Grass CC + Ntill","(d)")

# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`l-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Mean Diurnal Range', 'Initial Soil Nitrate', 'Initial Residue Fraction',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccg_ntill_t   = c('Management', 'Management', 'Management', 'Site', 'Climate')
# plot
ccg_ntill_fig_lw = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_lw = ccg_ntill_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(e)")

# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`l-l`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Soil pH', 'Initial Residue Fraction', 'Initial Soil Water Content',
                'Cash Crop', 'Soil Bulk Density')
# types (actual order)
ccg_ntill_t   = c('Soil', 'Management', 'Site', 'Management', 'Soil')
# plot
ccg_ntill_fig_ll = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_ll = ccg_ntill_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(f)")

  ## ccl-res ##
# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`w-l`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Soil Bulk Density', 'Soil pH', 'Nitrogen Inputs',
                'Initial Soil Nitrate', 'Cash Crop')
# types (actual order)
ccl_res_t   = c('Management', 'Site', 'Management', 'Soil', 'Soil')
# plot
ccl_res_fig_wl = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_wl = ccl_res_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("Legume CC","(g)")

# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`l-w`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Initial Relative Soil Water', 'Water Management', 'Initial Soil Nitrate',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccl_res_t   = c('Management', 'Management', 'Site', 'Management', 'Site')
# plot
ccl_res_fig_lw = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_lw = ccl_res_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(h)")

# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`l-l`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Soil Bulk Density', 'Cash Crop', 'Water Management',
                'Initial Soil Nitrate', 'Nitrogen Inputs')
# types (actual order)
ccl_res_t   = c('Management', 'Site', 'Management', 'Management', 'Soil')
# plot
ccl_res_fig_ll = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_ll = ccl_res_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(i)")

  ## ccl-ntill ##
# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`w-l`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Water Management', 'Initial Residue Fraction', 'Nitrogen Inputs',
                  'Initial Soil Nitrate', 'Cash Crop')
# types (actual order)
ccl_ntill_t   = c('Management', 'Site', 'Management', 'Management', 'Management')
# plot
ccl_ntill_fig_wl = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_wl = ccl_ntill_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("Legume CC + Ntill", "(j)")

# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`l-w`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Mean Diurnal Range', 'Initial Residue Return', 'Initial Soil Nitrate',
                  'Nitrogen Inputs', 'Cash rop')
# types (actual order)
ccl_ntill_t   = c('Management', 'Management', 'Site', 'Management', 'Climate')
# plot
ccl_ntill_fig_lw = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_lw = ccl_ntill_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("", "(k)")

# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`l-l`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Initial Soil Nitrate', 'Cash Crop', 'Nitrogen Inputs',
                  'Initial Soil Water', 'Soil Bulk Density')
# types (actual order)
ccl_ntill_t   = c('Soil', 'Site', 'Management', 'Management', 'Site')
# plot
ccl_ntill_fig_ll = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_ll = ccl_ntill_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("","(l)")

fig7_final = ggarrange(ccg_res_fig_wl, ccg_res_fig_lw, ccg_res_fig_ll, 
                       ccg_ntill_fig_wl, ccg_ntill_fig_lw, ccg_ntill_fig_ll, 
                       ccl_res_fig_wl, ccl_res_fig_lw, ccl_res_fig_ll, 
                       ccl_ntill_fig_wl, ccl_ntill_fig_lw, ccl_ntill_fig_ll, 
                       ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
# save
ggsave(paste(out_p, 'figure7-si.pdf', sep = '/'), fig7_final, units = 'mm', width = 180, height = 225, device='pdf', dpi=300)
