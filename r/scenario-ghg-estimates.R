# file name:    scenario-ghg-estimates.R
# created:      14 December 2024
# last updated: 16 December 2024

# description: This file combines uncertainty output for each scenario (absolute responses) and
#              estimates differences in responses between scenario and continued practice (conv).
#              Units are converted from g C or g N m-2 for somsc and direct N2O, respectively, to
#              Mg CO2-eq ha-1 and indirect N2O is converted from Mg N2O ha-1 to CO2-eq.
# note:        Run each scenario + conv separately (large amount of data)
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# FUNCTIONS & CONSTANTS
#-----------------------------------------------------------------------------------------
delta_soc_co2_eq          = function(delta_somsc) { # negative output indicates soil C sink
  gmwCO2 = 44
  gmwC   = 12
  Mg_ha  = 100
  delta_soc_co2_eq        = ((-delta_somsc)*(gmwCO2/gmwC))/Mg_ha 
}
direct_N2O_co2_eq         = function(N2Oflux) {
  gwpN2O = 273
  gmwN2O = 44
  gmwN   = 14
  Mg_ha  = 100
  direct_N2O_co2_eq       = (N2Oflux*gwpN2O*(gmwN2O/(2*gmwN)))/Mg_ha
}
gwpN2O = 273
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FILES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
u_data   = paste(dir, 'data/uncertainty-output', sep = '/')
#-----------------------------------------------------------------------------------------
# LOAD FILES
#-----------------------------------------------------------------------------------------
### ALWAYS LOAD ###
  ## conv ##
# somsc g C m-2
conv_somsc    = fread(paste(u_data, 's_somsc-uncertainty-ssp370-conv.csv', sep = '/'))
gc()
# dN2O g N m-2
conv_dN2O     = fread(paste(u_data, 's_N2Oflux-uncertainty-ssp370-conv.csv', sep = '/'))
gc()
# iN2O Mg iN2O ha-1
conv_iN2O     = fread(paste(u_data, 's_iN2Oflux-uncertainty-ssp370-conv.csv', sep = '/'))
gc()

### These should be loaded and executed one at a time because of file size ###  
{
  ## ccg-res ##
# # somsc g C m-2
# ccg_res_somsc1 = fread(paste(u_data, 's_somsc-uncertainty-ssp370-ccg-res.csv', sep = '/'))
# gc()
# ccg_res_somsc2 = fread(paste(u_data, 's_somsc-uncertainty-ssp370-soyb-wht-ccg-res.csv', sep = '/'))
# gc()
# ccg_res_somsc  = rbind(ccg_res_somsc1, ccg_res_somsc2)
# rm(ccg_res_somsc1, ccg_res_somsc2)
# gc()
# # dN2O g N m-2
# ccg_res_dN2O  = fread(paste(u_data, 's_N2Oflux-uncertainty-ssp370-ccg-res.csv', sep = '/'))
# gc()
# # iN2O Mg iN2O ha-1
# ccg_res_iN2O  = fread(paste(u_data, 's_iN2Oflux-uncertainty-ssp370-ccg-res.csv', sep = '/'))
# gc()
}
{
  ## ccl-res ##
# # somsc g C m-2
# ccl_res_somsc = fread(paste(u_data, 's_somsc-uncertainty-ssp370-ccl-res.csv', sep = '/'))
# gc()
# # dN2O g N m-2
# ccl_res_dN2O  = fread(paste(u_data, 's_N2Oflux-uncertainty-ssp370-ccl-res.csv', sep = '/'))
# gc()
# # iN2O Mg iN2O ha-1
# ccl_res_iN2O  = fread(paste(u_data, 's_iN2Oflux-uncertainty-ssp370-ccl-res.csv', sep = '/'))
# gc()
}
{
  ## ccg-ntill ##
# # somsc g C m-2
# ccg_ntill_somsc = fread(paste(u_data, 's_somsc-uncertainty-ssp370-ccg-ntill.csv', sep = '/'))
# ccg_ntill_somsc = unique(ccg_ntill_somsc)
# gc()
# # dN2O g N m-2
# ccg_ntill_dN2O  = fread(paste(u_data, 's_N2Oflux-uncertainty-ssp370-ccg-ntill.csv', sep = '/'))
# gc()
# # iN2O Mg iN2O ha-1
# ccg_ntill_iN2O  = fread(paste(u_data, 's_iN2Oflux-uncertainty-ssp370-ccg-ntill.csv', sep = '/'))
# gc()
}
{
  ## ccl-ntill ##
# somsc g C m-2
ccl_ntill_somsc = fread(paste(u_data, 's_somsc-uncertainty-ssp370-ccl-ntill.csv', sep = '/'))
ccl_ntill_somsc = unique(ccl_ntill_somsc)
gc()
# dN2O g N m-2
ccl_ntill_dN2O  = fread(paste(u_data, 's_N2Oflux-uncertainty-ssp370-ccl-ntill.csv', sep = '/'))
gc()
# iN2O Mg iN2O ha-1
ccl_ntill_iN2O  = fread(paste(u_data, 's_iN2Oflux-uncertainty-ssp370-ccl-ntill.csv', sep = '/'))
gc()
}
#-----------------------------------------------------------------------------------------
# UNIT CONVERSIONS
#-----------------------------------------------------------------------------------------
### ALWAYS USE ###
  ## conv ##
# somsc to Mg CO2-eq ha-1
conv_somsc[, s_SOC   := delta_soc_co2_eq(s_somsc)]
conv_somsc[, s_somsc := NULL]
conv_somsc[, rep     := gsub("somsc_", "", rep)]
gc()
# dN2O
conv_dN2O[, s_dN2O    := direct_N2O_co2_eq(s_N2Oflux)]
conv_dN2O[, s_N2Oflux := NULL]
conv_dN2O[, rep       := gsub("dN2O_", "", rep)]
gc()
# iN2O
conv_iN2O[, s_iN2O     := as.numeric(s_iN2Oflux)*gwpN2O]
conv_iN2O[, s_iN2Oflux := NULL]
conv_iN2O[, rep        := gsub("iN2O_", "", rep)]
gc()

### These should be uncommented depending upon scenario ###  
{
  ## ccg-res ##
# # somsc to Mg CO2-eq ha-1
# ccg_res_somsc[, s_SOC   := delta_soc_co2_eq(s_somsc)]
# ccg_res_somsc[, s_somsc := NULL]
# ccg_res_somsc[, rep     := gsub("somsc_", "", rep)]
# gc()
# # dN2O
# ccg_res_dN2O[, s_dN2O    := direct_N2O_co2_eq(s_N2Oflux)]
# ccg_res_dN2O[, s_N2Oflux := NULL]
# ccg_res_dN2O[, rep       := gsub("dN2O_", "", rep)]
# gc()
# # iN2O
# ccg_res_iN2O[, s_iN2O     := as.numeric(s_iN2Oflux)*gwpN2O]
# ccg_res_iN2O[, s_iN2Oflux := NULL]
# ccg_res_iN2O[, rep        := gsub("iN2O_", "", rep)]
# gc()
}
{
  ## ccl-res ##
# # somsc to Mg CO2-eq ha-1
# ccl_res_somsc[, s_SOC   := delta_soc_co2_eq(s_somsc)]
# ccl_res_somsc[, s_somsc := NULL]
# ccl_res_somsc[, rep     := gsub("somsc_", "", rep)]
# gc()
# # dN2O
# ccl_res_dN2O[, s_dN2O    := direct_N2O_co2_eq(s_N2Oflux)]
# ccl_res_dN2O[, s_N2Oflux := NULL]
# ccl_res_dN2O[, rep       := gsub("dN2O_", "", rep)]
# gc()
# # iN2O
# ccl_res_iN2O[, s_iN2O     := as.numeric(s_iN2Oflux)*gwpN2O]
# ccl_res_iN2O[, s_iN2Oflux := NULL]
# ccl_res_iN2O[, rep        := gsub("iN2O_", "", rep)]
# gc()
}
{
  ## ccg-ntill
# # somsc to Mg CO2-eq ha-1
# ccg_ntill_somsc[, s_SOC   := delta_soc_co2_eq(s_somsc)]
# ccg_ntill_somsc[, s_somsc := NULL]
# ccg_ntill_somsc[, rep     := gsub("somsc_", "", rep)]
# gc()
# # dN2O
# ccg_ntill_dN2O[, s_dN2O    := direct_N2O_co2_eq(s_N2Oflux)]
# ccg_ntill_dN2O[, s_N2Oflux := NULL]
# ccg_ntill_dN2O[, rep       := gsub("dN2O_", "", rep)]
# gc()
# # iN2O
# ccg_ntill_iN2O[, s_iN2O     := as.numeric(s_iN2Oflux)*gwpN2O]
# ccg_ntill_iN2O[, s_iN2Oflux := NULL]
# ccg_ntill_iN2O[, rep        := gsub("iN2O_", "", rep)]
# gc()
}
{
  ## ccl-ntill
# somsc to Mg CO2-eq ha-1
ccl_ntill_somsc[, s_SOC   := delta_soc_co2_eq(s_somsc)]
ccl_ntill_somsc[, s_somsc := NULL]
ccl_ntill_somsc[, rep     := gsub("somsc_", "", rep)]
gc()
# dN2O
ccl_ntill_dN2O[, s_dN2O    := direct_N2O_co2_eq(s_N2Oflux)]
ccl_ntill_dN2O[, s_N2Oflux := NULL]
ccl_ntill_dN2O[, rep       := gsub("dN2O_", "", rep)]
gc()
# iN2O
ccl_ntill_iN2O[, s_iN2O     := as.numeric(s_iN2Oflux)*gwpN2O]
ccl_ntill_iN2O[, s_iN2Oflux := NULL]
ccl_ntill_iN2O[, rep        := gsub("iN2O_", "", rep)]
gc()
}
#-----------------------------------------------------------------------------------------
# JOIN DT BY SCENARIO & ESTIMATE GHG
#-----------------------------------------------------------------------------------------
### ALWAYS USE ###
  ## conv ##
conv_dt = conv_somsc[conv_dN2O, on = .(gridid   = gridid,
                                       crop     = crop,
                                       irr      = irr,
                                       scenario = scenario,
                                       ssp      = ssp,
                                       gcm      = gcm,
                                       y_block  = y_block,
                                       rep      = rep)]
rm(conv_somsc, conv_dN2O)
gc()
conv_dt = conv_dt[conv_iN2O, on = .(gridid   = gridid,
                                       crop     = crop,
                                       irr      = irr,
                                       scenario = scenario,
                                       ssp      = ssp,
                                       gcm      = gcm,
                                       y_block  = y_block,
                                       rep      = rep)]
rm(conv_iN2O)
gc()
conv_dt[, s_N2O := s_dN2O + s_iN2O]
conv_dt[, s_GHG := s_SOC + s_dN2O + s_iN2O]

### These should be uncommented depending upon scenario ###  
{
  ## ccg-res ##
# ccg_res_dt = ccg_res_somsc[ccg_res_dN2O, on = .(gridid   = gridid,
#                                                 crop     = crop,
#                                                 irr      = irr,
#                                                 scenario = scenario,
#                                                 ssp      = ssp,
#                                                 gcm      = gcm,
#                                                 y_block  = y_block,
#                                                 rep      = rep)]
# rm(ccg_res_somsc, ccg_res_dN2O)
# gc()
# ccg_res_dt = ccg_res_dt[ccg_res_iN2O, on = .(gridid   = gridid,
#                                              crop     = crop,
#                                              irr      = irr,
#                                              scenario = scenario,
#                                              ssp      = ssp,
#                                              gcm      = gcm,
#                                              y_block  = y_block,
#                                              rep      = rep)]
# rm(ccg_res_iN2O)
# gc()
# ccg_res_dt[, s_N2O := s_dN2O + s_iN2O]
# ccg_res_dt[, s_GHG := s_SOC + s_dN2O + s_iN2O]
}
{
   ## ccl-res ##
# ccl_res_dt = ccl_res_somsc[ccl_res_dN2O, on = .(gridid   = gridid,
#                                        crop     = crop,
#                                        irr      = irr,
#                                        scenario = scenario,
#                                        ssp      = ssp,
#                                        gcm      = gcm,
#                                        y_block  = y_block,
#                                        rep      = rep)]
# rm(ccl_res_somsc, ccl_res_dN2O)
# gc()
# ccl_res_dt = ccl_res_dt[ccl_res_iN2O, on = .(gridid   = gridid,
#                                     crop     = crop,
#                                     irr      = irr,
#                                     scenario = scenario,
#                                     ssp      = ssp,
#                                     gcm      = gcm,
#                                     y_block  = y_block,
#                                     rep      = rep)]
# rm(ccl_res_iN2O)
# gc()
# ccl_res_dt[, s_N2O := s_dN2O + s_iN2O]
# ccl_res_dt[, s_GHG := s_SOC + s_dN2O + s_iN2O]
}
{
  ## ccg-ntill ##
# ccg_ntill_dt = ccg_ntill_somsc[ccg_ntill_dN2O, on = .(gridid   = gridid,
#                                                 crop     = crop,
#                                                 irr      = irr,
#                                                 scenario = scenario,
#                                                 ssp      = ssp,
#                                                 gcm      = gcm,
#                                                 y_block  = y_block,
#                                                 rep      = rep)]
# rm(ccg_ntill_somsc, ccg_ntill_dN2O)
# gc()
# ccg_ntill_dt = ccg_ntill_dt[ccg_ntill_iN2O, on = .(gridid   = gridid,
#                                              crop     = crop,
#                                              irr      = irr,
#                                              scenario = scenario,
#                                              ssp      = ssp,
#                                              gcm      = gcm,
#                                              y_block  = y_block,
#                                              rep      = rep)]
# rm(ccg_ntill_iN2O)
# gc()
# ccg_ntill_dt[, s_N2O := s_dN2O + s_iN2O]
# ccg_ntill_dt[, s_GHG := s_SOC + s_dN2O + s_iN2O]
}
{
  ## ccl-ntill ##
ccl_ntill_dt = ccl_ntill_somsc[ccl_ntill_dN2O, on = .(gridid   = gridid,
                                                      crop     = crop,
                                                      irr      = irr,
                                                      scenario = scenario,
                                                      ssp      = ssp,
                                                      gcm      = gcm,
                                                      y_block  = y_block,
                                                      rep      = rep)]
rm(ccl_ntill_somsc, ccl_ntill_dN2O)
gc()
ccl_ntill_dt = ccl_ntill_dt[ccl_ntill_iN2O, on = .(gridid   = gridid,
                                                   crop     = crop,
                                                   irr      = irr,
                                                   scenario = scenario,
                                                   ssp      = ssp,
                                                   gcm      = gcm,
                                                   y_block  = y_block,
                                                   rep      = rep)]
rm(ccl_ntill_iN2O)
gc()
ccl_ntill_dt[, s_N2O := s_dN2O + s_iN2O]
ccl_ntill_dt[, s_GHG := s_SOC + s_dN2O + s_iN2O]
}
#-----------------------------------------------------------------------------------------
# ESTIMATE DIFFERENCES
#-----------------------------------------------------------------------------------------
### These should be uncommented depending upon scenario ###  
{
  ## ccg-res ##
# d_ccg_res_dt = ccg_res_dt[conv_dt[, -c('scenario')], on = .(gridid   = gridid,
#                                                             crop     = crop,
#                                                             irr      = irr,
#                                                             ssp      = ssp,
#                                                             gcm      = gcm,
#                                                             y_block  = y_block,
#                                                             rep      = rep)]
# rm(ccg_res_dt)
# gc()
# # d_s_SOC
# d_ccg_res_dt[, d_s_SOC := s_SOC - i.s_SOC]
# d_ccg_res_dt[, i.s_SOC := NULL]
# # d_s_dN2O
# d_ccg_res_dt[, d_s_dN2O := s_dN2O - i.s_dN2O]
# d_ccg_res_dt[, i.s_dN2O := NULL]
# # d_s_iN2O
# d_ccg_res_dt[, d_s_iN2O := s_iN2O - i.s_iN2O]
# d_ccg_res_dt[, i.s_iN2O := NULL]
# # d_s_N2O
# d_ccg_res_dt[, d_s_N2O := s_N2O - i.s_N2O]
# d_ccg_res_dt[, i.s_N2O := NULL]
# # d_s_GHG
# d_ccg_res_dt[, d_s_GHG := s_GHG - i.s_GHG]
# d_ccg_res_dt[, i.s_GHG := NULL]
# # remove incomplete cases
# d_ccg_res_dt = d_ccg_res_dt[!is.na(scenario),]
}
{
  ## ccl-res ##
# d_ccl_res_dt = ccl_res_dt[conv_dt[, -c('scenario')], on = .(gridid   = gridid,
#                                           crop     = crop,
#                                           irr      = irr,
#                                           ssp      = ssp,
#                                           gcm      = gcm,
#                                           y_block  = y_block,
#                                           rep      = rep)]
# rm(ccl_res_dt)
# gc()
# # d_s_SOC
# d_ccl_res_dt[, d_s_SOC := s_SOC - i.s_SOC]
# d_ccl_res_dt[, i.s_SOC := NULL]
# # d_s_dN2O
# d_ccl_res_dt[, d_s_dN2O := s_dN2O - i.s_dN2O]
# d_ccl_res_dt[, i.s_dN2O := NULL]
# # d_s_iN2O
# d_ccl_res_dt[, d_s_iN2O := s_iN2O - i.s_iN2O]
# d_ccl_res_dt[, i.s_iN2O := NULL]
# # d_s_N2O
# d_ccl_res_dt[, d_s_N2O := s_N2O - i.s_N2O]
# d_ccl_res_dt[, i.s_N2O := NULL]
# # d_s_GHG
# d_ccl_res_dt[, d_s_GHG := s_GHG - i.s_GHG]
# d_ccl_res_dt[, i.s_GHG := NULL]
# # remove incomplete cases
# d_ccl_res_dt = d_ccl_res_dt[!is.na(scenario),]
}
{
  ## ccg-ntill ## 
# d_ccg_ntill_dt = ccg_ntill_dt[conv_dt[, -c('scenario')], on = .(gridid   = gridid,
#                                                             crop     = crop,
#                                                             irr      = irr,
#                                                             ssp      = ssp,
#                                                             gcm      = gcm,
#                                                             y_block  = y_block,
#                                                             rep      = rep)]
# rm(ccg_ntill_dt)
# gc()
# # d_s_SOC
# d_ccg_ntill_dt[, d_s_SOC := s_SOC - i.s_SOC]
# d_ccg_ntill_dt[, i.s_SOC := NULL]
# # d_s_dN2O
# d_ccg_ntill_dt[, d_s_dN2O := s_dN2O - i.s_dN2O]
# d_ccg_ntill_dt[, i.s_dN2O := NULL]
# # d_s_iN2O
# d_ccg_ntill_dt[, d_s_iN2O := s_iN2O - i.s_iN2O]
# d_ccg_ntill_dt[, i.s_iN2O := NULL]
# # d_s_N2O
# d_ccg_ntill_dt[, d_s_N2O := s_N2O - i.s_N2O]
# d_ccg_ntill_dt[, i.s_N2O := NULL]
# # d_s_GHG
# d_ccg_ntill_dt[, d_s_GHG := s_GHG - i.s_GHG]
# d_ccg_ntill_dt[, i.s_GHG := NULL]
# # remove incomplete cases
# d_ccg_ntill_dt = d_ccg_ntill_dt[!is.na(scenario),]
}
{
  ## ccl-ntill ## 
d_ccl_ntill_dt = ccl_ntill_dt[conv_dt[, -c('scenario')], on = .(gridid   = gridid,
                                                                crop     = crop,
                                                                irr      = irr,
                                                                ssp      = ssp,
                                                                gcm      = gcm,
                                                                y_block  = y_block,
                                                                rep      = rep)]
rm(ccl_ntill_dt)
gc()
# d_s_SOC
d_ccl_ntill_dt[, d_s_SOC := s_SOC - i.s_SOC]
d_ccl_ntill_dt[, i.s_SOC := NULL]
# d_s_dN2O
d_ccl_ntill_dt[, d_s_dN2O := s_dN2O - i.s_dN2O]
d_ccl_ntill_dt[, i.s_dN2O := NULL]
# d_s_iN2O
d_ccl_ntill_dt[, d_s_iN2O := s_iN2O - i.s_iN2O]
d_ccl_ntill_dt[, i.s_iN2O := NULL]
# d_s_N2O
d_ccl_ntill_dt[, d_s_N2O := s_N2O - i.s_N2O]
d_ccl_ntill_dt[, i.s_N2O := NULL]
# d_s_GHG
d_ccl_ntill_dt[, d_s_GHG := s_GHG - i.s_GHG]
d_ccl_ntill_dt[, i.s_GHG := NULL]
# remove incomplete cases
d_ccl_ntill_dt = d_ccl_ntill_dt[!is.na(scenario),]
}
#-----------------------------------------------------------------------------------------
# SAVE OUTPUT, actual emissions & differences
#-----------------------------------------------------------------------------------------
### These should be uncommented depending upon scenario ###  
# NOTE: outliers at gridid 103186,164414
#   ## conv ##
# conv_dt = conv_dt[!gridid %in% c(103186,164414)]
# gc()
# save(conv_dt, file = paste(u_data, 'conv-absolute-ghg-flux-uncertainty.RData', sep = '/'))
#   ## ccg-res ##
# d_ccg_res_dt = d_ccg_res_dt[!gridid %in% c(103186,164414)]
# gc()
# save(d_ccg_res_dt, file = paste(u_data, 'ccg-res-ghg-flux-uncertainty.RData', sep = '/'))
#   ## ccl-res ##
# d_ccl_res_dt = d_ccl_res_dt[!gridid %in% c(103186,164414)]
# gc()
# save(d_ccl_res_dt, file = paste(u_data, 'ccl-res-ghg-flux-uncertainty.RData', sep = '/'))
#   ## ccg-ntill
# d_ccg_ntill_dt = d_ccg_ntill_dt[!gridid %in% c(103186,164414)]
# gc()
# save(d_ccg_ntill_dt, file = paste(u_data, 'ccg-ntill-ghg-flux-uncertainty.RData', sep = '/'))
  ## ccl-ntill
d_ccl_ntill_dt = d_ccl_ntill_dt[!gridid %in% c(103186,164414)]
gc()
save(d_ccl_ntill_dt, file = paste(u_data, 'ccl-ntill-ghg-flux-uncertainty.RData', sep = '/'))
