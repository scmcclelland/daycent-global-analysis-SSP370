# filename:    analysis-recommended-practices-2050.R
# created:     06 March 2023
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file estimates cumulative mean and standard error GHG mitigation 
#              and crop yield potential from DayCent uncertainty and climate variance output,
#              respectively. These data included imputed estimates for arriving at total potentials
#              over all cropland area. Analysis from 2016-2050.
#              Estimates are made based on practices that align with desired outcomes:
#              (1) GHG mitigation (no yield consideration)
#              (2) Yield (no GHG consideration)
#              (3) GHG mitigation (no yield loss)
#              (4) Yield (no increased GHG)
# note:        Run each goal separately (large amount of data)
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
  
  # 2050
  d_ccg_res_dt = d_ccg_res_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccl_res_dt = d_ccl_res_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccg_ntill_dt = d_ccg_ntill_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccl_ntill_dt = d_ccl_ntill_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccg_res_y_dt = d_ccg_res_y_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccl_res_y_dt = d_ccl_res_y_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccg_ntill_y_dt = d_ccg_ntill_y_dt[y_block %in% c(2050),]
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
  
  # 2050
  d_ccl_ntill_y_dt = d_ccl_ntill_y_dt[y_block %in% c(2050),]
  gc()
}
{ ## join ghg
  d_ghg_dt = rbind(d_ccg_res_dt, d_ccl_res_dt, d_ccg_ntill_dt, d_ccl_ntill_dt)
  rm(d_ccg_res_dt, d_ccl_res_dt, d_ccg_ntill_dt, d_ccl_ntill_dt)
  gc()
}
{ ## join yield
  d_yield_dt = rbind(d_ccg_res_y_dt, d_ccl_res_y_dt, d_ccg_ntill_y_dt, d_ccl_ntill_y_dt)
  rm(d_ccg_res_y_dt, d_ccl_res_y_dt, d_ccg_ntill_y_dt, d_ccl_ntill_y_dt)
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
{ 
  d_ghg_dt   = d_ghg_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                 crop   = crop,
                                                                                 irr    = irr)]
  d_ghg_dt   = d_ghg_dt[!is.na(scenario),]
  gc()
  
  d_yield_dt = d_yield_dt[crop_area_dt[, -c('WB_NAME', 'WB_REGION')], on = .(gridid = gridid,
                                                                                    crop   = crop,
                                                                                    irr    = irr)]
  d_yield_dt = d_yield_dt[!is.na(scenario)]
  gc()
}
#-----------------------------------------------------------------------------------------
# IDENTIFY RECOMMENDED PRACTICE
#-----------------------------------------------------------------------------------------
### Data must be reloaded and the following executed one at a time because of file size ###  
### Uncomment as required to run ###

  ### Here and below, 1. max GHG mitigation (ignoring crop yield) is not commented ###
{ ## 1. max GHG mitigation (ignoring crop yield)
  # d_ghg gridid (no-impute)   = 28389
  # d_yield gridid (no-impute) = 28395

  d_ghg_m1_dt = d_ghg_dt[, max_scenario := ifelse(d_s_GHG == min(d_s_GHG), scenario, NA),
           by = .(gridid, crop, irr, rep)] # min because GHG mitigation expressed as negative
  # check min is not positive, if positive then update max scenario to cp
  d_ghg_m1_dt = d_ghg_m1_dt[!is.na(max_scenario) & d_s_GHG > 0, max_scenario := 'cp']
  # CHECK gridid (not imputed) = 28389
  
    # EXTRACT FOR PLOTTING
  practice_1 = copy(d_ghg_m1_dt)
  practice_1 = practice_1[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
                          by = .(gridid)]
  practice_1 = practice_1[freq_scenario == 'cp', scenario := 'cp']
  practice_1 = practice_1[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  practice_1 = practice_1[scen_match == TRUE] # CHECK gridid (not imputed) = 28380
  # drop unnecessary columns
  practice_1 = practice_1[, -c('max_scenario', 'freq_scenario', 'scen_match',
                               'rep', 'd_s_GHG', 'hectares', 'crop', 'irr')]
  # make unique
  practice_1 = unique(practice_1)
  # remove imputed, gcm column
  practice_1 = practice_1[!gcm == 'imputed']
  practice_1 = practice_1[, gcm := NULL]
  practice_1 = unique(practice_1) # CHECK gridid (not imputed) = 28380

    # CONTINUE SCENARIO ID
  # return scenario with highest frequency by gridid, crop, irr
  d_ghg_m1_dt = d_ghg_m1_dt[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
                            by = .(gridid, crop, irr)]
  # for cp, update scenario to cp and change d_s_GHG to 0 (to exclude from count)
  d_ghg_m1_dt = d_ghg_m1_dt[freq_scenario == 'cp', scenario := 'cp']
  d_ghg_m1_dt = d_ghg_m1_dt[scenario == 'cp', d_s_GHG := 0]
  # filter dt to most frequent scenario
  d_ghg_m1_dt = d_ghg_m1_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  d_ghg_m1_dt = d_ghg_m1_dt[scen_match == TRUE] # CHECK gridid (not imputed) = 28381
  # drop unnecessary columns
  d_ghg_m1_dt = d_ghg_m1_dt[, -c('max_scenario','scen_match', 'gcm')]
  # catch cp repeated scenarios
  d_ghg_m1_dt = unique(d_ghg_m1_dt)
  gc()
  # select yield based on scenarios for gridid, crop, irr above
  ghg_m1_id = copy(d_ghg_m1_dt)
  # join to filter
  d_y_ghg_m1_dt = d_yield_dt[unique(ghg_m1_id[, .(gridid, crop, irr, freq_scenario)]), 
                             on = .(gridid = gridid,
                                    crop      = crop,
                                    irr       = irr)] # CHECK gridid (not imputed) = 28,391
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[freq_scenario == 'cp', scenario := 'cp']
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[scen_match == TRUE]
  # update cp response to 0
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[scenario == 'cp', d_s_cgrain := 0]
  # drop unnecessary columns
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[, -c('freq_scenario', 'scen_match', 'gcm')]
  # remove extra cp (make dt unique)
  d_y_ghg_m1_dt = unique(d_y_ghg_m1_dt) # CHECK gridid (not imputed) = 28,374
  d_y_ghg_m1_dt = d_y_ghg_m1_dt[!is.na(ssp)]
  rm(ghg_m1_id)
  gc()
}
{ ## 2. max crop yield difference (ignoring GHG)
  # # d_ghg gridid (no-impute)   = 28389
  # # d_yield gridid (no-impute) = 28395
  # d_yield_m1_dt = d_yield_dt[, max_scenario := ifelse(d_s_cgrain == max(d_s_cgrain, na.rm = TRUE), scenario, NA),
  #                        by = .(gridid, crop, irr, rep)]
  # # check max is not negative, if negative then update max_scenario to cp
  # d_yield_m1_dt = d_yield_m1_dt[!is.na(max_scenario) & d_s_cgrain < 0, max_scenario := 'cp']
  # # CHECK gridid (not imputed) = 28395
  # 
  #    # EXTRACT FOR PLOTTING
  # practice_2 = copy(d_yield_m1_dt)
  # practice_2 = practice_2[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                         by = .(gridid)]
  # practice_2 = practice_2[freq_scenario == 'cp', scenario := 'cp']
  # practice_2 = practice_2[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # practice_2 = practice_2[scen_match == TRUE] # CHECK gridid (not imputed) = 28395
  # # drop unnecessary columns
  # practice_2 = practice_2[, -c('max_scenario', 'freq_scenario', 'scen_match',
  #                              'rep', 'd_s_cgrain', 'hectares', 'crop', 'irr')]
  # # make unique
  # practice_2 = unique(practice_2)
  # # remove imputed, gcm column
  # practice_2 = practice_2[!gcm == 'imputed']
  # practice_2 = practice_2[, gcm := NULL]
  # practice_2 = unique(practice_2) # CHECK gridid (not imputed) = 28395
  # 
  #   # CONTINUE SCENARIO ID
  # # return scenario with highest frequency by gridid, crop, irr
  # d_yield_m1_dt = d_yield_m1_dt[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                           by = .(gridid, crop, irr)]
  # # for cp, update scenario to cp and change d_s_cgrain to 0 (to exclude from count)
  # d_yield_m1_dt = d_yield_m1_dt[freq_scenario == 'cp', scenario := 'cp']
  # d_yield_m1_dt = d_yield_m1_dt[scenario == 'cp', d_s_cgrain := 0]
  # # filter dt to most frequent scenario
  # d_yield_m1_dt = d_yield_m1_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # d_yield_m1_dt = d_yield_m1_dt[scen_match == TRUE] # CHECK gridid (not imputed) = 28395
  # # drop unnecessary columns
  # d_yield_m1_dt = d_yield_m1_dt[, -c('max_scenario', 'scen_match', 'gcm')]
  # # catch cp repeated scenarios
  # d_yield_m1_dt = unique(d_yield_m1_dt) 
  # gc()
  # 
  # # select yield based on scenarios for gridid, crop, irr above
  # yield_m1_id = copy(d_yield_m1_dt)
  # # join to filter
  # d_g_yield_m1_dt = d_ghg_dt[unique(yield_m1_id[, .(gridid, crop, irr, freq_scenario)]), 
  #                            on = .(gridid = gridid,
  #                                   crop      = crop,
  #                                   irr       = irr)] # CHECK gridid (not imputed) = 
  # d_g_yield_m1_dt = d_g_yield_m1_dt[freq_scenario == 'cp', scenario := 'cp']
  # d_g_yield_m1_dt = d_g_yield_m1_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # d_g_yield_m1_dt = d_g_yield_m1_dt[scen_match == TRUE]
  # # update cp response to 0
  # d_g_yield_m1_dt = d_g_yield_m1_dt[scenario == 'cp', d_s_GHG := 0]
  # # drop unnecessary columns
  # d_g_yield_m1_dt = d_g_yield_m1_dt[, -c('freq_scenario', 'scen_match', 'gcm')]
  # # remove extra cp (make dt unique)
  # d_g_yield_m1_dt = unique(d_g_yield_m1_dt) # CHECK gridid (not imputed) = 28360
  # d_g_yield_m1_dt = d_g_yield_m1_dt[!is.na(ssp)]
  # rm(yield_m1_id)
  # gc()
}
{ ## 3. max GHG mitigation (no crop yield loss)
  # d_ghg gridid (no-impute)   = 28389
  # d_yield gridid (no-impute) = 28395
  
  # # CHECK GRIDID AT EACH STEP
  # d_y_ghg_m2_dt = copy(d_yield_dt)
  # # find scenarios where yield >= 0
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[, pos_scenario := ifelse(d_s_cgrain >= 0, scenario, 'cp'),
  #                            by = .(gridid, crop, irr, rep)] 
  # # get freq, if freq is cp then updated scenario
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[, freq_scenario := names(which.max(table(pos_scenario, useNA = "no"))),
  #                               by = .(gridid, crop, irr)]
  # # merge this larger table with multiple scenario options to ghg table
  # # keep freq scenario
  # d_ghg_m2_dt = copy(d_ghg_dt)
  # d_ghg_m2_dt = d_ghg_m2_dt[unique(d_y_ghg_m2_dt[, .(gridid, crop, irr, freq_scenario)]), 
  #                        on = .(gridid   = gridid,
  #                               crop     = crop, 
  #                               irr      = irr)]
  # # if freq_scenario is cp, then 0 out GHG and update scenario
  # d_ghg_m2_dt = d_ghg_m2_dt[freq_scenario == 'cp', scenario := 'cp']
  # d_ghg_m2_dt = d_ghg_m2_dt[scenario == 'cp', d_s_GHG := 0] # indicating no practice change
  # d_ghg_m2_dt = unique(d_ghg_m2_dt) # CHECK gridid (not imputed) = 28365
  # # find max scenario
  # d_ghg_m2_dt = d_ghg_m2_dt[!scenario == 'cp', max_scenario := ifelse(d_s_GHG == min(d_s_GHG), scenario, NA),
  #                        by = .(gridid, crop, irr, rep)] # min because GHG mitigation expressed as negative
  # # check min is not positive, if positive then update max scenario to cp
  # d_ghg_m2_dt = d_ghg_m2_dt[!is.na(max_scenario) & d_s_GHG > 0, max_scenario := 'cp']
  # # update max_scenario for cp only
  # d_ghg_m2_dt = d_ghg_m2_dt[scenario == 'cp', max_scenario := 'cp']
  # 
  #   # EXTRACT FOR PLOTTING
  # practice_3 = copy(d_ghg_m2_dt)
  # # estimate most frequent scenario at gridid level
  # practice_3 = practice_3[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                         by = .(gridid)]
  # practice_3 = practice_3[freq_scenario == 'cp', scenario := 'cp']
  # # keep only identified scenario
  # practice_3 = practice_3[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # practice_3 = practice_3[scen_match == TRUE] # CHECK gridid (not imputed) = 28364
  # # drop unnecessary columns
  # practice_3 = practice_3[, -c('max_scenario', 'freq_scenario', 'scen_match',
  #                              'rep', 'd_s_GHG', 'hectares', 'crop', 'irr')]
  # # remove imputed, gcm column
  # practice_3 = practice_3[!gcm %in% 'imputed']
  # practice_3 = practice_3[, gcm := NULL]
  # practice_3 = unique(practice_3) # NROW 28389 
  # 
  #   # CONTINUE SCENARIO ID
  # # return scenario with highest frequency by gridid, crop, irr
  # d_ghg_m2_dt = d_ghg_m2_dt[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                           by = .(gridid, crop, irr)]
  # d_ghg_m2_dt = d_ghg_m2_dt[freq_scenario == 'cp', scenario := 'cp']
  # # filter dt to most frequent scenario
  # d_ghg_m2_dt = d_ghg_m2_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # d_ghg_m2_dt = d_ghg_m2_dt[scen_match == TRUE]
  # # update cp response to 0
  # d_ghg_m2_dt = d_ghg_m2_dt[scenario == 'cp', d_s_GHG := 0]
  # # drop unnecessary columns
  # d_ghg_m2_dt = d_ghg_m2_dt[, -c('max_scenario', 'scen_match', 'gcm')]
  # # CHECK gridid (not imputed) = 28360
  # d_ghg_m2_dt = d_ghg_m2_dt[!is.na(ssp)]
  # d_ghg_m2_dt = unique(d_ghg_m2_dt)
  # 
  # # filter yield dt again to align with ghg dt
  # # select yield based on scenarios for gridid, crop, irr above
  # ghg_m2_id = copy(d_ghg_m2_dt)
  # # join to filter
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[unique(ghg_m2_id[, .(gridid, crop, irr,freq_scenario)]), 
  #                        on = .(gridid   = gridid,
  #                               crop     = crop, 
  #                               irr      = irr)] # CHECK gridid (not imputed) = 28392
  # # update i.freq_scenario and scenario here
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[i.freq_scenario == 'cp', scenario := 'cp']
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[, scen_match := ifelse(scenario == i.freq_scenario, TRUE, FALSE)]
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[scen_match == TRUE]
  # # update cp response to 0
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[scenario == 'cp', d_s_cgrain := 0]
  # # drop unnecessary columns
  # d_y_ghg_m2_dt = d_y_ghg_m2_dt[, -c('pos_scenario', 'freq_scenario', 'i.freq_scenario', 'scen_match', 'gcm')]
  # # CHECK gridid (not imputed) = 28381
  # # remove extra cp (make dt unique)
  # d_y_ghg_m2_dt = unique(d_y_ghg_m2_dt)
  # rm(ghg_m2_id)
}
{ ## 4. max crop yield difference (no GHG emissions)
  # d_ghg gridid (no-impute)   = 28389
  # d_yield gridid (no-impute) = 28395
  
  # # CHECK GRIDID AT EACH STEP
  # d_g_yield_m2_dt = copy(d_ghg_dt)
  # # find scenarios where ghg <= 0
  # d_g_yield_m2_dt = d_g_yield_m2_dt[, pos_scenario := ifelse(d_s_GHG <= 0, scenario, 'cp'),
  #                               by = .(gridid, crop, irr, rep)] 
  # # get freq, if freq is cp then updated scenario
  # d_g_yield_m2_dt = d_g_yield_m2_dt[, freq_scenario := names(which.max(table(pos_scenario, useNA = "no"))),
  #                               by = .(gridid, crop, irr)]
  # # merge this larger table with multiple scenario options to yield table
  # # keep freq scenario
  # d_yield_m2_dt = copy(d_yield_dt)
  # d_yield_m2_dt = d_yield_m2_dt[unique(d_g_yield_m2_dt[, .(gridid, crop, irr, freq_scenario)]), 
  #                        on = .(gridid   = gridid,
  #                               crop     = crop, 
  #                               irr      = irr)]
  # # if freq_scenario is cp, then 0 out yield and update scenario
  # d_yield_m2_dt = d_yield_m2_dt[freq_scenario == 'cp', scenario := 'cp']
  # d_yield_m2_dt = d_yield_m2_dt[scenario == 'cp', d_s_cgrain := 0] # indicating no practice change
  # d_yield_m2_dt = unique(d_yield_m2_dt) # CHECK gridid (not imputed) = 28391
  # # find max scenario
  # d_yield_m2_dt = d_yield_m2_dt[!scenario == 'cp', max_scenario := ifelse(d_s_cgrain == max(d_s_cgrain), scenario, NA),
  #                           by = .(gridid, crop, irr, rep)] 
  # # check max is not negative, if negative then update max scenario to cp
  # d_yield_m2_dt = d_yield_m2_dt[!is.na(max_scenario) & d_s_cgrain < 0, max_scenario := 'cp']
  # # update max_scenario for cp only
  # d_yield_m2_dt = d_yield_m2_dt[scenario == 'cp', max_scenario := 'cp']
  # 
  # # EXTRACT FOR PLOTTING
  # practice_4 = copy(d_yield_m2_dt)
  # practice_4[, freq_scenario := NULL]
  # # estimate most frequent scenario at gridid level
  # practice_4 = practice_4[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                         by = .(gridid)]
  # practice_4 = practice_4[freq_scenario == 'cp', scenario := 'cp']
  # # keep only identified scenario
  # practice_4 = practice_4[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # practice_4 = practice_4[scen_match == TRUE] # CHECK gridid (not imputed) = 28384
  # # drop unnecessary columns
  # practice_4 = practice_4[, -c('max_scenario', 'freq_scenario', 'scen_match',
  #                              'rep', 'd_s_cgrain', 'hectares', 'crop', 'irr')]
  # # make unique
  # practice_4 = unique(practice_4)
  # # remove imputed, gcm column
  # practice_4 = practice_4[!gcm == 'imputed']
  # practice_4 = practice_4[, gcm := NULL]
  # practice_4 = unique(practice_4) # NROW 28384 
  # 
  # # CONTINUE SCENARIO ID
  # # return scenario with highest frequency by gridid, crop, irr
  # d_yield_m2_dt[, freq_scenario := NULL]
  # d_yield_m2_dt = d_yield_m2_dt[, freq_scenario := names(which.max(table(max_scenario, useNA = "no"))),
  #                           by = .(gridid, crop, irr)]
  # d_yield_m2_dt = d_yield_m2_dt[freq_scenario == 'cp', scenario := 'cp']
  # # filter dt to most frequent scenario
  # d_yield_m2_dt = d_yield_m2_dt[, scen_match := ifelse(scenario == freq_scenario, TRUE, FALSE)]
  # d_yield_m2_dt = d_yield_m2_dt[scen_match == TRUE]
  # # update cp response to 0
  # d_yield_m2_dt = d_yield_m2_dt[scenario == 'cp', d_s_cgrain := 0]
  # # drop unnecessary columns
  # d_yield_m2_dt = d_yield_m2_dt[, -c('max_scenario', 'scen_match', 'gcm')]
  # # CHECK gridid (not imputed) = 28391
  # d_yield_m2_dt = d_yield_m2_dt[!is.na(ssp)]
  # d_yield_m2_dt = unique(d_yield_m2_dt)
  # 
  # # filter ghg dt again to align with yield dt
  # # select ghg based on scenarios for gridid, crop, irr above
  # yield_m2_id = copy(d_yield_m2_dt)
  # # join to filter
  # d_g_yield_m2_dt = d_g_yield_m2_dt[unique(yield_m2_id[, .(gridid, crop, irr,freq_scenario)]), 
  #                               on = .(gridid   = gridid,
  #                                      crop     = crop, 
  #                                      irr      = irr)] # CHECK gridid (not imputed) = 28365
  # # update i.freq_scenario and scenario here
  # d_g_yield_m2_dt = d_g_yield_m2_dt[i.freq_scenario == 'cp', scenario := 'cp']
  # d_g_yield_m2_dt = d_g_yield_m2_dt[, scen_match := ifelse(scenario == i.freq_scenario, TRUE, FALSE)]
  # d_g_yield_m2_dt = d_g_yield_m2_dt[scen_match == TRUE]
  # # update cp response to 0
  # d_g_yield_m2_dt = d_g_yield_m2_dt[scenario == 'cp', d_s_GHG := 0]
  # # drop unnecessary columns
  # d_g_yield_m2_dt = d_g_yield_m2_dt[, -c('pos_scenario', 'freq_scenario', 'i.freq_scenario', 'scen_match', 'gcm')]
  # # CHECK gridid (not imputed) = 28365
  # # remove extra cp (make dt unique)
  # d_g_yield_m2_dt = unique(d_g_yield_m2_dt)
  # rm(yield_m2_id)
}
#-----------------------------------------------------------------------------------------
# MULTIPLY BY CROPLAND AREA, Mg CO2-eq
#-----------------------------------------------------------------------------------------
### Uncomment as required to run ###
  ## 1. max GHG mitigation (ignoring crop yield)
d_ghg_m1_dt[, t_d_s_GHG      := d_s_GHG*hectares]
d_y_ghg_m1_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
# ## 2. max crop yield difference (ignoring GHG)
# d_yield_m1_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
# d_g_yield_m1_dt[, t_d_s_GHG  := d_s_GHG*hectares] 
# ## 3. max GHG mitigation (no crop yield loss)
# d_ghg_m2_dt[, t_d_s_GHG      := d_s_GHG*hectares]
# d_y_ghg_m2_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
# ## 4. max crop yield difference (no GHG emissions)
# d_yield_m2_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
# d_g_yield_m2_dt[, t_d_s_GHG  := d_s_GHG*hectares] 
#-----------------------------------------------------------------------------------------
# ESTIMATE GHG & YIELD POTENTIAL
#-----------------------------------------------------------------------------------------
ss   = 24 # number of climate variants
ci_l = 1.96 # 95% CI level

### Uncomment as required to run ###
### Here and below, 1. max GHG mitigation (ignoring crop yield) is not commented ###

## 1. max GHG mitigation (ignoring crop yield)
## global ##
{ ## 1. max GHG mitigation (ignoring crop yield)
  global_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                     by = .(y_block, rep)]
  global_ghg_m1_m_dt   = global_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(y_block)]
  global_ghg_m1_se_dt  = global_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                          .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(y_block)]
  setnames(global_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(global_ghg_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  global_ghg_m1_dt = global_ghg_m1_m_dt[global_ghg_m1_se_dt, on = .(y_block)]
  global_ghg_m1_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
  # add CI
  global_ghg_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  global_ghg_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # YIELD
  global_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                        by = .(y_block, rep)]
  global_y_ghg_m1_m_dt  = global_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(y_block)]
  global_y_ghg_m1_se_dt = global_y_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                             .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(y_block)]
  setnames(global_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(global_y_ghg_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  global_y_ghg_m1_dt = global_y_ghg_m1_m_dt[global_y_ghg_m1_se_dt, on = .(y_block)]
  global_y_ghg_m1_dt[, IPCC_NAME := 'GLB']
  setcolorder(global_y_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
  # add CI
  global_y_ghg_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  global_y_ghg_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## regional ## 
{ ## 1. max GHG mitigation (ignoring crop yield)
  region_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                     by = .(y_block, rep, IPCC_NAME)]
  region_ghg_m1_m_dt   = region_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(y_block, IPCC_NAME)]
  region_ghg_m1_se_dt  = region_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                          .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(y_block, IPCC_NAME)]
  setnames(region_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_ghg_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  region_ghg_m1_dt = region_ghg_m1_m_dt[region_ghg_m1_se_dt, on = .(y_block, IPCC_NAME)]
  setcolorder(region_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
  # add CI
  region_ghg_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  region_ghg_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # YIELD
  region_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                        by = .(y_block, rep, IPCC_NAME)]
  region_y_ghg_m1_m_dt  = region_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(y_block, IPCC_NAME)]
  region_y_ghg_m1_se_dt = region_y_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                             .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(y_block, IPCC_NAME)]
  setnames(region_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_y_ghg_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  region_y_ghg_m1_dt = region_y_ghg_m1_m_dt[region_y_ghg_m1_se_dt, on = .(y_block, IPCC_NAME)]
  setcolorder(region_y_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
  # add CI
  region_y_ghg_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  region_y_ghg_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
  
  ## by scenario
  region_scen_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
                                          by = .(y_block, scenario, rep, IPCC_NAME)]
  region_scen_ghg_m1_m_dt   = region_scen_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
                                                    by = .(y_block, IPCC_NAME, scenario)]
  region_scen_ghg_m1_se_dt  = region_scen_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                                    .SDcols = c('t_d_s_GHG', 'hectares'),
                                                    by = .(y_block, IPCC_NAME, scenario)]
  setnames(region_scen_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  setnames(region_scen_ghg_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  region_scen_ghg_m1_dt = region_scen_ghg_m1_m_dt[region_scen_ghg_m1_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  setcolorder(region_scen_ghg_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # add CI
  region_scen_ghg_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  region_scen_ghg_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # YIELD
  region_scen_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                             by = .(y_block, scenario,rep, IPCC_NAME)]
  region_scen_y_ghg_m1_m_dt  = region_scen_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                       by = .(y_block, IPCC_NAME, scenario)]
  region_scen_y_ghg_m1_se_dt = region_scen_y_ghg_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
                                                       .SDcols = c('t_d_s_cgrain', 'hectares'),
                                                       by = .(y_block, IPCC_NAME, scenario)]
  setnames(region_scen_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  setnames(region_scen_y_ghg_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  region_scen_y_ghg_m1_dt = region_scen_y_ghg_m1_m_dt[region_scen_y_ghg_m1_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  setcolorder(region_scen_y_ghg_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # add CI
  region_scen_y_ghg_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  region_scen_y_ghg_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## 2. max crop yield difference (ignoring GHG)
## global ##
{ ## 2. max crop yield difference (ignoring GHG)
  # global_g_yield_m1_dt     = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                            by = .(y_block, rep)]
  # global_g_yield_m1_m_dt   = global_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block)]
  # global_g_yield_m1_se_dt  = global_g_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                 .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block)]
  # setnames(global_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(global_g_yield_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # global_g_yield_m1_dt = global_g_yield_m1_m_dt[global_g_yield_m1_se_dt, on = .(y_block)]
  # global_g_yield_m1_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_g_yield_m1_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_g_yield_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # global_g_yield_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # # YIELD
  # global_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep)]
  # global_yield_m1_m_dt  = global_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # global_yield_m1_se_dt = global_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # setnames(global_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(global_yield_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # global_yield_m1_dt = global_yield_m1_m_dt[global_yield_m1_se_dt, on = .(y_block)]
  # global_yield_m1_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_yield_m1_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_yield_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # global_yield_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## regional ## 
{ ## 2. max crop yield difference (ignoring GHG)
  # region_g_yield_m1_dt   = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                          by = .(y_block, rep, IPCC_NAME)]
  # region_g_yield_m1_m_dt = region_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                               by = .(y_block, IPCC_NAME)]
  # region_g_yield_m1_se_dt  = region_g_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                 .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block, IPCC_NAME)]
  # setnames(region_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_g_yield_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_g_yield_m1_dt = region_g_yield_m1_m_dt[region_g_yield_m1_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_g_yield_m1_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_g_yield_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_g_yield_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # region_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep, IPCC_NAME)]
  # region_yield_m1_m_dt  = region_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # region_yield_m1_se_dt = region_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # setnames(region_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_yield_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_yield_m1_dt = region_yield_m1_m_dt[region_yield_m1_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_yield_m1_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_yield_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_yield_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
  # 
  # # by scenario
  # region_scen_g_yield_m1_dt   = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                               by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_g_yield_m1_m_dt = region_scen_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                         by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_g_yield_m1_se_dt  = region_scen_g_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                           .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                           by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_scen_g_yield_m1_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_scen_g_yield_m1_dt = region_scen_g_yield_m1_m_dt[region_scen_g_yield_m1_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_g_yield_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_g_yield_m1_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_scen_g_yield_m1_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # region_scen_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_yield_m1_m_dt  = region_scen_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_yield_m1_se_dt = region_scen_yield_m1_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                      .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_scen_yield_m1_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_scen_yield_m1_dt = region_scen_yield_m1_m_dt[region_scen_yield_m1_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_yield_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_yield_m1_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_scen_yield_m1_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## 3. max GHG mitigation (no crop yield loss)
## global ##
{ ## 3. max GHG mitigation (no crop yield loss)
  # global_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                    by = .(y_block, rep)]
  # global_ghg_m2_m_dt   = global_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                         by = .(y_block)]
  # global_ghg_m2_se_dt  = global_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                         .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                         by = .(y_block)]
  # setnames(global_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(global_ghg_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # global_ghg_m2_dt = global_ghg_m2_m_dt[global_ghg_m2_se_dt, on = .(y_block)]
  # global_ghg_m2_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_ghg_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # global_ghg_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # # YIELD
  # global_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep)]
  # global_y_ghg_m2_m_dt  = global_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # global_y_ghg_m2_se_dt = global_y_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # setnames(global_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(global_y_ghg_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # global_y_ghg_m2_dt = global_y_ghg_m2_m_dt[global_y_ghg_m2_se_dt, on = .(y_block)]
  # global_y_ghg_m2_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_y_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_y_ghg_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # global_y_ghg_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## regional ## 
{ ## 3. max GHG mitigation (no crop yield loss)
  # region_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                    by = .(y_block, rep, IPCC_NAME)]
  # region_ghg_m2_m_dt   = region_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                         by = .(y_block, IPCC_NAME)]
  # region_ghg_m2_se_dt  = region_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                         .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                         by = .(y_block, IPCC_NAME)]
  # setnames(region_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_ghg_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_ghg_m2_dt = region_ghg_m2_m_dt[region_ghg_m2_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_ghg_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_ghg_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # region_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep, IPCC_NAME)]
  # region_y_ghg_m2_m_dt  = region_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # region_y_ghg_m2_se_dt = region_y_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # setnames(region_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_y_ghg_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_y_ghg_m2_dt = region_y_ghg_m2_m_dt[region_y_ghg_m2_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_y_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_y_ghg_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_y_ghg_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
  # 
  # # by scenario
  # region_scen_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                         by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_ghg_m2_m_dt   = region_scen_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                   by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_ghg_m2_se_dt  = region_scen_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                   .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                   by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_scen_ghg_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_scen_ghg_m2_dt = region_scen_ghg_m2_m_dt[region_scen_ghg_m2_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_ghg_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_ghg_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_scen_ghg_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # # YIELD
  # region_scen_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_y_ghg_m2_m_dt  = region_scen_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_y_ghg_m2_se_dt = region_scen_y_ghg_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                      .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_scen_y_ghg_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_scen_y_ghg_m2_dt = region_scen_y_ghg_m2_m_dt[region_scen_y_ghg_m2_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_y_ghg_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_y_ghg_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_scen_y_ghg_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## 4. max crop yield difference (no GHG emissions)
## global ##
{ ## 4. max crop yield difference (no GHG emissions)
  # global_g_yield_m2_dt     = d_g_yield_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                            by = .(y_block, rep)]
  # global_g_yield_m2_m_dt   = global_g_yield_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block)]
  # global_g_yield_m2_se_dt  = global_g_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                 .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block)]
  # setnames(global_g_yield_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(global_g_yield_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # global_g_yield_m2_dt = global_g_yield_m2_m_dt[global_g_yield_m2_se_dt, on = .(y_block)]
  # global_g_yield_m2_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_g_yield_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_g_yield_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # global_g_yield_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # global_yield_m2_dt    = d_yield_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep)]
  # global_yield_m2_m_dt  = global_yield_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # global_yield_m2_se_dt = global_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block)]
  # setnames(global_yield_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(global_yield_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # global_yield_m2_dt = global_yield_m2_m_dt[global_yield_m2_se_dt, on = .(y_block)]
  # global_yield_m2_dt[, IPCC_NAME := 'GLB']
  # setcolorder(global_yield_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # global_yield_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # global_yield_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
## regional ## 
{ ## 4. max crop yield difference (no GHG emissions)
  # region_g_yield_m2_dt   = d_g_yield_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                          by = .(y_block, rep, IPCC_NAME)]
  # region_g_yield_m2_m_dt = region_g_yield_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                               by = .(y_block, IPCC_NAME)]
  # region_g_yield_m2_se_dt  = region_g_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                 .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                 by = .(y_block, IPCC_NAME)]
  # setnames(region_g_yield_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_g_yield_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_g_yield_m2_dt = region_g_yield_m2_m_dt[region_g_yield_m2_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_g_yield_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_g_yield_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_g_yield_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # region_yield_m2_dt    = d_yield_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                       by = .(y_block, rep, IPCC_NAME)]
  # region_yield_m2_m_dt  = region_yield_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # region_yield_m2_se_dt = region_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                            .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, IPCC_NAME)]
  # setnames(region_yield_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_yield_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_yield_m2_dt = region_yield_m2_m_dt[region_yield_m2_se_dt, on = .(y_block, IPCC_NAME)]
  # setcolorder(region_yield_m2_dt, c('y_block', 'IPCC_NAME'))
  # # add CI
  # region_yield_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_yield_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
  # 
  # # by scenario
  # region_scen_g_yield_m2_dt   = d_g_yield_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                               by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_g_yield_m2_m_dt = region_scen_g_yield_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                         by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_g_yield_m2_se_dt  = region_scen_g_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                           .SDcols = c('t_d_s_GHG', 'hectares'),
  #                                                           by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_g_yield_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
  # setnames(region_scen_g_yield_m2_se_dt, c('t_d_s_GHG', 'hectares'), c('se_s_GHG', 'se_hectares'))
  # region_scen_g_yield_m2_dt = region_scen_g_yield_m2_m_dt[region_scen_g_yield_m2_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_g_yield_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_g_yield_m2_dt[, CI_95_lower := s_GHG-(ci_l*se_s_GHG)]
  # region_scen_g_yield_m2_dt[, CI_95_upper := s_GHG+(ci_l*se_s_GHG)]
  # 
  # # YIELD
  # region_scen_yield_m2_dt    = d_yield_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                            by = .(y_block, rep, IPCC_NAME, scenario)]
  # region_scen_yield_m2_m_dt  = region_scen_yield_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # region_scen_yield_m2_se_dt = region_scen_yield_m2_dt[, lapply(.SD, function(x) {(sd(x, na.rm = TRUE)/sqrt(ss))}), 
  #                                                      .SDcols = c('t_d_s_cgrain', 'hectares'),
  #                                                      by = .(y_block, IPCC_NAME, scenario)]
  # setnames(region_scen_yield_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
  # setnames(region_scen_yield_m2_se_dt, c('t_d_s_cgrain', 'hectares'), c('se_s_grain', 'se_hectares'))
  # region_scen_yield_m2_dt = region_scen_yield_m2_m_dt[region_scen_yield_m2_se_dt, on = .(y_block, IPCC_NAME, scenario)]
  # setcolorder(region_scen_yield_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
  # # add CI
  # region_scen_yield_m2_dt[, CI_95_lower := s_grain-(ci_l*se_s_grain)]
  # region_scen_yield_m2_dt[, CI_95_upper := s_grain+(ci_l*se_s_grain)]
}
#-----------------------------------------------------------------------------------------
# SAVE
#-----------------------------------------------------------------------------------------
### Uncomment as required to run ###
## 1. max GHG mitigation (ignoring crop yield)
m1_dt  = rbind(global_ghg_m1_dt, region_ghg_m1_dt)
fwrite(m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential.csv', sep = '/'))
m1_y_dt = rbind(global_y_ghg_m1_dt, region_y_ghg_m1_dt)
fwrite(m1_y_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential.csv', sep = '/'))
fwrite(region_scen_ghg_m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential-by-scenario.csv', sep = '/'))
fwrite(region_scen_y_ghg_m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential-by-scenario.csv', sep = '/'))
# practices
fwrite(practice_1, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-practices.csv', sep = '/'))
# 
# ## 2. max crop yield difference (ignoring GHG)
# yield_m1_dt  = rbind(global_yield_m1_dt, region_yield_m1_dt)
# fwrite(yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential.csv', sep = '/'))
# m1_ghg_dt = rbind(global_g_yield_m1_dt, region_g_yield_m1_dt)
# fwrite(m1_ghg_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential.csv', sep = '/'))
# fwrite(region_scen_g_yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential-by-scenario.csv', sep = '/'))
# fwrite(region_scen_yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential-by-scenario.csv', sep = '/'))
# # practices
# fwrite(practice_2, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-practices.csv', sep = '/'))
# 
# ## 3. max GHG mitigation (no crop yield loss)
# m2_dt  = rbind(global_ghg_m2_dt, region_ghg_m2_dt)
# fwrite(m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential.csv', sep = '/'))
# m2_y_dt = rbind(global_y_ghg_m2_dt, region_y_ghg_m2_dt)
# fwrite(m2_y_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential.csv', sep = '/'))
# fwrite(region_scen_ghg_m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential-by-scenario.csv', sep = '/'))
# fwrite(region_scen_y_ghg_m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential-by-scenario.csv', sep = '/'))
# # practices
# fwrite(practice_3, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-practices.csv', sep = '/'))
# 
# ## 4. max crop yield difference (no GHG emissions)
# yield_m2_dt  = rbind(global_yield_m2_dt, region_yield_m2_dt)
# fwrite(yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential.csv', sep = '/'))
# m2_ghg_dt = rbind(global_g_yield_m2_dt, region_g_yield_m2_dt)
# fwrite(m2_ghg_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential.csv', sep = '/'))
# fwrite(region_scen_g_yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential-by-scenario.csv', sep = '/'))
# fwrite(region_scen_yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential-by-scenario.csv', sep = '/'))
# # practices
# fwrite(practice_4, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-practices.csv', sep = '/'))
