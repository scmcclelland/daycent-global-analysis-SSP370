# filename:    analysis-Pareto-2050.R
# created:     06 March 2023
# updated:     21 January 2025
# author:      S.C. McClelland
# description: This file estimates
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(data.table)
library(parallel)
library(pbmcapply)
library(rPref)
# library(rstudioapi)
library(sf)
library(stringr)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES, FUNCTIONS & FILES
#-----------------------------------------------------------------------------------------
# dir = dirname(getActiveDocumentContext()$path)
dir = getwd()
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
# Pareto Optimal Solution
#-----------------------------------------------------------------------------------------
gc()
# add cp scenario to ghg and yield dt
  ## ghg ##
cp_ghg_dt = unique(d_ghg_dt[, c('gridid', 'crop', 'irr', 'rep')])
cp_ghg_dt[, scenario := 'cp']
setcolorder(cp_ghg_dt, c('gridid', 'crop', 'irr', 'scenario','rep'))
cp_ghg_dt[, d_s_GHG := 0]
# combine
ghg_dt = rbind(d_ghg_dt[, c('gridid', 'crop', 'irr', 'scenario', 'rep', 'd_s_GHG')],
                 cp_ghg_dt)
setorder(ghg_dt, gridid, crop, irr)
gc()
rm(cp_ghg_dt, d_ghg_dt)
gc()
  ## yield ##
cp_yield_dt = unique(d_yield_dt[, c('gridid', 'crop', 'irr', 'rep')])
cp_yield_dt[, scenario := 'cp']
setcolorder(cp_yield_dt, c('gridid', 'crop', 'irr', 'scenario','rep'))
cp_yield_dt[, d_s_cgrain := 0]
# combine
yield_dt = rbind(d_yield_dt[, c('gridid', 'crop', 'irr', 'scenario', 'rep', 'd_s_cgrain')],
                 cp_yield_dt)
setorder(yield_dt, gridid, crop, irr)
gc()
rm(cp_yield_dt, d_yield_dt)

  ## FIND PARETO OPTIMAL ##
print('Running Pareto bootstrap.')
print(Sys.time()) 

set.seed(11162024)
n_boot  = 1000 # Number of bootstrap iterations
n_cores = detectCores() - 1  # Leave one core free for system processes
pref    = high(yield_mean) * low(ghg_mean) # Define Pareto preferences

# Run parallel bootstrap with progress bar
bootstrap_results = pbmclapply(1:n_boot, 
                                compute_pareto,
                                mc.cores = n_cores,
                                mc.style = "txt")  # text-based progress bar

# Combine bootstrap results
all_pareto = rbindlist(bootstrap_results, idcol = "iteration")

# Count the number of bootstrap iterations (total n_boot) for each group
robust_scenarios = all_pareto[, .(
  scenario_count = .N  # Number of times the scenario appears
), by = .(gridid, crop, irr, scenario)]
setorder(robust_scenarios, gridid, crop, irr)

# Add the total bootstrap iterations per group
total_iterations = all_pareto[, .N, by = .(gridid, crop, irr)] 
robust_scenarios = merge(robust_scenarios, total_iterations,
                         by = c("gridid", "crop", "irr"))

# Calculate frequency as the proportion of bootstrap iterations
robust_scenarios[, frequency := scenario_count / N]
robust_scenarios[, freq_scenario := ifelse(frequency == max(frequency), scenario, NA),
                 by = .(gridid, crop, irr)]
# keep only most frequent scenarios
robust_scenarios = robust_scenarios[complete.cases(robust_scenarios)]
setorder(robust_scenarios, gridid, crop, irr)

# output results in very few optimal solutions, most locations are missing

# tbd: what to do when there are multiple Pareto optimal solutions for
#      gridid, crop, irr?

# tbd: for missing gridid, crop, irr assume no clear 'trade-off'
#      meaning outcomes the same or no Pareto front
#      I think this will mean assuming 'cp' for these OR
#      running something similar to old approach to determine if cp or not.
#      If not, then go with whatever scenario balances the outcomes

#-----------------------------------------------------------------------------------------
# MULTIPLY BY CROPLAND AREA, Mg CO2-eq
#-----------------------------------------------------------------------------------------
#   ## 1. max GHG mitigation (ignoring crop yield)
# d_ghg_m1_dt[, t_d_s_GHG      := d_s_GHG*hectares]
# d_y_ghg_m1_dt[, t_d_s_cgrain := d_s_cgrain*hectares]
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
# ## global ##
# { ## 1. max GHG mitigation (ignoring crop yield)
#   global_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                      by = .(y_block, rep)]
#   global_ghg_m1_m_dt   = global_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block)]
#   global_ghg_m1_sd_dt  = global_ghg_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block)]
#   setnames(global_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(global_ghg_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   global_ghg_m1_dt = global_ghg_m1_m_dt[global_ghg_m1_sd_dt, on = .(y_block)]
#   global_ghg_m1_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
#   # YIELD
#   global_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep)]
#   global_y_ghg_m1_m_dt  = global_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   global_y_ghg_m1_sd_dt = global_y_ghg_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   setnames(global_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(global_y_ghg_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   global_y_ghg_m1_dt = global_y_ghg_m1_m_dt[global_y_ghg_m1_sd_dt, on = .(y_block)]
#   global_y_ghg_m1_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_y_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
# }
# ## regional ## 
# { ## 1. max GHG mitigation (ignoring crop yield)
#   region_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                      by = .(y_block, rep, IPCC_NAME)]
#   region_ghg_m1_m_dt   = region_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, IPCC_NAME)]
#   region_ghg_m1_sd_dt  = region_ghg_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, IPCC_NAME)]
#   setnames(region_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_ghg_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_ghg_m1_dt = region_ghg_m1_m_dt[region_ghg_m1_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
#   # YIELD
#   region_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep, IPCC_NAME)]
#   region_y_ghg_m1_m_dt  = region_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   region_y_ghg_m1_sd_dt = region_y_ghg_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   setnames(region_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_y_ghg_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_y_ghg_m1_dt = region_y_ghg_m1_m_dt[region_y_ghg_m1_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_y_ghg_m1_dt, c('y_block', 'IPCC_NAME'))
#   
#   ## by scenario
#   region_scen_ghg_m1_dt     = d_ghg_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, scenario, rep, IPCC_NAME)]
#   region_scen_ghg_m1_m_dt   = region_scen_ghg_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                     by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_ghg_m1_sd_dt  = region_scen_ghg_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                     by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_ghg_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_scen_ghg_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_scen_ghg_m1_dt = region_scen_ghg_m1_m_dt[region_scen_ghg_m1_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_ghg_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
#   # YIELD
#   region_scen_y_ghg_m1_dt    = d_y_ghg_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, scenario,rep, IPCC_NAME)]
#   region_scen_y_ghg_m1_m_dt  = region_scen_y_ghg_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_y_ghg_m1_sd_dt = region_scen_y_ghg_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_y_ghg_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_scen_y_ghg_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_scen_y_ghg_m1_dt = region_scen_y_ghg_m1_m_dt[region_scen_y_ghg_m1_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_y_ghg_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
# }
# ## global ##
# { ## 2. max crop yield difference (ignoring GHG)
#   global_g_yield_m1_dt     = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                              by = .(y_block, rep)]
#   global_g_yield_m1_m_dt   = global_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                   by = .(y_block)]
#   global_g_yield_m1_sd_dt  = global_g_yield_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                   by = .(y_block)]
#   setnames(global_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(global_g_yield_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   global_g_yield_m1_dt = global_g_yield_m1_m_dt[global_g_yield_m1_sd_dt, on = .(y_block)]
#   global_g_yield_m1_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_g_yield_m1_dt, c('y_block', 'IPCC_NAME'))
#   
#   # YIELD
#   global_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep)]
#   global_yield_m1_m_dt  = global_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   global_yield_m1_sd_dt = global_yield_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   setnames(global_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(global_yield_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   global_yield_m1_dt = global_yield_m1_m_dt[global_yield_m1_sd_dt, on = .(y_block)]
#   global_yield_m1_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_yield_m1_dt, c('y_block', 'IPCC_NAME'))
# }
# ## regional ## 
# { ## 2. max crop yield difference (ignoring GHG)
#   region_g_yield_m1_dt   = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                            by = .(y_block, rep, IPCC_NAME)]
#   region_g_yield_m1_m_dt = region_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                 by = .(y_block, IPCC_NAME)]
#   region_g_yield_m1_sd_dt  = region_g_yield_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                   by = .(y_block, IPCC_NAME)]
#   setnames(region_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_g_yield_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_g_yield_m1_dt = region_g_yield_m1_m_dt[region_g_yield_m1_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_g_yield_m1_dt, c('y_block', 'IPCC_NAME'))
#   # YIELD
#   region_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep, IPCC_NAME)]
#   region_yield_m1_m_dt  = region_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   region_yield_m1_sd_dt = region_yield_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   setnames(region_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_yield_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_yield_m1_dt = region_yield_m1_m_dt[region_yield_m1_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_yield_m1_dt, c('y_block', 'IPCC_NAME'))
#   
#   # by scenario
#   region_scen_g_yield_m1_dt   = d_g_yield_m1_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                 by = .(y_block, rep, IPCC_NAME, scenario)]
#   region_scen_g_yield_m1_m_dt = region_scen_g_yield_m1_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                           by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_g_yield_m1_sd_dt  = region_scen_g_yield_m1_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                             by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_g_yield_m1_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_scen_g_yield_m1_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_scen_g_yield_m1_dt = region_scen_g_yield_m1_m_dt[region_scen_g_yield_m1_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_g_yield_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
#   # YIELD
#   region_scen_yield_m1_dt    = d_yield_m1_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, rep, IPCC_NAME, scenario)]
#   region_scen_yield_m1_m_dt  = region_scen_yield_m1_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_yield_m1_sd_dt = region_scen_yield_m1_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_yield_m1_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_scen_yield_m1_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_scen_yield_m1_dt = region_scen_yield_m1_m_dt[region_scen_yield_m1_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_yield_m1_dt, c('y_block', 'IPCC_NAME', 'scenario'))
# }
# ## global ##
# { ## 3. max GHG mitigation (no crop yield loss)
#   global_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                      by = .(y_block, rep)]
#   global_ghg_m2_m_dt   = global_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block)]
#   global_ghg_m2_sd_dt  = global_ghg_m2_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block)]
#   setnames(global_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(global_ghg_m2_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   global_ghg_m2_dt = global_ghg_m2_m_dt[global_ghg_m2_sd_dt, on = .(y_block)]
#   global_ghg_m2_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
#   # YIELD
#   global_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep)]
#   global_y_ghg_m2_m_dt  = global_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   global_y_ghg_m2_sd_dt = global_y_ghg_m2_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block)]
#   setnames(global_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(global_y_ghg_m2_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   global_y_ghg_m2_dt = global_y_ghg_m2_m_dt[global_y_ghg_m2_sd_dt, on = .(y_block)]
#   global_y_ghg_m2_dt[, IPCC_NAME := 'GLB']
#   setcolorder(global_y_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
# }
# ## regional ## 
# { ## 3. max GHG mitigation (no crop yield loss)
#   region_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                      by = .(y_block, rep, IPCC_NAME)]
#   region_ghg_m2_m_dt   = region_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, IPCC_NAME)]
#   region_ghg_m2_sd_dt  = region_ghg_m2_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, IPCC_NAME)]
#   setnames(region_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_ghg_m2_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_ghg_m2_dt = region_ghg_m2_m_dt[region_ghg_m2_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
#   # YIELD
#   region_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                         by = .(y_block, rep, IPCC_NAME)]
#   region_y_ghg_m2_m_dt  = region_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   region_y_ghg_m2_sd_dt = region_y_ghg_m2_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, IPCC_NAME)]
#   setnames(region_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_y_ghg_m2_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_y_ghg_m2_dt = region_y_ghg_m2_m_dt[region_y_ghg_m2_sd_dt, on = .(y_block, IPCC_NAME)]
#   setcolorder(region_y_ghg_m2_dt, c('y_block', 'IPCC_NAME'))
#   
#   # by scenario
#   region_scen_ghg_m2_dt     = d_ghg_m2_dt[, lapply(.SD, sum), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                           by = .(y_block, rep, IPCC_NAME, scenario)]
#   region_scen_ghg_m2_m_dt   = region_scen_ghg_m2_dt[, lapply(.SD, mean), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                     by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_ghg_m2_sd_dt  = region_scen_ghg_m2_dt[, lapply(.SD, sd), .SDcols = c('t_d_s_GHG', 'hectares'),
#                                                     by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_ghg_m2_m_dt,  c('t_d_s_GHG', 'hectares'), c('s_GHG', 'm_hectares'))
#   setnames(region_scen_ghg_m2_sd_dt, c('t_d_s_GHG', 'hectares'), c('sd_s_GHG', 'sd_hectares'))
#   region_scen_ghg_m2_dt = region_scen_ghg_m2_m_dt[region_scen_ghg_m2_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_ghg_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
#   # YIELD
#   region_scen_y_ghg_m2_dt    = d_y_ghg_m2_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                              by = .(y_block, rep, IPCC_NAME, scenario)]
#   region_scen_y_ghg_m2_m_dt  = region_scen_y_ghg_m2_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   region_scen_y_ghg_m2_sd_dt = region_scen_y_ghg_m2_dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c('t_d_s_cgrain', 'hectares'),
#                                                        by = .(y_block, IPCC_NAME, scenario)]
#   setnames(region_scen_y_ghg_m2_m_dt,  c('t_d_s_cgrain', 'hectares'), c('s_grain', 'm_hectares'))
#   setnames(region_scen_y_ghg_m2_sd_dt, c('t_d_s_cgrain', 'hectares'), c('sd_s_grain', 'sd_hectares'))
#   region_scen_y_ghg_m2_dt = region_scen_y_ghg_m2_m_dt[region_scen_y_ghg_m2_sd_dt, on = .(y_block, IPCC_NAME, scenario)]
#   setcolorder(region_scen_y_ghg_m2_dt, c('y_block', 'IPCC_NAME', 'scenario'))
# }
#-----------------------------------------------------------------------------------------
# SAVE
#-----------------------------------------------------------------------------------------
print('Saving Pareto results.')
print(Sys.time())
fwrite(robust_scenarios, file = paste(out_p, 'Pareto-solutions-2050.csv', sep = '/'))


# ## 1. max GHG mitigation (ignoring crop yield)
# m1_dt  = rbind(global_ghg_m1_dt, region_ghg_m1_dt)
# fwrite(m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential.csv', sep = '/'), append = TRUE)
# m1_y_dt = rbind(global_y_ghg_m1_dt, region_y_ghg_m1_dt)
# fwrite(m1_y_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_ghg_m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential-by-scenario.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_y_ghg_m1_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential-by-scenario.csv', sep = '/'), append = TRUE)
# # practices
# fwrite(practice_1, file = paste(out_p, 'balanced-outcomes-max-ghg-no-yield-practices.csv', sep = '/'), append = TRUE)
# 
# ## 2. max crop yield difference (ignoring GHG)
# yield_m1_dt  = rbind(global_yield_m1_dt, region_yield_m1_dt)
# fwrite(yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential.csv', sep = '/'), append = TRUE)
# m1_ghg_dt = rbind(global_g_yield_m1_dt, region_g_yield_m1_dt)
# fwrite(m1_ghg_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_g_yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential-by-scenario.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_yield_m1_dt, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential-by-scenario.csv', sep = '/'), append = TRUE)
# # practices
# fwrite(practice_2, file = paste(out_p, 'balanced-outcomes-max-yield-no-ghg-practices.csv', sep = '/'), append = TRUE)
# 
# ## 3. max GHG mitigation (no crop yield loss)
# m2_dt  = rbind(global_ghg_m2_dt, region_ghg_m2_dt)
# fwrite(m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential.csv', sep = '/'), append = TRUE)
# m2_y_dt = rbind(global_y_ghg_m2_dt, region_y_ghg_m2_dt)
# fwrite(m2_y_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_ghg_m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential-by-scenario.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_y_ghg_m2_dt, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential-by-scenario.csv', sep = '/'), append = TRUE)
# # practices
# fwrite(practice_3, file = paste(out_p, 'balanced-outcomes-max-ghg-yield-constrained-practices.csv', sep = '/'), append = TRUE)
# 
# ## 4. max crop yield difference (no GHG emissions)
# yield_m2_dt  = rbind(global_yield_m2_dt, region_yield_m2_dt)
# fwrite(yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential.csv', sep = '/'), append = TRUE)
# m2_ghg_dt = rbind(global_g_yield_m2_dt, region_g_yield_m2_dt)
# fwrite(m2_ghg_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_g_yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential-by-scenario.csv', sep = '/'), append = TRUE)
# fwrite(region_scen_yield_m2_dt, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential-by-scenario.csv', sep = '/'), append = TRUE)
# # practices
# fwrite(practice_4, file = paste(out_p, 'balanced-outcomes-max-yield-ghg-constrained-practices.csv', sep = '/'), append = TRUE)
