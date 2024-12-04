# file name:    uncertainty-dN2O-script.R
# created:      16 November 2024
# last updated: 29 November 2024

# description: This file contains a script to complete direct N2O (dN2O) uncertainty analysis 
#              on global DayCent simulations on a local machine.
#              It does the following:
#              1. creates look up table of 500 simulated betas from LME
#              2. estimates uncertainty for each crop, irr, gridid
#              3. each iteration (500) estimated with a different climate for each crop, irr, gridid 
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# ARGUMENTS
#-----------------------------------------------------------------------------------------
args   = commandArgs(trailingOnly = TRUE)
if (isFALSE(length(args) == 1))
  stop(
    'Need 1 command-line arguments (1. Scenario selection for uncertainty).'
  )
# args[1] Scenario selection (conv, ccg-res, ccl-res, ccg-ntill, ccl-ntill)
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FUNCTIONS
#-----------------------------------------------------------------------------------------
dir = getwd()
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
source("data/LME_data/SimBetas_func.R")
LME_data = paste(dir, 'data/LME_data', sep = '/')
m_data   = paste(dir, 'data/daycent-simulations', sep = '/')
o_data   = paste(dir, 'data/uncertainty-output', sep = '/')
#-----------------------------------------------------------------------------------------
# Set MC conditions
#-----------------------------------------------------------------------------------------
# number of replicates   
nrep   = 500
# run
crop_n = c('maiz', 'soyb', 'swht')
irr_l  = c(0,1)
cf     = 'uncertainty-climate-look-up-table.csv'

# scenario
scen   = args[1]
f      = paste0(scen,'-dN2O-ssp370.Rdata')
#-----------------------------------------------------------------------------------------
# DayCent data
#-----------------------------------------------------------------------------------------
load(paste(m_data, f, sep ='/'))
gc()

# restrict to complete runs (85)
dN2O_rda_dt = dN2O_rda_dt[run_yrs == 85,]
gc()

# climate look-up
climate_dt = fread(paste(o_data, cf, sep = '/'))

# input table
load(paste(m_data, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
main_table = main_table[,c('gridid','crop', 'irr','fertN.amt')]
# change to 0,1
main_table[fertN.amt > 0, fertN.amt := 1]
# join N fertilizer
dN2O_rda_dt = dN2O_rda_dt[main_table, on = .(gridid = gridid,
                                             crop   = crop,
                                             irr    = irr)]
dN2O_rda_dt = dN2O_rda_dt[complete.cases(dN2O_rda_dt)]
rm(main_table)
gc()
#-----------------------------------------------------------------------------------------
# LME Model Estimates
#-----------------------------------------------------------------------------------------
dN2O_beta   = fread(paste(LME_data, 'N2OCroplandBeta.csv', sep = '/'))
dN2O_cov    = fread(paste(LME_data, 'N2OCroplandCov.csv', sep = '/'))
minmax_dN2O = fread(paste(LME_data, 'N2OCroplandMinMax.csv', sep = '/'))

# extract values
{
    dN2O_cmb_beta        = dN2O_beta[, 2][[1]] # vector of values
    names(dN2O_cmb_beta) = dN2O_beta[, 1][[1]] # names
  
    dN2O_cmb_cov           = dN2O_cov[, -1]      # drop first col
    rownames(dN2O_cmb_cov) = dN2O_cov[, 1][[1]]  # keep row name
    colnames(dN2O_cmb_cov) = dN2O_cov[, 1][[1]]  # keep col name
    
    # Simulate betas 
    dN2O_sim_beta = sim.betas(fitted.betas = dN2O_cmb_beta,  
                             covariance   = dN2O_cmb_cov,  
                             nreps        = nrep, 
                             iseed        = 11162024)
    
}
gc()
#-----------------------------------------------------------------------------------------
# Monte Carlo Simulation
#-----------------------------------------------------------------------------------------
  for (c in crop_n) {
    for (ir in irr_l) {
      # get unique dt of grid
      grs = unique(dN2O_rda_dt[crop %in% c &
                                irr %in% ir, gridid])
      for(id in grs) {
        ## Create Data Table  ##
        # create MC dt
        dt_s = dN2O_rda_dt[crop %in% c &
                          irr %in% ir &
                          gridid %in% id,]
        if (NROW(dt_s) < 2040) {next()} # skip if less than 24 gcm
        # climate look-up
        {
        dt_c = climate_dt[gridid %in% id,]
          if (NROW(dt_c) < 500) {next()} # skip if missing
        # create dt table with responses for selected climate for each iteration
        dt = rbindlist(lapply(dt_c[, gcm], function(cl) {
          dt_s[gcm %in% cl,]
        }))
        # bind to rep
        reps = data.table(rep = rep(1:nrep,85))
        setorder(reps, rep)
        dt = cbind(dt, reps)
        setcolorder(dt, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                          'y_block', 'time', 'run_yrs', 'rep'))
        }
        ## Empty dt for storing results by gridid ##
        adj_dN2O_gr_dt = data.table()
        
        ## Model Creation - Monte Carlo  ##
        for (n in 1:nrep) { 
          print(paste0('Running uncertainty iteration for ', scen, ' ', c, ' ',
                       ir, ' gridid ', id, ' iteration ', n, '.'))
          {
            # Get iteration
            dt_i = dt[rep %in% n,]
            # Initialize the list to store adjusted dN2O for each replicate
            adj_dN2O_list = vector("list", 1) # always 1 since running each iteration separately
            
            # Prepare covariates
            dN2O_fmtd = data.table(
              "(Intercept)"       = 1,
              "ln_sim"            = log(dt_i$N2Oflux), # convert to log
              "corn"              = ifelse(unique(dt_i$crop) %in% 'maiz', 1,0),
              "nfert_bool"        = ifelse(unique(dt_i$fertN.amt) == 1, 1, 0),
              "ln_sim:nfert_bool" = ifelse(unique(dt_i$fertN.amt) == 1, 1, 0)
            )
            colnames(dN2O_fmtd) = c("(Intercept)", "ln_sim", "corn", "nfert_bool", "ln_sim:nfert_bool")
            
          }
          ## Calculate adjusted SOC with variability for each replicate ##
          {
            adj_dN2O_list[[1]] = exp(as.matrix(dN2O_fmtd) %*% dN2O_sim_beta[n, ]) # get diff beta
            
            # Combine results into a data frame then data.table
            adj_dN2O_dt            = do.call(cbind, adj_dN2O_list)
            colnames(adj_dN2O_dt)  = paste0("adj_dN2O_rep_", n)
          }
          ## Create Uncertainty Table ##
          { # keep headers
          dt_i[, N2Oflux   := NULL]
          dt_i[, run_yrs   := NULL]
          dt_i[, fertN.amt := NULL]
          # join
          adj_dN2O_dt = cbind(dt_i, adj_dN2O_dt)
          # wide to long
          adj_dN2O_dt = melt(adj_dN2O_dt,
                              id.vars = c("gridid", "crop", "scenario", "irr", 
                                          "ssp", "gcm", "y_block", "time"),
                              measure.vars = patterns("adj_dN2O_"),
                              variable.name = "rep",
                              value.name = c("adj_dN2O"))
          # Calculate dN2O flux (g N m-2), cumulative
          adj_dN2O_dt[, adj_dN2O_sum := cumsum(adj_dN2O), 
                       by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
          # subset years for output
          adj_dN2O_dt = adj_dN2O_dt[time %in% c(2030,2050,2100)]
          # clean table
          adj_dN2O_dt = adj_dN2O_dt[, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 
                                          'gcm', 'y_block','rep', 'adj_dN2O_sum')]
          setcolorder(adj_dN2O_dt, c('gridid', 'crop', 'irr'))
          setnames(adj_dN2O_dt, 'adj_dN2O_sum', 's_N2Oflux')
          adj_dN2O_gr_dt = rbind(adj_dN2O_gr_dt, adj_dN2O_dt)
          }
        }
        print(paste0('Writing uncertainty results for ', scen, ' ', c, ' ',
                     ir, ' gridid ', id, ' to csv file.'))
        fwrite(adj_dN2O_gr_dt, file = paste0(o_data, '/s_N2Oflux-uncertainty-ssp370-',scen,'.csv'),
               append = TRUE)
        }
    }
  }
