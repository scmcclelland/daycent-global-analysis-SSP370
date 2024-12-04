# file name:    uncertainty-somsc.R
# created:      16 November 2024
# last updated: 23 November 2024

# description: This file contains a script to complete SOC uncertainty analysis on global
#              DayCent simulations on a local machine.
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
f      = paste0(scen,'-SOC-ssp370.Rdata')
#-----------------------------------------------------------------------------------------
# Functions
#-----------------------------------------------------------------------------------------
# Calculate dSOC (g m-2 yr-1), annual
delta_soc                 = function(x) {
  delta_soc               = x - data.table::shift(x)
}
#-----------------------------------------------------------------------------------------
# DayCent data
#-----------------------------------------------------------------------------------------
load(paste(m_data, f, sep ='/'))
gc()

# restrict to complete runs (85)
SOC_rda_dt = SOC_rda_dt[run_yrs == 85,]
gc()

# climate look-up
climate_dt = fread(paste(o_data, cf, sep = '/'))
#-----------------------------------------------------------------------------------------
# LME Model Estimates
#-----------------------------------------------------------------------------------------
soc_beta   = fread(paste(LME_data, 'SOC_Beta.csv', sep = '/'))
soc_cov    = fread(paste(LME_data, 'SOC_Cov.csv', sep = '/'))
minmax_soc = fread(paste(LME_data, 'SOC_MinMax.csv', sep = '/'))

# extract values
{
    soc_cmb_beta        = soc_beta[, 2][[1]] # vector of values
    names(soc_cmb_beta) = soc_beta[, 1][[1]] # names
  
    soc_cmb_cov           = soc_cov[, -1]      # drop first col
    rownames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep row name
    colnames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep col name
    
    # Simulate betas 
    soc_sim_beta = sim.betas(fitted.betas = soc_cmb_beta,  
                             covariance   = soc_cmb_cov,  
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
      grs = unique(SOC_rda_dt[crop %in% c &
                                irr %in% ir, gridid])
      for(id in grs) {
        ## Create Data Table  ##
        # create MC dt
        dt_s = SOC_rda_dt[crop %in% c &
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
        adj_somsc_gr_dt = data.table()
        
        ## Model Creation - Monte Carlo  ##
        for (n in 1:nrep) { 
          print(paste0('Running uncertainty iteration for ', scen, ' ', c, ' ',
                       ir, ' gridid ', id, ' iteration ', n, '.'))
          {
            # Get iteration
            dt_i = dt[rep %in% n,]
            # Initialize the list to store adjusted SOC for each replicate
            adj_somsc_list = vector("list", 1) # always 1 since running each iteration separately
            
            # Prepare covariates
            SOC_fmtd = data.table(
              "(Intercept)" = 1,
              "ln_sim"      = log(dt_i$somsc) # convert to log
            )
            colnames(SOC_fmtd) = c("(Intercept)", "ln_sim")
            
          }
          ## Calculate adjusted SOC with variability for each replicate ##
          {
            adj_somsc_list[[1]] = exp(as.matrix(SOC_fmtd) %*% soc_sim_beta[n, ]) # get diff beta
            
            # Combine results into a data frame then data.table
            adj_somsc_dt           = do.call(cbind, adj_somsc_list)
            colnames(adj_somsc_dt) = paste0("adj_somsc_rep_", n)
          }
          ## Create Uncertainty Table ##
          { # keep headers
          dt_i[, somsc   := NULL]
          dt_i[, run_yrs := NULL]
          # join
          adj_somsc_dt = cbind(dt_i, adj_somsc_dt)
          # wide to long
          adj_somsc_dt = melt(adj_somsc_dt,
                              id.vars = c("gridid", "crop", "scenario", "irr", 
                                          "ssp", "gcm", "y_block", "time"),
                              measure.vars = patterns("adj_somsc_"),
                              variable.name = "rep",
                              value.name = c("adj_somsc"))
          ## Calculate cumulative change over time ##
          adj_somsc_dt[, delta_adj_somsc := delta_soc(adj_somsc), by = .(gridid, crop, 
                                                                         scenario, irr, 
                                                                         ssp, gcm, rep)]
          adj_somsc_dt[, delta_adj_somsc := replace(delta_adj_somsc, is.na(delta_adj_somsc), 0)]
          
          # Calculate dSOC (g C m-2), cumulative
          adj_somsc_dt[, delta_adj_somsc_sum := round(cumsum(delta_adj_somsc), digits = 2), 
                       by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
          # subset years for output
          adj_somsc_dt = adj_somsc_dt[time %in% c(2030,2050,2100)]
          # clean table
          adj_somsc_dt = adj_somsc_dt[, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 
                                          'gcm', 'y_block','rep', 'delta_adj_somsc_sum')]
          setcolorder(adj_somsc_dt, c('gridid', 'crop', 'irr'))
          setnames(adj_somsc_dt, 'delta_adj_somsc_sum', 's_somsc')
          adj_somsc_gr_dt = rbind(adj_somsc_gr_dt, adj_somsc_dt)
          }
        }
        print(paste0('Writing uncertainty results for ', scen, ' ', c, ' ',
                     ir, ' gridid ', id, ' to csv file.'))
        fwrite(adj_somsc_gr_dt, file = paste0(o_data, '/s_somsc-uncertainty-ssp370-',scen,'.csv'),
               append = TRUE)
        }
    }
  }
