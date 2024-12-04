# file name:    uncertainty-iN2O-script.R
# created:      16 November 2024
# last updated: 02 December 2024

# description: This file contains a script to complete indirect N2O (iN2O) uncertainty analysis 
#              on global DayCent simulations on a local machine.
#              It does the following:
#              1. creates look up table of 500 EF (truncated normal distribution) 
#              for volatilized and leached N2O emissions
#              2. estimates uncertainty for each crop, irr, gridid
#              3. each iteration (500) estimated with a different climate for each crop, irr, gridid 
#              4. output is cumulative indirect N2O (Mg iN2O ha-1)
# notes:       EF values and uncertainty ranges from 2019 refinement to 2006 IPCC guidelines.
#              Volatilized N2O: Table 11.3 and Table A2, Annex 11.A2
#              Leached N2O: Table 11.3 and Table 11.A6
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(stringr)
library(truncnorm)
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
f      = paste0(scen,'-iN2O-ssp370.Rdata')
#-----------------------------------------------------------------------------------------
# DayCent data
#-----------------------------------------------------------------------------------------
load(paste(m_data, f, sep ='/'))
gc()

# restrict to complete runs (85)
iN2O_rda_dt = iN2O_rda_dt[run_yrs == 85,]
gc()

# climate look-up
climate_dt = fread(paste(o_data, cf, sep = '/'))

# PET and precip. calculations
iN2O_rda_dt[, PET_r := round(ANNPPT/petann, digits = 0)]
iN2O_rda_dt[PET_r == Inf, PET_r := ifelse(ANNPPT >= 100, 1, 0)] # replace with 1 or 0 by ANNPPT
iN2O_rda_dt[PET_r >= 1 | ANNPPT >= 100, PET_t := 'wet'] # designated wet
iN2O_rda_dt[!PET_t %in% 'wet', PET_t   := 'dry']        # all other climates
# drop unnecessary columns
iN2O_rda_dt[, ANNPPT := NULL]
iN2O_rda_dt[, petann := NULL]
iN2O_rda_dt[, PET_r  := NULL]
# estimate volatilized and leached N, g N m-2 yr-1
iN2O_rda_dt[, vol_N  := NOflux + volpac]
iN2O_rda_dt[, lch_N  := `strmac(2)` + `strmac(6)`]
# unit conversions
  # g to kg N (kg N2O-N)
g_t_kg = 1000L
iN2O_rda_dt[, vol_N  := vol_N/g_t_kg]
iN2O_rda_dt[, lch_N  := lch_N/g_t_kg]
# drop unnecessary columns
iN2O_rda_dt[, NOflux      := NULL]
iN2O_rda_dt[, volpac      := NULL]
iN2O_rda_dt[, `strmac(2)` := NULL]
iN2O_rda_dt[, `strmac(6)` := NULL]
gc()

# constants
gmwN2O  = 44
gmwN    = 14
kgm2_t_Mgha = 10
#-----------------------------------------------------------------------------------------
# EF Estimates
#-----------------------------------------------------------------------------------------
# 2019 Refinement to IPCC 2006 Guidelines
# https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch11_Soils_N2O_CO2.pdf

# volatilized N2O
# estimate SD from 95% CI
{
cv = 1.96
moe_wet = (0.017 - 0.011)/2 # Table 11.3
ss_wet  = 648 # Table 2A.2
moe_dry = (0.011 - 0.000)/2 # Table 11.3
ss_dry  = 207 # Annex 11A.6

v_N2O_EF = data.table(climate_t = c('wet', 
                                    'dry'), 
                        mean    = c(0.014, 
                                    0.005),
                        stdev   = c(sqrt(ss_wet)*moe_wet/(cv),
                                    sqrt(ss_dry)*moe_dry/(cv)))
}
# leached N2O
# estimate SD from 95% CI
{
cv = 1.96
moe_lch = (0.020 - 0.000)/2
ss_lch  = 254

l_N2O_EF = data.table(mean    = 0.011,
                      stdev   = c((sqrt(ss_lch)*moe_lch)/cv))
}
# simulate EF uncertainty
# using truncated rnorm (truncnorm package)
{
set.seed(11162024)
v_wet_sim_EF = rtruncnorm(nrep, a = 0, b = Inf, 
                     mean = v_N2O_EF[climate_t %in% 'wet', mean], 
                     sd   = v_N2O_EF[climate_t %in% 'wet',   stdev])
set.seed(11162024)
v_dry_sim_EF = rtruncnorm(nrep, a = 0, b = Inf, 
                          mean = v_N2O_EF[climate_t %in% 'dry', mean], 
                          sd   = v_N2O_EF[climate_t %in% 'dry',   stdev])
set.seed(11162024)
l_sim_EF     = rtruncnorm(nrep, a = 0, b = Inf,
                          mean = l_N2O_EF[,mean],
                          sd   = l_N2O_EF[, stdev])
}
# create list of simulated EF values
iN2O_sim_l = as.matrix(data.frame(v_wet_sim_EF,v_dry_sim_EF,l_sim_EF))
gc()
#-----------------------------------------------------------------------------------------
# Monte Carlo Simulation
#-----------------------------------------------------------------------------------------
  for (c in crop_n) {
    for (ir in irr_l) {
      # get unique dt of grid
      grs = unique(iN2O_rda_dt[crop %in% c &
                                irr %in% ir, gridid])
      for(id in grs) {
        ## Create Data Table  ##
        # create MC dt
        dt_s = iN2O_rda_dt[crop %in% c &
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
        adj_iN2O_gr_dt = data.table()
        
        ## Model Creation - Monte Carlo  ##
        for (n in 1:nrep) { 
          print(paste0('Running uncertainty iteration for ', scen, ' ', c, ' ',
                       ir, ' gridid ', id, ' iteration ', n, '.'))
          {
            # Get iteration
            dt_i = dt[rep %in% n,]
            # Initialize the list to store adjusted dN2O for each replicate
            adj_iN2O_list = vector("list", 1) # always 1 since running each iteration separately
            
            # Prepare covariates
            iN2O_fmtd = data.table(
              "response_v_wet"    = ifelse(dt_i$PET_t %in% 'wet', 1, 0)*dt_i$vol_N,
              "response_v_dry"    = ifelse(dt_i$PET_t %in% 'dry', 1,0)*dt_i$vol_N,
              "response_lch"      = dt_i$lch_N
            )
            colnames(iN2O_fmtd) = c("response_v_wet", "response_v_dry", "response_lch")
            
          }
          ## Calculate adjusted SOC with variability for each replicate ##
          {
            adj_iN2O_list[[1]] = as.matrix(iN2O_fmtd) %*% iN2O_sim_l[n, ] # get diff EF
            
            # Combine results into a data frame then data.table
            adj_iN2O_dt            = do.call(cbind, adj_iN2O_list)
            colnames(adj_iN2O_dt)  = paste0("adj_iN2O_rep_", n)
          }
          ## Create Uncertainty Table ##
          { # keep headers
          dt_i[, PET_t     := NULL]
          dt_i[, vol_N     := NULL]
          dt_i[, lch_N     := NULL]
          # join
          adj_iN2O_dt = cbind(dt_i, adj_iN2O_dt)
          # wide to long
          adj_iN2O_dt = melt(adj_iN2O_dt,
                              id.vars = c("gridid", "crop", "scenario", "irr", 
                                          "ssp", "gcm", "y_block", "time"),
                              measure.vars = patterns("adj_iN2O_"),
                              variable.name = "rep",
                              value.name = c("adj_iN2O"))
          # Calculate iN2O flux (kg N2O-N m-2), cumulative
          adj_iN2O_dt[, adj_iN2O_sum := cumsum(adj_iN2O), 
                       by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
          # subset years for output
          adj_iN2O_dt = adj_iN2O_dt[time %in% c(2030,2050,2100)]
          # convert to Mg iN2O ha-1, just need to convert to CO2-eq later
          adj_iN2O_dt[, adj_iN2O_sum := round(adj_iN2O_sum*(gmwN2O/(gmwN*2))*kgm2_t_Mgha, digits = 5)] # Mg iN2O ha-1
          # clean table
          adj_iN2O_dt = adj_iN2O_dt[, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 
                                          'gcm', 'y_block','rep', 'adj_iN2O_sum')]
          setcolorder(adj_iN2O_dt, c('gridid', 'crop', 'irr'))
          setnames(adj_iN2O_dt, 'adj_iN2O_sum', 's_iN2Oflux')
          adj_iN2O_gr_dt = rbind(adj_iN2O_gr_dt, adj_iN2O_dt)
          }
        }
        print(paste0('Writing uncertainty results for ', scen, ' ', c, ' ',
                     ir, ' gridid ', id, ' to csv file.'))
        fwrite(adj_iN2O_gr_dt, file = paste0(o_data, '/s_iN2Oflux-uncertainty-ssp370-',scen,'.csv'),
               append = TRUE)
        }
    }
  }
