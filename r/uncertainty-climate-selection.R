# file name:    uncertainty-climate-selection.R
# created:      21 November 2024
# last updated: 21 November 2024

# description: This file contains a script to complete random climate selection for each
#              uncertainty iteration.
#              It does the following:
#              Selects a random climate among the 24 gcm and assigns it to one of 500 iterations
#              for each gridid. The resulting table is save as output and used as input for
#              all Monte Carlo uncertainty estimates.
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FILES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
m_data   = paste(dir, 'data/daycent-simulations', sep = '/')
o_data   = paste(dir, 'data/uncertainty-output', sep = '/')
f      = 'conv-SOC-ssp370.Rdata'
#-----------------------------------------------------------------------------------------
# Load simulation output
#-----------------------------------------------------------------------------------------
# note: only one response for one scenario required
load(paste(m_data, f, sep ='/'))
gc()

# restrict to complete runs (85)
SOC_rda_dt = SOC_rda_dt[run_yrs == 85,]
gc()
#-----------------------------------------------------------------------------------------
# Random climate selection for 500 iterations
#-----------------------------------------------------------------------------------------
set.seed(11162024)
gcms  = unique(SOC_rda_dt[, gcm])
grs   = unique(SOC_rda_dt[,gridid])
nreps = 500
pos   = sample(1:length(gcms), length(grs)*500, replace = TRUE) # get gcm name
gcm_s = c(gcms[pos]) # create vector

# create new dt
climate_selection = data.table(gridid = rep(grs, 500),
                               gcm    = gcm_s)
setorder(climate_selection, gridid)
# add iteration rep
climate_selection[, rep := 1:500, by = gridid]

# save
fwrite(climate_selection, file = paste(o_data, 'uncertainty-climate-look-up-table.csv', 
                                       sep = '/'))
