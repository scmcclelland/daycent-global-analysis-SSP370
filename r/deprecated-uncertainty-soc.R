# file name:    uncertainty-soc.R
# created:      16 November 2024
# last updated: 16 November 2024

# description: This file contains a script to complete SOC uncertainty analysis on global
#              DayCent simulations on a local machine.
#              It does the following:
#             
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FUNCTIONS
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
source("data/LME_data/SimBetas_func.R")
LME_data = paste(dir, 'data/LME_data', sep = '/')
m_data   = paste(dir, 'data/daycent-post-processed', sep = '/')
#-----------------------------------------------------------------------------------------
# Set MC conditions
#-----------------------------------------------------------------------------------------
# number of replicates   
nrep   = 10000
# run
crop_n = 'maiz'
irr_l  = 0
scen   = 'ccg-res'
f      = 'ccg-res-SOC-ssp370.Rdata'
#-----------------------------------------------------------------------------------------
# DayCent data
#-----------------------------------------------------------------------------------------
load(paste(m_data, f, sep ='/'))
gc()

# FOR TESTING - simplify dt
SOC_rda_dt = SOC_rda_dt[crop %in% crop_n &
                          irr %in% irr_l]
SOC_rda_dt = SOC_rda_dt[gridid == 29220 & gcm %like% 'Can'] # NEED TO DEAL WITH CLIMATE
gc()
#-----------------------------------------------------------------------------------------
# LME Model Estimates
#-----------------------------------------------------------------------------------------
soc_beta   = fread(paste(LME_data, 'SOC_Beta.csv', sep = '/'))
soc_cov    = fread(paste(LME_data, 'SOC_Cov.csv', sep = '/'))
sigma_soc  = fread(paste(LME_data, 'SOC_Sigma.csv', sep = '/'))
minmax_soc = fread(paste(LME_data, 'SOC_MinMax.csv', sep = '/'))

# extract values
{
    soc_cmb_beta        = soc_beta[, 2][[1]] # vector of values
    names(soc_cmb_beta) = soc_beta[, 1][[1]] # names
  
    soc_cmb_cov           = soc_cov[, -1]      # drop first col
    rownames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep row name
    colnames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep col name
}
#-----------------------------------------------------------------------------------------
# Monte Carlo Simulation
#-----------------------------------------------------------------------------------------
  ## Random Effects##

# set seed for random effects
set.seed(11162024)
# Extract variance components from the model
sigma_site    = sigma_soc[, sigma_ran.site]
sigma_site_yr = sigma_soc[, ran_site_yr]

# Get unique sites and site-years
n_sites  = length(unique(SOC_rda_dt[,gridid]))
site_ids = unique(SOC_rda_dt[,gridid])

# Create site-level random effects
site_effects = matrix(
  rnorm(n = n_sites * nrep, mean = 0, sd = sigma_site),
  nrow    = n_sites, ncol = nrep
)

# Create site-year random effects
n_rows          = NROW(SOC_rda_dt)
site_yr_effects = matrix( 
  rnorm(n = n_rows * nrep, mean = 0, sd = sigma_site_yr), 
  nrow = n_rows, ncol = nrep
)

# Map site effects to each observation
site_effects_mapped = matrix(0, nrow = n_rows, ncol = nrep)
for (i in 1:n_rows) {
  site_idx                 = match(SOC_rda_dt[,gridid][i], site_ids)
  site_effects_mapped[i, ] = site_effects[site_idx, ]
}

# Combine random effects
random_effects_combined = site_effects_mapped + site_yr_effects

  ## Model Creation - Monte Carlo  ##
{
# Initialize the list to store adjusted SOC for each replicate
adj_somsc_list = vector("list", nrep)

# Prepare covariates
SOC_fmtd = data.table(
  "(Intercept)" = 1,
  "ln_sim"      = log(SOC_rda_dt$somsc) # convert to log
)
colnames(SOC_fmtd) = c("(Intercept)", "ln_sim")

# Simulate betas
soc_sim_beta = sim.betas(fitted.betas = soc_cmb_beta,  
                          covariance = soc_cmb_cov,  
                          nreps = nrep, 
                          iseed = 11162024)

# Extract sigma for random variability
sigma_residual = sigma_soc[,sigma_residual][[1]] 

# set seed for random variability
set.seed(11162024)

# Simulate random variability using sigma
random_variability = matrix(
  rnorm(n = nrow(SOC_rda_dt) * nrep, mean = 0, sd = sigma_residual), 
  nrow = nrow(SOC_rda_dt), ncol = nrep
  )
}

# Calculate adjusted SOC with variability for each replicate
{for (i in 1:nrep) {
  adj_somsc_list[[i]] = exp(as.matrix(SOC_fmtd) %*% soc_sim_beta[i, ] + 
                              random_effects_combined[, i] + 
                              random_variability[, i])
}

# Combine results into a data frame
adj_somsc_df           = do.call(cbind, adj_somsc_list)
colnames(adj_somsc_df) = paste0("adj_somsc_rep_", 1:nrep)

# Assign a unique ID to each SOC_rda_dt row
SOC_rda_dt[, id := .I]

# JOIN SOC_rda_dt to adj_somsc_df
adj_somsc_dt = cbind(SOC_rda_dt, adj_somsc_df)

# WIDE to LONG FORMAT
adj_somsc_dt = melt(adj_somsc_dt,
     id.vars = c("id", "gridid", "crop", "scenario", "irr", 
                 "ssp", "gcm", "y_block", "time", "run_yrs", "somsc"),
     measure.vars = patterns("adj_somsc_"),
     variable.name = "rep",
     value.name = c("adj_somsc"))
}

# Calculate dSOC (g m-2 yr-1), annual
delta_soc                 = function(x) {
  delta_soc               = x - data.table::shift(x)
}

adj_somsc_dt[, delta_adj_somsc := delta_soc(adj_somsc), by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
adj_somsc_dt[, delta_adj_somsc := replace(delta_adj_somsc, is.na(delta_adj_somsc), 0)]

# Calculate dSOC (g m-2), cumulative
adj_somsc_dt[, delta_adj_somsc_sum := round(cumsum(delta_adj_somsc), digits = 2), 
      by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]

mean(adj_somsc_dt[time == 2100, delta_adj_somsc_sum])
sd(adj_somsc_dt[time == 2100, delta_adj_somsc_sum])

# iteration test | for 208303 gridid didn't really help, for 29220 didn't really help either
adj_somsc_dt_500  = copy(adj_somsc_dt)
adj_somsc_dt_1000 = copy(adj_somsc_dt)
adj_somsc_dt_10k  = copy(adj_somsc_dt)


mean(adj_somsc_dt_500[time == 2100, delta_adj_somsc_sum])
sd(adj_somsc_dt_500[time == 2100, delta_adj_somsc_sum])

mean(adj_somsc_dt_1000[time == 2100, delta_adj_somsc_sum])
sd(adj_somsc_dt_1000[time == 2100, delta_adj_somsc_sum])

mean(adj_somsc_dt_10k[time == 2100, delta_adj_somsc_sum])
sd(adj_somsc_dt_10k[time == 2100, delta_adj_somsc_sum])

# What to keep? Cumulative only
# mean dSOC for each year
# mean dSOC cumulative

