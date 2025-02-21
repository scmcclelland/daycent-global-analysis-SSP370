# filename:     check.R
# created:      21 February 2025
# last updated: 21 February 2025
# description:  Execute this script to create the directory structure required for the 
#               R project. Once created, downloaded data from Zenodo repo can be added
#               and unzipped
#-----------------------------------------------------------------------------------------
library(rstudioapi)
library(stringr)
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir) 
# this directory should contain the sub-directory /r, /data, and /figures. 
# it is the main working dir

# CHECKS  
  ## /r
req_r   = c("analysis-functions.R", "analysis-hectare-gridid.R",           
            "analysis-hectare-weighted-gridid.R","analysis-hectare.R",                  
            "analysis-ML.R", "analysis-potential-bootstrap.R",      
            "analysis-potential.R", "analysis-recommended-practice-2050.R",
            "analysis-recommended-practice-2100.R", "check.R",                             
            "figure-functions.R", "figures-extended-data.R",             
            "figures-main.R", "figures-si.R",                        
            "impute-functions.R", "impute-missing-gcm.R",                
            "impute-missing-uncertainty.R", "make-dirs.R",                         
            "plot_discrete_color_bar.R", "scenario-crop-estimates.R",           
            "scenario-ghg-estimates.R", "uncertainty-climate-selection.R",     
            "uncertainty-dN2O-script.R", "uncertainty-iN2O-script.R",           
            "uncertainty-somsc-script.R" )
check_r = list.files(paste(dir, 'r', sep = '/'), '.R')
diff_r  = setdiff(req_r, check_r)

if(length(diff_r) == 0) {
  print('All /r files present in directory.')
} else {
  print(paste0('WARNING: missing files in /r directory.', ' Please download the following script: ', 
               diff_r, '.'))
}
  ## /data
# /daycent-simulation (OPTIONAL)
req_d_sim   = c("ccg-ntill-covercrop-biomass-ssp370.Rdata", "ccg-ntill-dN2O-ssp370.Rdata",             
                "ccg-ntill-grain-covariates-ssp370.Rdata",  "ccg-ntill-iN2O-ssp370.Rdata",             
                "ccg-ntill-SOC-ssp370.Rdata",               "ccg-res-covercrop-biomass-ssp370.Rdata",  
                "ccg-res-dN2O-ssp370.Rdata",                "ccg-res-grain-covariates-ssp370.Rdata",   
                "ccg-res-iN2O-ssp370.Rdata",                "ccg-res-SOC-ssp370.Rdata",                
                "ccl-ntill-covercrop-biomass-ssp370.Rdata", "ccl-ntill-dN2O-ssp370.Rdata",             
                "ccl-ntill-grain-covariates-ssp370.Rdata",  "ccl-ntill-iN2O-ssp370.Rdata",             
                "ccl-ntill-SOC-ssp370.Rdata",               "ccl-res-covercrop-biomass-ssp370.Rdata",  
                "ccl-res-dN2O-ssp370.Rdata",                "ccl-res-grain-covariates-ssp370.Rdata",   
                "ccl-res-iN2O-ssp370.Rdata",                "ccl-res-SOC-ssp370.Rdata",                
                "conv-dN2O-ssp370.Rdata",                   "conv-grain-covariates-ssp370.Rdata",      
                "conv-iN2O-ssp370.Rdata",                   "conv-SOC-ssp370.Rdata",                   
                "input_table_by_gridid_crop_irr.RData",     "msw-masked-cropland-rf-ir-area.tif")
check_d_sim = list.files(paste(dir, 'data/daycent-simulations', sep = '/'), '.')
diff_d_sim  = setdiff(req_d_sim, check_d_sim)

if(length(diff_d_sim) == 0) {
  print('All /data/daycent-simulation files present in directory.')
} else {
  print(paste0('WARNING: There are missing files in the /data/daycent-simulation directory.',
               'These files are only required if user wants to recreate uncertainty analysis.'))
  print(paste0('Please download the missing file in /data/daycent-simulation directory: ',  
              diff_d_sim, '.'))
}
# /uncertainty-output
req_d_unc   = c("s_iN2Oflux-uncertainty-ssp370-ccg-ntill.csv",     "s_iN2Oflux-uncertainty-ssp370-ccg-res.csv",      
                "s_iN2Oflux-uncertainty-ssp370-ccl-ntill.csv",     "s_iN2Oflux-uncertainty-ssp370-ccl-res.csv",      
                "s_iN2Oflux-uncertainty-ssp370-conv.csv",          "s_N2Oflux-uncertainty-ssp370-ccg-ntill.csv",     
                "s_N2Oflux-uncertainty-ssp370-ccg-res.csv",        "s_N2Oflux-uncertainty-ssp370-ccl-ntill.csv",     
                "s_N2Oflux-uncertainty-ssp370-ccl-res.csv",        "s_N2Oflux-uncertainty-ssp370-conv.csv",          
                "s_somsc-uncertainty-ssp370-ccg-ntill.csv",        "s_somsc-uncertainty-ssp370-ccg-res.csv",         
                "s_somsc-uncertainty-ssp370-ccl-ntill.csv",        "s_somsc-uncertainty-ssp370-ccl-res.csv",         
                "s_somsc-uncertainty-ssp370-conv.csv",             "s_somsc-uncertainty-ssp370-soyb-wht-ccg-res.csv",
                "uncertainty-climate-look-up-table.csv")
check_d_unc = list.files(paste(dir, 'data/uncertainty-output', sep = '/'), '.')
diff_d_unc  = setdiff(req_d_unc, check_d_unc)

if(length(diff_d_unc) == 0) {
  print('All /data/uncertainty-output files present in directory.')
} else {
  print(paste0('WARNING: There are missing files in the /data/uncertainty-output directory.',
               'These files are only required if user wants to recreate post-processed DayCent files.'))
  print(paste0('Please download the missing file in /data/uncertainty-output directory: ',  
               diff_d_unc, '.'))
}
# /daycent-post-processed
req_d_pp   = c("ccg-ntill-ghg-flux-imputed.csv",               "ccg-ntill-ghg-flux-uncertainty.RData",        
               "ccg-ntill-yield-imputed.csv",                  "ccg-ntill-yield.RData",                       
               "ccg-res-ghg-flux-imputed.csv",                 "ccg-res-ghg-flux-uncertainty.RData",          
               "ccg-res-yield-imputed.csv",                    "ccg-res-yield.RData",                         
               "ccl-ntill-ghg-flux-imputed.csv",               "ccl-ntill-ghg-flux-uncertainty.RData",        
               "ccl-ntill-yield-imputed.csv",                  "ccl-ntill-yield.RData",                       
               "ccl-res-ghg-flux-imputed.csv",                 "ccl-res-ghg-flux-uncertainty.RData",          
               "ccl-res-yield-imputed.csv",                    "ccl-res-yield.RData",                         
               "conv-absolute-ghg-flux-uncertainty.RData",     "conv-absolute-yield.RData",                   
               "cover-crop-crop-area-country-ipcc-region.rds", "input_site_data.csv",                         
               "input_table_by_gridid_crop_irr.RData",         "input_weather_data_ssp370.csv",               
               "msw-masked-cropland-rf-ir-area.tif",           "shp")
check_d_pp = list.files(paste(dir, 'data/daycent-post-processed', sep = '/'), '.')
diff_d_pp  = setdiff(req_d_pp, check_d_pp)

if(length(diff_d_pp) == 0) {
  print('All /data/daycent-post-processed files present in directory.')
} else {
  print(paste0('ERROR: There are missing files in the /data/daycent-post-processed directory.',
               'These files are required if user wants to recreate analyses and files.',
               'Otherwise, user can use files from Zenodo repo in analysis-output.tar.gz'))
  print(paste0('Please download the missing file in /data/daycent-post-processed directory: ',  
               diff_d_pp, '.'))
}
# /analysis-output
req_d_ao   = c("balanced-outcomes-max-ghg-no-yield-crop-potential-by-scenario.csv",               
               "balanced-outcomes-max-ghg-no-yield-crop-potential.csv",                           
               "balanced-outcomes-max-ghg-no-yield-mitigation-potential-by-scenario.csv",         
               "balanced-outcomes-max-ghg-no-yield-mitigation-potential.csv",                     
               "balanced-outcomes-max-ghg-no-yield-practices.csv",                                
               "balanced-outcomes-max-ghg-yield-constrained-crop-potential-by-scenario.csv",      
               "balanced-outcomes-max-ghg-yield-constrained-crop-potential.csv",                  
               "balanced-outcomes-max-ghg-yield-constrained-mitigation-potential-by-scenario.csv",
               "balanced-outcomes-max-ghg-yield-constrained-mitigation-potential.csv",            
               "balanced-outcomes-max-ghg-yield-constrained-practices.csv",                       
               "balanced-outcomes-max-yield-ghg-constrained-crop-potential-by-scenario.csv",      
               "balanced-outcomes-max-yield-ghg-constrained-crop-potential.csv",                  
               "balanced-outcomes-max-yield-ghg-constrained-mitigation-potential-by-scenario.csv",
               "balanced-outcomes-max-yield-ghg-constrained-mitigation-potential.csv",            
               "balanced-outcomes-max-yield-ghg-constrained-practices.csv",                       
               "balanced-outcomes-max-yield-no-ghg-crop-potential-by-scenario.csv",               
               "balanced-outcomes-max-yield-no-ghg-crop-potential.csv",                           
               "balanced-outcomes-max-yield-no-ghg-mitigation-potential-by-scenario.csv",         
               "balanced-outcomes-max-yield-no-ghg-mitigation-potential.csv",                     
               "balanced-outcomes-max-yield-no-ghg-practices.csv",                                
               "ccg_ntill_SHAP.Rdata",                                                            
               "ccg_res_SHAP.Rdata",                                                              
               "ccg-ntill-cumulative-ghg-potential.csv",                                          
               "ccg-ntill-cumulative-yield-potential.csv",                                        
               "ccg-ntill-gridid-ghg-responses.csv",                                              
               "ccg-ntill-gridid-yield-responses.csv",                                            
               "ccg-ntill-hectare-ghg-responses.csv",                                             
               "ccg-ntill-hectare-yield-responses.csv",                                           
               "ccg-ntill-weighted-gridid-ghg-responses.csv",                                     
               "ccg-ntill-weighted-gridid-yield-responses.csv",                                   
               "ccg-res-cumulative-ghg-potential.csv",                                            
               "ccg-res-cumulative-yield-potential.csv",                                          
               "ccg-res-gridid-ghg-responses.csv",                                                
               "ccg-res-gridid-yield-responses.csv",                                              
               "ccg-res-hectare-ghg-responses.csv",                                               
               "ccg-res-hectare-yield-responses.csv",                                             
               "ccg-res-weighted-gridid-ghg-responses.csv",                                       
               "ccg-res-weighted-gridid-yield-responses.csv",                                     
               "ccl_ntill_SHAP.Rdata",                                                            
               "ccl_res_SHAP.Rdata",                                                              
               "ccl-ntill-cumulative-ghg-potential.csv",                                          
               "ccl-ntill-cumulative-yield-potential.csv",                                        
               "ccl-ntill-gridid-ghg-responses.csv",                                              
               "ccl-ntill-gridid-yield-responses.csv",                                            
               "ccl-ntill-hectare-ghg-responses.csv",                                             
               "ccl-ntill-hectare-yield-responses.csv",                                           
               "ccl-ntill-weighted-gridid-ghg-responses.csv",                                     
               "ccl-ntill-weighted-gridid-yield-responses.csv",                                   
               "ccl-res-cumulative-ghg-potential.csv",                                            
               "ccl-res-cumulative-yield-potential.csv",                                          
               "ccl-res-gridid-ghg-responses.csv",                                                
               "ccl-res-gridid-yield-responses.csv",                                             
               "ccl-res-hectare-ghg-responses.csv",                                               
               "ccl-res-hectare-yield-responses.csv",                                             
               "ccl-res-weighted-gridid-ghg-responses.csv",                                       
               "ccl-res-weighted-gridid-yield-responses.csv")
check_d_ao = list.files(paste(dir, 'data/analysis-output', sep = '/'), '.')
diff_d_ao  = setdiff(req_d_ao, check_d_ao)

if(length(diff_d_ao) == 0) {
  print('All /data/analysis-output files present in directory.')
} else {
  print(paste0('ERROR: There are missing files in the /data/analysis-output directory.',
               'These files are required if user wants to review analysis output.',
               'These files are also required if user wants to create / modify figures.'))
  print(paste0('Please download the missing file in /data/analysis-output directory: ',  
               diff_d_ao, '.'))
}
  ## /figures
f_dirs       = c('ext', 'main', 'si')
req_f_dirs   = as.vector(unlist(outer(paste0(dir, '/figures'), f_dirs, paste, sep = '/')))
req_f_dirs   = c(paste0(dir, '/figures'), req_f_dirs)
check_f_dirs = list.dirs(paste(dir, 'figures', sep = '/'))

diff_f_dirs  = setdiff(req_f_dirs, check_f_dirs)

if(length(diff_f_dirs) == 0) {
  print('All /figure sub-directories present.')
} else {
  print(paste0('WARNING: There are missing subirectories in the /figures directory.',
               'These directories are required if user wants to create / modify figures.'))
  print(paste0('Please create the missing directory in /figures: ',  
               diff_f_dirs, '.'))
}

## CHECK COMPLETE ##