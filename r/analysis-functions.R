# file name:    analysis-functions.R
# created:      18 November 2024
# last updated: 21 February 2025
#-----------------------------------------------------------------------------------------
# analysis-hectare
#-----------------------------------------------------------------------------------------
  ## CALCULATE GRID CELL CROP AREA ##
grid_crop_area = function(crop_r) {
  crop_r$cell_area = cellSize(crop_r, mask=FALSE, lyrs=FALSE, unit="ha")
  crop_area_dt     = as.data.table(terra::as.data.frame(crop_r, xy = TRUE, cells = TRUE))
  crop_area_dt     = crop_area_dt[maize_rainfed_2015 > 0     |
                                    maize_irrigated_2015 > 0   |
                                    soybean_rainfed_2015 > 0   |
                                    soybean_irrigated_2015 > 0 |
                                    wheat_rainfed_2015 > 0     |
                                    wheat_irrigated_2015 > 0,]
  crop_area_dt[is.na(wheat_rainfed_2015),  wheat_rainfed_2015    := 0]
  crop_area_dt[is.na(wheat_irrigated_2015), wheat_irrigated_2015 := 0]
  # round all (1 digit)
  crop_area_dt = crop_area_dt[, lapply(.SD, round, digits = 1), .SDcols = c('maize_rainfed_2015',
                                                                            'maize_irrigated_2015',
                                                                            'soybean_rainfed_2015',
                                                                            'soybean_irrigated_2015',
                                                                            'wheat_rainfed_2015',
                                                                            'wheat_irrigated_2015'),
                              by = .(cell)]
  # sum crop area
  crop_area_dt[, sum_crop_2015 := maize_rainfed_2015 + maize_irrigated_2015 +
                 soybean_rainfed_2015 + soybean_irrigated_2015 +
                 wheat_rainfed_2015 + wheat_irrigated_2015]
  # remove < 1
  crop_area_dt = crop_area_dt[sum_crop_2015 >= 1,]
  setnames(crop_area_dt, 'cell', 'gridid')
  return(crop_area_dt)
}
  ## ADD IPCC NAME
add_region     = function(dt, main_table, ipcc_region_dt) {
  dt  = dt[main_table[gridid %in% unique(dt[,gridid])],
                               on = .(gridid = gridid)]
  dt  = dt[ipcc_region_dt[, .(WB_NAME, IPCC_NAME)], 
                               on = .(WB_NAME = WB_NAME) ]
  dt  = dt[!is.na(gridid)]
  dt[, WB_NAME := NULL]
  gc()
  return(dt)
}
  ## ADD CROP AREA WEIGHTS ##
add_weights    = function(dt, crop_dt) {
  dt = dt[crop_dt, on = .(gridid = gridid)]
  dt = dt[!is.na(crop)]
  dt[crop %in% 'maiz' & irr == 0, crop_area_2015 := maize_rainfed_2015]
  dt[crop %in% 'maiz' & irr == 1, crop_area_2015 := maize_irrigated_2015]
  dt[crop %in% 'soyb' & irr == 0, crop_area_2015 := soybean_rainfed_2015]
  dt[crop %in% 'soyb' & irr == 1, crop_area_2015 := soybean_irrigated_2015]
  dt[crop %in% 'swht' & irr == 0, crop_area_2015 := wheat_rainfed_2015]
  dt[crop %in% 'swht' & irr == 1, crop_area_2015 := wheat_irrigated_2015]
  dt = dt[, -c('maize_rainfed_2015', 'maize_irrigated_2015',
                                   'soybean_rainfed_2015', 'soybean_irrigated_2015',
                                   'wheat_rainfed_2015', 'wheat_irrigated_2015')]
  # normalized weight
  dt[, norm_w := round(crop_area_2015/sum_crop_2015, digits = 1)]
  # weight
  dt[, w      := norm_w*sum_crop_2015]
  # drop normalized weight
  dt[, norm_w := NULL]
  return(dt)
}
## ADD LOCAL CROP AREA WEIGHTS ##
add_l_weights    = function(dt, crop_dt) {
  dt = dt[crop_dt, on = .(gridid = gridid)]
  dt = dt[!is.na(crop)]
  dt[crop %in% 'maiz' & irr == 0, crop_area_2015 := maize_rainfed_2015]
  dt[crop %in% 'maiz' & irr == 1, crop_area_2015 := maize_irrigated_2015]
  dt[crop %in% 'soyb' & irr == 0, crop_area_2015 := soybean_rainfed_2015]
  dt[crop %in% 'soyb' & irr == 1, crop_area_2015 := soybean_irrigated_2015]
  dt[crop %in% 'swht' & irr == 0, crop_area_2015 := wheat_rainfed_2015]
  dt[crop %in% 'swht' & irr == 1, crop_area_2015 := wheat_irrigated_2015]
  dt = dt[, -c('maize_rainfed_2015', 'maize_irrigated_2015',
               'soybean_rainfed_2015', 'soybean_irrigated_2015',
               'wheat_rainfed_2015', 'wheat_irrigated_2015')]
  # normalized weight
  dt[, w := round(crop_area_2015/sum_crop_2015, digits = 1)]
  return(dt)
}
  ## ESTIMATE WEIGHTED STANDAR DEVIATION
weighted_sd = function(values, weights) {
  
  # Calculate weighted mean (using the previous method)
  weighted_mean = sum(values * weights) / sum(weights)
  
  # Calculate weighted variance
  # Squared difference from the weighted mean, multiplied by combined weights
  weighted_variance = sum(weights * (values - weighted_mean)^2) / 
    ((sum(weights) - 1) / sum(weights) * sum(weights))
  
  # Return standard deviation (square root of variance)
  return(sqrt(weighted_variance))
}

  ## CREATE CROP AREA DT ##
create_crop_dt = function(country.sf, crop_r) {
  country.sf_dt = setDT(as.data.frame(country.sf))
  # CREATE raster
  shp_r       = rast(ext(country.sf), nrow = 360, ncol = 720)
  # CREATE shp as raster
  country_r   = terra::rasterize(country.sf, shp_r, fun = 'sum', "OBJECTID")
  # MATCH resolution of simulation data, dimensions the same
  target.r    = rast(nrow = 360, ncol = 720, resolution = 0.5)
  country_r   = resample(country_r, target.r, method = "near")
  country_r   = focal(country_r, w=9, fun = "modal", na.policy = "only", na.rm = TRUE) # needed to capture all gridid
  names(country_r) = "OBJECTID"
  # CREATE data.frame, merge
  country_r.dt    = as.data.frame(country_r, cells=TRUE, xy=TRUE)
  country_r.dt    = setDT(country_r.dt)
  country_n       = data.table(WB_NAME = country.sf_dt$WB_NAME, ID = country.sf_dt$OBJECTID,
                               WB_REGION = country.sf_dt$WB_REGION)
  # BIND to cell numbers
  country_r.dt = country_r.dt[country_n, on = .(OBJECTID = ID)]
  # JOIN with crop area table
  crop_r$cell_area = cellSize(crop_r, mask=FALSE, lyrs=FALSE, unit="ha")
  crop_area_dt     = as.data.table(terra::as.data.frame(crop_r, xy = TRUE, cells = TRUE))
  crop_area_dt     = crop_area_dt[maize_rainfed_2015 > 0     |
                                    maize_irrigated_2015 > 0   |
                                    soybean_rainfed_2015 > 0   |
                                    soybean_irrigated_2015 > 0 |
                                    wheat_rainfed_2015 > 0     |
                                    wheat_irrigated_2015 > 0,]
  crop_area_dt[is.na(wheat_rainfed_2015),  wheat_rainfed_2015    := 0]
  crop_area_dt[is.na(wheat_irrigated_2015), wheat_irrigated_2015 := 0]
  # round all (1 digit)
  crop_area_dt = crop_area_dt[, lapply(.SD, round, digits = 1), .SDcols = c('maize_rainfed_2015',
                                                                            'maize_irrigated_2015',
                                                                            'soybean_rainfed_2015',
                                                                            'soybean_irrigated_2015',
                                                                            'wheat_rainfed_2015',
                                                                            'wheat_irrigated_2015'),
                              by = .(cell)]
  # sum crop area
  crop_area_dt[, sum_crop_2015 := maize_rainfed_2015 + maize_irrigated_2015 +
                 soybean_rainfed_2015 + soybean_irrigated_2015 +
                 wheat_rainfed_2015 + wheat_irrigated_2015]
  # remove < 1
  crop_area_dt = crop_area_dt[sum_crop_2015 >= 1,]
  
  # join dt
  crop_area_dt = crop_area_dt[country_r.dt[, .(cell, WB_NAME, WB_REGION)], on = .(cell = cell)]
  gc()
  setorder(crop_area_dt, cell)
  setnames(crop_area_dt, 'cell', 'gridid')
  crop_area_dt = crop_area_dt[!is.na(gridid),]
  gc()
  # filter again
  crop_area_dt     = crop_area_dt[maize_rainfed_2015 > 0     |
                                    maize_irrigated_2015 > 0   |
                                    soybean_rainfed_2015 > 0   |
                                    soybean_irrigated_2015 > 0 |
                                    wheat_rainfed_2015 > 0     |
                                    wheat_irrigated_2015 > 0,]
  
  # IPCC Region Names (AR6 & Roe et al. 2021)
  # Africa and Middle East
  AME   = c('Congo, Democratic Republic of', 'Nigeria', 'Tanzania', 'South Africa', 'Congo, Rep. of', 'Zambia',
            'Angola', 'Cameroon', 'Ethiopia', 'Mozambique', 'Iran, Islamic Republic of', 'Uganda',
            'Central African Republic', 'Gabon', 'Sudan', "Côte d'Ivoire", 'Kenya', 'Egypt, Arab Republic of',
            'Ghana', 'Zimbabwe', 'Mali', 'Namibia', 'South Sudan', 'Chad', 'Morocco', 'Botswana', 'Burkina Faso',
            'Niger', 'Guinea', 'Algeria', 'Liberia', 'Malawi', 'Senegal', 'Somalia', 'Saudi Arabia', 'Benin', 
            'Sierra Leone', 'Iraq', 'Rwanda', 'Eritrea', 'eSwatini', 'Benin', 'Burundi', 'Djibouti', 'Equatorial Guinea',
            'Madagascar', 'Mauritania', 'Tunisia', 'Syrian Arab Republic', 'Lebanon', 'Jordan', 'Libya', 'Israel', 
            'West Bank and Gaza', 'Kuwait', 'Oman', 'Qatar', 'United Arab Emirates', 'Yemen, Republic of', 'Cabo Verde',
            'Guinea-Bissau', 'Togo', 'Comoros', 'Mauritius', 'Lesotho')
  ADP   = c('China', 'Indonesia', 'India', 'Myanmar', 'Vietnam', 'Malaysia', 'Thailand', 'Pakistan', 'Papua New Guinea',
            'Philippines', 'Bangladesh', 'Cambodia', "Lao People's Democratic Republic", 'Mongolia', 'Korea, Republic of',
            'Afghanistan', 'Nepa', 'Sri Lanka', "Korea, Democratic People's Republic of", 'Solomon Islands', 'Bhutan',
            'Timor-Leste', 'Fiji', 'Nepal', 'Hong Kong (SAR, China)', 'Brunei Darussalam', 'Samoa', 'Vanuatu', 'Tonga')
  DEV   = c('United States of America', 'Canada', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark',
            'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg',
            'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia','Spain', 'Sweden', 'United Kingdom', 'Australia', 'Ukraine',
            'Japan', 'Turkey', 'New Zealand', 'Norway', 'Iceland', 'Greenland (Den.)', 'Faroe Islands (Den.)', 'Switzerland', 'Saint-Pierre-et-Miquelon (Fr.)',
            'Cyprus', 'Puerto Rico (US)', 'American Samoa (US)', 'Saint Helena, Ascension and Tristan da Cunha (UK)', 'New Caledonia (Fr.)',
            'French Southern and Antarctic Lands (Fr.)', 'Falkland Islands (UK)/Islas Malvinas', 'South Georgia and South Sandwich Islands (UK)')
  EEWCA = c('Russian Federation', 'Kazakhstan', 'Belarus', 'Uzbekistan', 'Turkmenistan', 'Kyrgyz Republic', 'Azerbaijan',
            'Moldova', 'Tajikistan', 'Armenia', 'Serbia', 'Bosnia and Herzegovina', 'Georgia', 'Montenegro', 'Kosovo', 'Albania',
            'North Macedonia')
  LAC   = c('Brazil', 'Colombia', 'Mexico', 'Argentina', 'Bolivia', 'Peru', 'Venezuela', 'Paraguay', 'Ecuador', 'Chile', 'Guyana', 'Suriname',
            'Cuba', 'Uruguay', 'Honduras', 'Nicaragua', 'Guatemala', 'Guyana', 'Costa Rica', 'Panama', 'Dominican Republic', 'El Salvador', 'Belize',
            'Bahamas, The', 'Haiti', 'Turks and Caicos Islands (UK)', 'Jamaica', 'Venezuela, Republica Bolivariana de', 'Trinidad and Tobago')
  crop_area_dt[WB_NAME %in% AME, IPCC_NAME := 'AME']
  crop_area_dt[WB_NAME %in% ADP, IPCC_NAME := 'ADP']
  crop_area_dt[WB_NAME %in% DEV, IPCC_NAME := 'DEV']
  crop_area_dt[WB_NAME %in% EEWCA, IPCC_NAME := 'EEWCA']
  crop_area_dt[WB_NAME %in% LAC, IPCC_NAME := 'LAC']
  return(crop_area_dt)
}
#-----------------------------------------------------------------------------------------
# analysis-recommended-practice-2050 
#-----------------------------------------------------------------------------------------
# create_crop_dt
#-----------------------------------------------------------------------------------------
# analysis-recommended-practice-2100
#-----------------------------------------------------------------------------------------
# create_crop_dt
#-----------------------------------------------------------------------------------------
# analysis-ML
#-----------------------------------------------------------------------------------------
pred_fun                 = function(object, newdata) {
  predict(object, data = newdata)$predictions[,2]
}
pred_fun_class = function(model, newdata, class_index) {
  predict(model, newdata)$predictions[, class_index]
}
rf_shap  = function(dt) {
  # reduce dt columns to numeric
  dt_r   = dt[, -c('scenario', 'y_block', 'gridid')]
  # remove all variables contributing to target response
  dt_r = dt_r[, -c('d_s_SOC', 'd_s_N2O', 'd_s_GHG', 'd_s_cgrain')]
  # return correlated variables
  correlationMatrix = cor(dt_r[,c(3:39,41)]) # all except response
  highlyCorrelated  = findCorrelation(correlationMatrix, cutoff=0.75, names = TRUE)
  print(highlyCorrelated)

  # remove all 
  dt_r = dt_r[, -c('SLCLAY','SLWLTP', 'SLCLIM', 'SLFLDC','SLSAND', 'SLSATC',
                   'frac_Urea', 'MINERL_sum_','bio5', 'bio10', 'bio1','bio16', 
                   'bio13', 'bio7', 'bio6', 'bio17')]
  # RF and SHAP
  print('Running ranger.')
  t_ranger       = ranger(dependent.variable.name = 'target', data = dt_r,
                          keep.inbag = TRUE, seed = 1234, 
                          classification = TRUE, 
                          probability = TRUE,
                          num.trees = 500,      
                          importance = 'impurity')
  print(t_ranger) # OOB prediction error = fraction of misclassified samples

  r_features = dt_r[, -c('target')]
  # OVERALL SHAP COMPUTATION 
  print('Computing SHAP')
  print(paste0('Started overall SHAP computation at ', Sys.time(), '.'))
  t_SHAP       = explain(t_ranger, 
                         X = r_features, 
                         pred_wrapper = pred_fun,
                         nsim = 50, # MC, try highest number possible while balancing computation time
                         adjust = TRUE,
                         shap_only = FALSE)
  print(paste0('Ended overall SHAP computation at ', Sys.time(), '.'))

  # SHAP BY CLASS
  classes       = levels(dt_r$target)
  shap_by_class = list()
  
  for(i in seq_along(classes)) {
    # function
    pred_wrapper = function(model, newdata) {
      pred_fun_class(model, newdata, i)
    }
    print(paste0('Started SHAP computation at ', Sys.time(), 'for class ', classes[i], '.'))
    
    shap_by_class[[classes[i]]] = explain(
      t_ranger,
      X            = r_features,
      pred_wrapper = pred_wrapper,
      nsim         = 50,
      adjust       = TRUE,
      shap_only    = FALSE)
    print(paste0('Ended SHAP computation at ', Sys.time(), 'for class ', classes[i], '.'))
    
  }
  
  # Visualize for each class
  # shapviz(shap_by_class[['l-l']])

  # save ranger, overall SHAP, class SHAP
  SHAP_list = list(ranger       = t_ranger,
                   overall_SHAP = t_SHAP,
                   class_SHAP   = shap_by_class,
                   input_dt     = dt,
                   ranger_dt    = dt_r)
  return(SHAP_list)
}
