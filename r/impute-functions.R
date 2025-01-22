# file name:    impute-functions.R
# created:      18 November 2024
# last updated: 18 December 2024
#-----------------------------------------------------------------------------------------
# DEPRECATED-impute-simulations.R
#-----------------------------------------------------------------------------------------
# IMPUTE missing gridcell based on a moving window approach
# function input:
# dt        - simulated data
# raster_ci - raster layer of cropland area
# response  - cumulative grain yield (s_cgrain)
# crop_irr  - name of layer from raster: maize_rainfed, maize_irri, soyb_rainfed,
#                                       soyb_irri, wht_rainfed, wht_irri
impute_gcm_response = function(dt, raster_ci, response, crop_irr) {
  require(data.table)
  require(terra)
  # empty dt 
  if (response %in% 's_cgrain') {
    imp_gcm_dt = data.table(cell = numeric(), x = numeric(), y = numeric(),
                            ssp = character(), gcm = character(), time = numeric(), 
                            type = numeric(), s_cgrain = numeric())
  }
  gcms = unique(dt[,gcm])
  times = c(2030, 2050, 2100)
  for (.gcm in gcms) {
    for (.time in times) {
      print(paste0('Running imputation for gcm ', .gcm, '.'))
      print(paste0('Running for year ', .time, '.'))
      dt_r = dt[time == .time & gcm %in% .gcm,]
      
      # TO DATAFRAME
      dt_r = as.data.frame(dt_r, xy = TRUE)
      setnames(dt_r, 'gridid', 'cell')
      setDT(dt_r)
      
      # CREATE raster
      dt_r   = as.data.frame(dt_r, xy = TRUE)
      r      = rast(nrow = 360, ncol = 720, nlyr = 12, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
      crs(r) = "epsg:4326"
      # original projection
      r_response = rast(res = 0.5, nlyr = 1, extent = ext(r), crs = crs(r)) 
      
      # ADD data to raster
      if(response %in% 's_cgrain') {
        r_response[[1]][dt_r$cell] = dt_r$s_cgrain
        # update names
        lyrs = ('s_cgrain_')
        lyrs     = outer(lyrs, .time, paste0)
        names(r_response) = lyrs
      }
      gc()
      
      # iterative moving window
      w = c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31)
      # w = c(5,9,13,17,21,25,29,33)
      # w = c(7,13,19,25,31)
      imp_dt  = c()
      imp_cells = c()
      for (i in w) {
        # IMPUTE with focal | need large window to capture ranges of table 
        r_all_imp = focal(r_response, w=i, fun = "mean", na.policy = "only", na.rm = TRUE)
        
        # TRANSFORM back to dt
        imp_w_dt    = setDT(terra::as.data.frame(r_all_imp, xy = TRUE, 
                                                 cells = TRUE))
        imp_w_dt    = imp_w_dt[!cell %in% imp_cells] # remove old cells
        # print(paste0('The difference in cells for window ', i, ' is ', length(unique(imp_w_dt[, cell]))))
        imp_dt  = rbind(imp_dt, imp_w_dt)      # combine tables
        setorder(imp_dt, cell)
        imp_cells = unique(imp_dt[, cell])     # update cells
        # print(paste0('New length of cells in iterative focal step is ',length(imp_cells), ' for window ', i, '.'))
        gc()
      }
      
      # WIDE TO LONG format
      imp_dt = melt(imp_dt,
                    id.vars = c("cell", "x", "y"),
                    measure.vars = patterns('_'))
      # ADJUST columns
      if (response %in% 's_cgrain') {
        imp_dt[, ssp  := 'ssp370']
        imp_dt[, gcm  := .gcm]
        imp_dt[, time := .time] 
        imp_dt[, type := 'imputed']
        setnames(imp_dt, 'value', 's_cgrain')
        imp_dt[, variable := NULL]
        
        # order
        setcolorder(imp_dt, c('cell','x', 'y', 'time', 'ssp', 'gcm', 'type', 's_cgrain'))
      }

      # TRANSFORM to dt
      crop_dt  = setDT(terra::as.data.frame(raster_ci, xy = TRUE, 
                                            cells = TRUE, na.rm =TRUE))
      
      # KEEP cell not in simulations
      crop_dt = crop_dt[!cell %in% dt_r$cell]
      
      # JOIN with crop
      imp_dt = imp_dt[crop_dt, on = .(cell = cell,
                                      x    = x,
                                      y    = y)]
      imp_dt = imp_dt[!is.na(time)]
      
      # FILTER by crop area
      if (crop_irr %in% 'maize_rainfed') {
        imp_dt = imp_dt[maize_rainfed_2015 > 0,]
        # remove area
        imp_dt[, maize_rainfed_2015 := NULL]
      } else if (crop_irr %in% 'maize_irri') {
        imp_dt = imp_dt[maize_irrigated_2015 > 0,]
        # remove area
        imp_dt[, maize_irrigated_2015 := NULL]
      } else if (crop_irr %in% 'soyb_rainfed') {
        imp_dt = imp_dt[soybean_rainfed_2015 > 0,]
        # remove area
        imp_dt[, soybean_rainfed_2015 := NULL]
      } else if (crop_irr %in% 'soyb_irri') {
        imp_dt = imp_dt[soybean_irrigated_2015 > 0,]
        # remove area
        imp_dt[, soybean_irrigated_2015 := NULL]
      } else if (crop_irr %in% 'wht_rainfed') {
        imp_dt = imp_dt[wheat_rainfed_2015 > 0,]
        # remove area
        imp_dt[, wheat_rainfed_2015 := NULL]
      } else if (crop_irr %in% 'wht_irri') {
        imp_dt = imp_dt[wheat_irrigated_2015 > 0,]
        # remove area
        imp_dt[, wheat_irrigated_2015 := NULL]
      } else {
        print('This is not a valid crop_irr entry. Stopping.')
        stop()
      }
      
      # COMBINE
      setcolorder(imp_dt, c('cell', 'x', 'y', 'ssp','gcm','time', 'type'))
      imp_gcm_dt = rbind(imp_gcm_dt, imp_dt)
      gc()
    }
  }
  # RENAME
  setnames(imp_gcm_dt, 'cell', 'gridid')
  # ALIGN with simulated dt
  imp_gcm_dt[, crop     := unique(dt[,crop])]
  imp_gcm_dt[, scenario :=unique(dt[,scenario])]
  imp_gcm_dt[, irr      := unique(dt[,irr])]
  # imp_gcm_dt[, run_yrs  := 85]
  
  # ORDER 
  setorder(imp_gcm_dt, 'gridid', 'gcm', 'time')
  setcolorder(imp_gcm_dt, c('gridid','x','y','crop','scenario', 
                            'irr','ssp','gcm','time')) # ,'run_yrs'
  
  return(imp_gcm_dt) 
}
#-----------------------------------------------------------------------------------------
# impute-missing-uncertainty
#-----------------------------------------------------------------------------------------
  ## LOAD RDATA WITH NEW OBJECT NAME ##
loadRData = function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
  ## ADD IPCC NAME TO UNCERTAINTY INPUT TABLE
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
  ## ADD IPCC NAME TO IMPUTE TABLE
add_region_missing = function(country.sf, crop_r) {
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
  ## IMPUTE LOCATIONS ##
impute_location = function(dt_known, n_iterations = 500, s = 11162024) {
  # Use mean of known locations as base estimate
  base_mean = mean(dt_known[, d_s_GHG]) 
  # Use sd of known locations as base estimate
  base_sd = sd(dt_known[, d_s_GHG])
  # Generate imputed data with scaled uncertainty
  set.seed(s)
  imputation = rnorm(
    n_iterations, 
    mean = base_mean, 
    sd   = base_sd
  )
  return(imputation)
}
#-----------------------------------------------------------------------------------------
# impute-missing-gcm
#-----------------------------------------------------------------------------------------
## LOAD RDATA WITH NEW OBJECT NAME ##
loadRData = function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
## ADD IPCC NAME TO UNCERTAINTY INPUT TABLE
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
## ADD IPCC NAME TO IMPUTE TABLE
add_region_missing = function(country.sf, crop_r) {
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
## IMPUTE LOCATIONS ##
impute_location_gcm = function(dt_known, n_iterations = 24, s = 11162024) {
  # Use mean of known locations as base estimate
  base_mean = mean(dt_known[, d_s_cgrain]) 
  # Use sd of known locations as base estimate
  base_sd = sd(dt_known[, d_s_cgrain])
  # Generate imputed data with scaled uncertainty
  set.seed(s)
  imputation = rnorm(
    n_iterations, 
    mean = base_mean, 
    sd   = base_sd
  )
  return(imputation)
}
