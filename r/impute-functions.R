# file name:    impute-functions.R
# created:      18 November 2024
# last updated: 20 November 2024
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

