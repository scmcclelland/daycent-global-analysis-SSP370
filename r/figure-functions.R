#filename: figure-functions.R
# created:      18 November 2024
# last updated: 21 February 2025
#-----------------------------------------------------------------------------------------
# figures-main
#-----------------------------------------------------------------------------------------
  ## Figure 1. Scatterplot ##
scatterplot_fig = function(dt, constant_Pg, constant_Tg) {
  scenario_lbl = c('ccg-res' = c('Grass CC'),
                   'ccl-res' = c('Legume CC'), 
                   'ccg-ntill' = c('Grass CC + Ntill'),
                   'ccl-ntill' = c('Legume CC + Ntill'))
  dt$scenario  = factor(dt$scenario, levels = c('ccg-res', 'ccg-ntill', 'ccl-res', 'ccl-ntill'))
  ipcc_lbl     = c('GLB'   = c('GLOBE'),
                   'ADP'   = c('ADP'),
                   'AME'   = c('AME'),
                   'DEV'   = c('DEV'),
                   'EEWCA' = c('EEWCA'),
                   'LAC'   = c('LAC'))
  dt$IPCC_NAME = factor(dt$IPCC_NAME, levels = c('GLB', 'ADP', 'AME', 'DEV', 'EEWCA', 'LAC'))

  glb_gg = ggplot(dt[IPCC_NAME == 'GLB'], aes(x = (s_GHG/constant_Pg), 
                                              y = (s_grain/constant_Pg), 
                                              color = IPCC_NAME, shape = scenario)) +
    # Add horizontal and vertical error bars
    geom_errorbar(aes(ymin = (s_grain/constant_Pg) - (se_s_grain/constant_Pg),
                      ymax = (s_grain/constant_Pg) + (se_s_grain/constant_Pg)),
                  width = 0,
                  size  = 0.5,
                  alpha = 0.8) +
    geom_errorbarh(aes(xmin = (s_GHG/constant_Pg) - (se_s_GHG/constant_Pg),
                       xmax = (s_GHG/constant_Pg) + (se_s_GHG/constant_Pg)),
                   height = 0,
                   size   = 0.5,
                   alpha  = 0.8) +
    # Add points
    geom_point(size = 1.7) + # fill = 'white'
    scale_shape_manual(name = 'Scenario', labels = scenario_lbl, 
                       values = c(15,19,17,3)) +
    scale_color_manual(name = 'Region', labels = ipcc_lbl, 
                       values = c("#913640","#040404","#4B4C40",
                                           "#CC79A7", "#928261","#EAAD89")) +
    # Add reference lines at x=0 and y=0 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Set coordinate limits
    xlim(-0.25, 1.0) +
    ylim(-0.25, 0.3) +
    xlab((expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Pg~CO[2]*-eq*~yr^-1*')')))) +
    ylab((expression(atop(paste(Annual~Yield~Difference), '('*Pg~yr^-1*')')))) +
    # Customize theme and labels
    theme_bw() +
    theme(text = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.x = element_text(size = 7),
          legend.position  = 'right',
          legend.justification = c(0, 0)) +
    guides(color = guide_legend(override.aes = list(linetype = NULL)))
  glb_gg
  
  reg_gg = ggplot(dt[!IPCC_NAME == 'GLB'], aes(x = (s_GHG/constant_Tg), 
                                               y = (s_grain/constant_Tg), 
                                               color = IPCC_NAME, shape = scenario)) +
    # Add horizontal and vertical error bars
    geom_errorbar(aes(ymin = (s_grain/constant_Tg) - (se_s_grain/constant_Tg),
                      ymax = (s_grain/constant_Tg) + (se_s_grain/constant_Tg)),
                  width = 0,
                  size  = 0.5,
                  alpha = 0.8) +
    geom_errorbarh(aes(xmin = (s_GHG/constant_Tg) - (se_s_GHG/constant_Tg),
                       xmax = (s_GHG/constant_Tg) + (se_s_GHG/constant_Tg)),
                   height = 0,
                   size   = 0.5,
                   alpha  = 0.8) +
    # Add points
    geom_point(size = 1.7) + # fill = 'white'
    scale_shape_manual(name = 'Scenario', labels = scenario_lbl, 
                       values = c(15,19,17,3)) +
    scale_color_manual(name = 'Region', labels = ipcc_lbl, 
                       values = c("#040404","#4B4C40","#CC79A7",
                                           "#928261","#EAAD89")) +
    # Add reference lines at x=0 and y=0 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Set coordinate limits
    xlim(-200, 500) +
    ylim(-200, 200) +
    xlab((expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Tg~CO[2]*-eq*~yr^-1*')')))) +
    ylab((expression(atop(paste(Annual~Yield~Difference), '('*Tg~yr^-1*')')))) +
    # Customize theme and labels
    theme_bw() +
    theme(text = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.x = element_text(size = 7),
          legend.position  = 'right',
          legend.justification = c(0, 0)) +
    guides(color = guide_legend(override.aes = list(linetype = NULL)))
  reg_gg
  
  return(list(glb = glb_gg, reg = reg_gg))
}
  ## Figure 2. Maps ##
ghg_map_fig     = function(dt) {
  # set xy
  dt_r = as.data.frame(dt, xy = TRUE)
  setDT(dt_r)
  
  # create manual breaks
  quants.GHG  = dt_r[, quantile(d_s_GHG,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  quants.N2O  = dt_r[, quantile(d_s_N2O,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  quants.SOC  = dt_r[, quantile(d_s_SOC,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  
  # make manual quantiles
  ghg.quants = c(-Inf,-3,-2,-1,-0.5,0,0.5,1,2,3,Inf)
  dt_r[, quant.cuts.GHG := cut(d_s_GHG, breaks = ghg.quants)]
  n2o.quants = c(-Inf,-3,-2,-1,-0.5,0,0.5,1,2,3,Inf)
  dt_r[, quant.cuts.N2O := cut(d_s_N2O, breaks = n2o.quants)]
  soc.quants = c(-Inf,-3,-2,-1,-0.5,0,0.5,1,2,3,Inf)
  dt_r[, quant.cuts.SOC := cut(d_s_SOC, breaks = soc.quants)]
  
  # create raster
  dt_r = as.data.frame(dt_r, xy = TRUE)
  r                = rast(nrow = 360, ncol = 720, nlyr = 3, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  crs(r)           = "epsg:4326"
  
  # original projection
  r_lat_ghg = rast(res = 0.5, nlyr = 3, extent = ext(r), crs = crs(r)) 
  r_lat_ghg[[1]][dt_r$gridid] = dt_r$quant.cuts.GHG
  r_lat_ghg[[2]][dt_r$gridid] = dt_r$quant.cuts.N2O
  r_lat_ghg[[3]][dt_r$gridid] = dt_r$quant.cuts.SOC
  names(r_lat_ghg) = c('d_s_GHG', 'd_s_N2O','d_s_SOC')
  
  # equal area projection
  newcrs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  # get new output dimensions
  project(r_lat_ghg, newcrs)
  # dimensions  : 569, 1138, 3  (nrow, ncol, nlyr)
  # resolution  : 29727.52, 29727.52  (x, y)
  # extent      : -16921203, 16908719, -8454359, 8460601  (xmin, xmax, ymin, ymax)
  r_newcrs   = rast(ncols = 1138, nrows = 569, nlyr = 3, xmin = -16921203, xmax = 16908719, ymin = -8454359, ymax = 8460601, crs = newcrs)
  r_eckiv    = project(r_lat_ghg, r_newcrs)
  
  r_eckiv_dt = as.data.frame(r_eckiv, cells = TRUE, xy = TRUE)
  r_eckiv_dt = setDT(r_eckiv_dt)
  r_eckiv_dt_GHG = r_eckiv_dt[,.(cell, x, y, d_s_GHG, d_s_N2O, d_s_SOC)]
  r_eckiv_dt_GHG[, d_s_GHG := round(d_s_GHG, digits = 0)]
  r_eckiv_dt_GHG[, d_s_GHG := as.character(d_s_GHG)]
  r_eckiv_dt_GHG[, d_s_SOC := round(d_s_SOC, digits = 0)]
  r_eckiv_dt_GHG[, d_s_SOC := as.character(d_s_SOC)]
  r_eckiv_dt_GHG[, d_s_N2O := round(d_s_N2O, digits = 0)]
  r_eckiv_dt_GHG[, d_s_N2O := as.character(d_s_N2O)]
  
  # create sf object
  data(wrld_simpl)
  wrld_simpl_sf = sf::st_as_sf(wrld_simpl)
  wrld_simpl_sf_eckiv = st_transform(wrld_simpl_sf, crs = newcrs)
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[wrld_simpl_sf_eckiv$NAME != 'Antarctica',]
  
  small_islands       = wrld_simpl_sf[wrld_simpl_sf$AREA < 10000,]
  remove              = c('Antigua and Barbuda', 'American Samoa', 'Barbados', 'Bermuda',
                          'Bahamas', 'Solomon Islands', 'Cayman Islands', 'Comoros','Cook Islands', 'Cape Verde',
                          'Dominica', 'Fiji','Falkland Islands (Malvinas)', 'Micronesia, Federated States of', 'Grenada',
                          'New Caledonia', 'Niue', 'Anguilla','French Polynesia', 'Guam', 'Kiribati', 'Martinique','Maldives', 'Aruba', 'Northern Mariana Islands',
                          'Faroe Islands', 'Mayotte', 'Mauritius','Aaland Islands', 'Norfolk Island', 'Cocos (Keeling) Islands',
                          'Bouvet Island', 'French Southern and Antarctic Lands', 'Heard Island and McDonald Islands',
                          'British Indian Ocean Territory', 'Christmas Island', 'Vanuatu','United States Minor Outlying Islands',
                          'Nauru', 'Reunion', 'Saint Kitts and Nevis', 'Seychelles', 'Saint Lucia', 'Tokelau', 'Tonga',
                          'Tuvalu','Saint Vincent and the Grenadines', 'British Virgin Islands', 'United States Virgin Islands',
                          'Wallis and Futuna Islands', 'Samoa', 'Guadeloupe', 'Netherlands Antilles', 'Pitcairn Islands','
                          Palau', 'Marshall Islands', 'Saint Pierre and Miquelon', 'Saint Helena', 'San Marino',
                          'Turks and Caicos Islands', 'Svalbard', 'Saint Martin', 'Saint Barthelemy', 'South Georgia South Sandwich Islands',
                          'Guernsey', 'Jersey')
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[!wrld_simpl_sf_eckiv$NAME %in% remove,]

  colors2 = c('1' = "#8E0152", '2' = "#C51B7D",'3' = "#DE77AE", '4'= "#F1B6DA", 
              '5' = "#FDE0EF", '6' = "#E6F5D0",'7' = "#B8E186", '8' = "#7FBC41", '9' = "#4D9221", '10'= "#276419")
  # BBOX
  # xmin      ymin      xmax      ymax 
  # -15861702  -6637514  15337601   8373855 
  
  # create maps
  gg_GHG = ggplot() + 
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_GHG[,.(cell, x, y, d_s_GHG)],
              aes(x = x, y = y, fill = d_s_GHG)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,0,0), "null"),
          axis.ticks = element_blank())
  gg_GHG
  
  gg_N2O = ggplot() + 
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_GHG[,.(cell, x, y, d_s_N2O)],
              aes(x = x, y = y, fill = d_s_N2O)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,-1,0), "cm"))
  gg_N2O
  
  gg_SOC = ggplot() + 
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_GHG[,.(cell, x, y, d_s_SOC)],
              aes(x = x, y = y, fill = d_s_SOC)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,-1,0), "cm"))
  gg_SOC
  
  colors_bar2 =c("#8E0152", "#C51B7D","#DE77AE", "#F1B6DA", 
                          "#FDE0EF", "#E6F5D0","#B8E186", "#7FBC41", "#4D9221", "#276419")
                          
  # create legend
  gg_legend1 = plot_discrete_cbar(c(-Inf,-3,-2,-1,-0.5,0,0.5,1,2,3,Inf),
                                  colors = colors_bar2,
                                  legend_title = expression(GHG~Difference~'('~Mg~CO[2]*-eq~ha^-1*~yr^-1*')'),
                                  spacing = 'constant',
                                  font_size = 4)
  gg_legend1

  gg_maps = list(N2O = gg_N2O, SOC = gg_SOC, GHG = gg_GHG, legend1 = gg_legend1)
  return(gg_maps)
}
yield_map_fig   = function(dt) {
  
  # set xy
  dt_r = as.data.frame(dt, xy = TRUE)
  setDT(dt_r)
  
  # modify dt
  Mg_to_kg = 1000L
  dt_r[, d_s_cgrain := d_s_cgrain*Mg_to_kg]
  
  # create manual breaks
  gr.quants  = dt_r[, quantile(d_s_cgrain,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  
  # make manual quantiles
  gr.quants = c(-Inf,-750,-500,-250,-100,0,100,250,500,750,Inf)
  dt_r[, quant.cuts.gr := cut(d_s_cgrain, breaks = gr.quants)]
  
  # create raster
  dt_r = as.data.frame(dt_r, xy = TRUE)
  r                = rast(nrow = 360, ncol = 720, nlyr = 1, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  crs(r)           = "epsg:4326"
  
  # original projection
  r_lat_yield = rast(res = 0.5, nlyr = 1, extent = ext(r), crs = crs(r)) 
  r_lat_yield[[1]][dt_r$gridid] = dt_r$quant.cuts.gr
  names(r_lat_yield) = c('d_s_cgrain')
  # equal area projection
  newcrs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # get new output dimensions
  project(r_lat_yield, newcrs)
  # dimensions  : 569, 1138, 1  (nrow, ncol, nlyr)
  # resolution  : 29727.52, 29727.52  (x, y)
  # extent      : -16921203, 16908719, -8454359, 8460601  (xmin, xmax, ymin, ymax)
  r_newcrs   = rast(ncols = 1138, nrows = 569, nlyr = 1, xmin = -16921203, xmax = 16908719, ymin = -8454359, ymax = 8460601, crs = newcrs)
  r_eckiv    = project(r_lat_yield, r_newcrs)
  
  r_eckiv_dt = as.data.frame(r_eckiv, cells = TRUE, xy = TRUE)
  r_eckiv_dt = setDT(r_eckiv_dt)
  r_eckiv_dt_yield = r_eckiv_dt[,.(cell, x, y, d_s_cgrain)]
  r_eckiv_dt_yield[, d_s_cgrain := round(d_s_cgrain, digits = 0)]
  r_eckiv_dt_yield[, d_s_cgrain := as.character(d_s_cgrain)]
  
  # create sf object
  data(wrld_simpl)
  wrld_simpl_sf = sf::st_as_sf(wrld_simpl)
  wrld_simpl_sf_eckiv = st_transform(wrld_simpl_sf, crs = newcrs)
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[wrld_simpl_sf_eckiv$NAME != 'Antarctica',]
  
  small_islands       = wrld_simpl_sf[wrld_simpl_sf$AREA < 10000,]
  remove              = c('Antigua and Barbuda', 'American Samoa', 'Barbados', 'Bermuda',
                          'Bahamas', 'Solomon Islands', 'Cayman Islands', 'Comoros','Cook Islands', 'Cape Verde',
                          'Dominica', 'Fiji','Falkland Islands (Malvinas)', 'Micronesia, Federated States of', 'Grenada',
                          'New Caledonia', 'Niue', 'Anguilla','French Polynesia', 'Guam', 'Kiribati', 'Martinique','Maldives', 'Aruba', 'Northern Mariana Islands',
                          'Faroe Islands', 'Mayotte', 'Mauritius','Aaland Islands', 'Norfolk Island', 'Cocos (Keeling) Islands',
                          'Bouvet Island', 'French Southern and Antarctic Lands', 'Heard Island and McDonald Islands',
                          'British Indian Ocean Territory', 'Christmas Island', 'Vanuatu','United States Minor Outlying Islands',
                          'Nauru', 'Reunion', 'Saint Kitts and Nevis', 'Seychelles', 'Saint Lucia', 'Tokelau', 'Tonga',
                          'Tuvalu','Saint Vincent and the Grenadines', 'British Virgin Islands', 'United States Virgin Islands',
                          'Wallis and Futuna Islands', 'Samoa', 'Guadeloupe', 'Netherlands Antilles', 'Pitcairn Islands','
                          Palau', 'Marshall Islands', 'Saint Pierre and Miquelon', 'Saint Helena', 'San Marino',
                          'Turks and Caicos Islands', 'Svalbard', 'Saint Martin', 'Saint Barthelemy', 'South Georgia South Sandwich Islands',
                          'Guernsey', 'Jersey')
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[!wrld_simpl_sf_eckiv$NAME %in% remove,]
  
  colors2 = c('1' = "#8E0152", '2' = "#C51B7D",'3' = "#DE77AE", '4'= "#F1B6DA", 
              '5' = "#FDE0EF", '6' = "#E6F5D0",'7' = "#B8E186", '8' = "#7FBC41", '9' = "#4D9221", '10'= "#276419")
  # BBOX
  # xmin      ymin      xmax      ymax 
  # -15861702  -6637514  15337601   8373855 
  
  gg_gr = ggplot() +
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_yield[,.(cell, x, y, d_s_cgrain)],
              aes(x = x, y = y, fill = d_s_cgrain)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,-2,0), "cm"))
  gg_gr
 
  colors_bar2 =c("#8E0152", "#C51B7D","#DE77AE", "#F1B6DA", 
                          "#FDE0EF", "#E6F5D0","#B8E186", "#7FBC41", "#4D9221", "#276419")
                          
  # create legend
  gg_legend2 = plot_discrete_cbar(c(-Inf,-750,-500,-250,-100,0,100,250,500,750,Inf),
                                  colors = colors_bar2,
                                  legend_title = expression(Yield~Difference~'('~kg~ha^-1*~yr^-1*')'),
                                  spacing = 'constant',
                                  font_size = 4)
  gg_legend2
  
  gg_maps = list(grain = gg_gr, legend2 = gg_legend2)
  return(gg_maps)
}
  ## Figure 3. Feature importance ##
feature_p      = function(feature_v, gg_features, gg_values, colors, groups) {
  dt  = data.table(feature = gg_features, value = gg_values,
                          type = groups)
  feature_gg = ggplot(dt, aes(x = value, y = feature, fill = type)) +
    geom_bar(stat = 'identity') +
    scale_y_discrete(labels = feature_v) +
    scale_fill_manual('Feature', 
                      values = colors, 
                      limits = names(colors), 
                      drop = FALSE) +
    xlab("Mean |SHAP Value|") +
    theme_bw() +
    theme(legend.position ='bottom',
          text            = element_text(color = 'black', size = 7),
          axis.title.y    = element_blank(),
          # axis.title.x    = element_blank(),
          axis.text       = element_text(size = 7, color = 'black'),
          strip.text      = element_text(size = 7, color = 'black'))
  return(feature_gg)
}
  ## Figure 4. Partial dependence plots ##
cont_pdp = function(shv, sv_imp, color_hex, ft_name, shv_name, lbl) {
  # for continuous
  # make df
  df = data.frame(
    shap_value    = shv$S[, shv_name],  # SHAP values of feature
    feature_value = shv$X[, shv_name]  # Original feature values
  )
  
  # for categorical
  cont_gg = ggplot(df, aes(x = feature_value, y = shap_value)) +
  geom_point(alpha = 0.7, color = color_hex, size = 1) +
  # Modify with loess  fit (calculation takes some time when returning plot)
  geom_smooth(
    method = "loess",  # or method = "gam" for larger datasets
    color = "grey35",
    se = FALSE
  ) +
  xlab(ft_name) +
  ylab('SHAP value') +
  theme_bw() +
  theme(text = element_text(color = 'black', size = 6),
          axis.title.x = element_text(size = 6),
          axis.text = element_text(size = 6, color = 'black'),
          strip.text = element_text(size = 6, color = 'black'))
  cont_gg = cont_gg + theme(
    plot.title.position = "plot",  # This moves the title to align with plot edge
    plot.title = element_text(
      hjust = -0.01,  # Slight adjustment left of the plot
      vjust = -0.5,   # Slight adjustment above the plot
      size = 7       # Match your other text size if needed
    )
  ) +
    ggtitle('', lbl)
return(cont_gg)
}
cat_pdp  = function(shv, sv_imp, color_hex, ft_name, shv_name, lbl) {
  # make df
  df = data.frame(
    shap_value    = shv$S[, shv_name],  # SHAP values of feature
    feature_value = shv$X[, shv_name]  # Original feature values
  )

  # for categorical
cat_gg = ggplot(df, aes(x = feature_value, y = shap_value)) +
    geom_point(alpha = 0.7, color = color_hex, size = 1) +
    stat_summary(
      geom = "point",
      fun = mean,
      color = "black",
      size = 3,
      shape = 15
    )  +
  xlab(ft_name) +
  ylab('SHAP value') +
    theme_bw() +
    theme(text = element_text(color = 'black', size = 6),
          axis.title.x = element_text(size = 6),
          axis.text = element_text(size = 6, color = 'black'),
          strip.text = element_text(size = 6, color = 'black'))
cat_gg = cat_gg + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle('', lbl)

  return(cat_gg)
}
  ## Figure 5. Regional goals ##
bmp_map             = function(dt) {
  # set xy
  dt_r = as.data.frame(dt, xy = TRUE)
  setDT(dt_r)
  
  # update scenarios
  dt_r[scenario %in% 'cp',        scenario := NA]
  dt_r[scenario %in% 'ccg-res',   scenario := '2']
  dt_r[scenario %in% 'ccl-res',   scenario := '3']
  dt_r[scenario %in% 'ccg-ntill', scenario := '4']
  dt_r[scenario %in% 'ccl-ntill', scenario := '5']
  dt_r[, scenario := as.numeric(scenario)]
  
  dt_r = dt_r[!is.na(scenario)]
  
  # create raster
  dt_r = as.data.frame(dt_r, xy = TRUE)
  r                = rast(nrow = 360, ncol = 720, nlyr = 1, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  crs(r)           = "epsg:4326"
  # original projection
  r_lat_scen = rast(res = 0.5, nlyr = 1, extent = ext(r), crs = crs(r)) 
  r_lat_scen[[1]][dt_r$gridid] = dt_r$scenario
  
  names(r_lat_scen) = c('scenario')
  # equal area projection
  newcrs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # get new output dimensions
  project(r_lat_scen, newcrs)
  # dimensions  : 569, 1138, 1  (nrow, ncol, nlyr)
  # resolution  : 29727.52, 29727.52  (x, y)
  # extent      : -16921203, 16908719, -8454359, 8460601  (xmin, xmax, ymin, ymax)
  r_newcrs   = rast(ncols = 1138, nrows = 569, nlyr = 1, xmin = -16921203, xmax = 16908719, ymin = -8454359, ymax = 8460601, crs = newcrs)
  r_eckiv    = project(r_lat_scen, r_newcrs)
  
  r_eckiv_dt = as.data.frame(r_eckiv, cells = TRUE, xy = TRUE)
  r_eckiv_dt = setDT(r_eckiv_dt)
  r_eckiv_dt_GHG = r_eckiv_dt[,.(cell, x, y, scenario)]
  r_eckiv_dt_GHG[, scenario := round(scenario, digits = 0)]
  r_eckiv_dt_GHG[, scenario := as.character(scenario)]
  
  # create sf object
  data(wrld_simpl)
  wrld_simpl_sf = sf::st_as_sf(wrld_simpl)
  wrld_simpl_sf_eckiv = st_transform(wrld_simpl_sf, crs = newcrs)
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[wrld_simpl_sf_eckiv$NAME != 'Antarctica',]
  
  small_islands       = wrld_simpl_sf[wrld_simpl_sf$AREA < 10000,]
  remove              = c('Antigua and Barbuda', 'American Samoa', 'Barbados', 'Bermuda',
                          'Bahamas', 'Solomon Islands', 'Cayman Islands', 'Comoros','Cook Islands', 'Cape Verde',
                          'Dominica', 'Fiji','Falkland Islands (Malvinas)', 'Micronesia, Federated States of', 'Grenada',
                          'New Caledonia', 'Niue', 'Anguilla','French Polynesia', 'Guam', 'Kiribati', 'Martinique','Maldives', 'Aruba', 'Northern Mariana Islands',
                          'Faroe Islands', 'Mayotte', 'Mauritius','Aaland Islands', 'Norfolk Island', 'Cocos (Keeling) Islands',
                          'Bouvet Island', 'French Southern and Antarctic Lands', 'Heard Island and McDonald Islands',
                          'British Indian Ocean Territory', 'Christmas Island', 'Vanuatu','United States Minor Outlying Islands',
                          'Nauru', 'Reunion', 'Saint Kitts and Nevis', 'Seychelles', 'Saint Lucia', 'Tokelau', 'Tonga',
                          'Tuvalu','Saint Vincent and the Grenadines', 'British Virgin Islands', 'United States Virgin Islands',
                          'Wallis and Futuna Islands', 'Samoa', 'Guadeloupe', 'Netherlands Antilles', 'Pitcairn Islands','
                          Palau', 'Marshall Islands', 'Saint Pierre and Miquelon', 'Saint Helena', 'San Marino',
                          'Turks and Caicos Islands', 'Svalbard', 'Saint Martin', 'Saint Barthelemy', 'South Georgia South Sandwich Islands',
                          'Guernsey', 'Jersey')
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[!wrld_simpl_sf_eckiv$NAME %in% remove,]
  colors = c('1' = "grey75", '2' = "#A75529", 
             '3' = "#8B0069", '4' = "#9A9800", '5' = "#5DD291")
  
  # create maps
  gg_BMP = ggplot() + 
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_GHG[,.(cell, x, y, scenario)],
              aes(x = x, y = y, fill = scenario)) +
    scale_fill_manual(values = colors) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,-2,-1), "cm"),
          panel.grid.major = element_blank())
  gg_BMP
  
  gg_bmp = list(bmp = gg_BMP)
  return(gg_bmp)
}
IPCC_map          = function(.lu_path, .shp_f, .raster) {
  country.sf    = st_read(paste(.lu_path, .shp_f, sep = '/'))
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
  crop_area_r           = rast(paste(.lu_path, .raster, sep = '/'))
  crop_area_r$cell_area = cellSize(crop_area_r, mask=FALSE, lyrs=FALSE, unit="ha")
  crop_area_dt          = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE))
  
  crop_area_dt = crop_area_dt[country_r.dt[, .(cell, WB_NAME, WB_REGION)], on = .(cell = cell)]
  gc()
  setorder(crop_area_dt, cell)
  crop_area_dt = crop_area_dt[!is.na(x),]
  gc()
  
  # NA to 0
  crop_area_dt[is.na(maize_rainfed_2015), maize_rainfed_2015 := 0]
  crop_area_dt[is.na(maize_irrigated_2015), maize_irrigated_2015 := 0]
  crop_area_dt[is.na(soybean_rainfed_2015), soybean_rainfed_2015 := 0]
  crop_area_dt[is.na(soybean_irrigated_2015), soybean_irrigated_2015 := 0]
  crop_area_dt[is.na(wheat_rainfed_2015), wheat_rainfed_2015 := 0]
  crop_area_dt[is.na(wheat_irrigated_2015), wheat_irrigated_2015 := 0]
  
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
  crop_area_dt[WB_NAME %in% AME, IPCC_NAME   := 'AME']
  crop_area_dt[WB_NAME %in% ADP, IPCC_NAME   := 'ADP']
  crop_area_dt[WB_NAME %in% DEV, IPCC_NAME   := 'DEV']
  crop_area_dt[WB_NAME %in% EEWCA, IPCC_NAME := 'EEWCA']
  crop_area_dt[WB_NAME %in% LAC, IPCC_NAME   := 'LAC']
  
  crop_area_dt = unique(crop_area_dt[, c('WB_NAME', 'IPCC_NAME')])
  
  # WB_countries_Admin0_10m.shp
  country.sf    = st_read(paste(.lu_path, .shp_f, sep = '/'))
  country.sf    = st_transform(country.sf, crs = 4326)
  country.sf    = st_wrap_dateline(country.sf)
  country.sf    = st_transform(country.sf, crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  country.sf    = merge(country.sf, crop_area_dt[, c("WB_NAME", "IPCC_NAME")], 
                        by.x = 'WB_NAME', by.y = 'WB_NAME', all.x = TRUE, sort = FALSE)
  WB_colors     = c('ADP' = c("#040404"), 'AME' = c("#4B4C40"), 'DEV' = c("#CC79A7"),
                    'EEWCA' = c("#928261"), 'LAC' = c("#EAAD89"))

  map = ggplot() +
    geom_sf() +
    geom_sf(data = country.sf, aes(alpha = 0.9, fill = IPCC_NAME, colour = IPCC_NAME),
            colour = "grey95", size = 0.05) +
    theme_map() +
    scale_fill_manual(values = WB_colors, na.value = "grey10") +
    theme(legend.position='none',
          plot.margin = unit(c(-2,-2,-2,-2), "cm"))
  map
  
  gg_maps = list(IPCC = map)
  return(gg_maps)
}
#-----------------------------------------------------------------------------------------
# figures-extended-data
#-----------------------------------------------------------------------------------------
# ghg_map_fig
# yield_map_fig
bmp_scatterplot_fig = function(dt, constant) {
  goal_lbl     = c('max-ghg'      = c('Maximum GHG Potential'),
                   'max-yield'    = c('Maximum Yield Potential'),
                   'max-ghg-yc'   = c('Maximum GHG Potential (Yield Balance)'),
                   'max-yield-gc' = c('Maximum Yield Potential (GHG Balance)'))
  dt$goal      = factor(dt$goal, levels = c('max-ghg', 'max-yield', 
                                                'max-ghg-yc', 'max-yield-gc'))

  gg = ggplot(dt, aes(x = (s_GHG/constant), y = (s_grain/constant), color = goal)) +
    # Add points
    geom_point(size = 1.5) +
    # Add horizontal and vertical error bars
    geom_errorbar(aes(ymin = (s_grain/constant) - (se_s_grain/constant),
                      ymax = (s_grain/constant) + (se_s_grain/constant)), width = 0) +
    geom_errorbarh(aes(xmin = (s_GHG/constant) - (se_s_GHG/constant),
                       xmax = (s_GHG/constant) + (se_s_GHG/constant)), height = 0) +
    scale_color_manual(name = 'Goal', labels = goal_lbl, values = c("#005E5E","#578B21","#E89E6B","#CC79A7")) +
                                                                                 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Set coordinate limits
    xlim(-0.5, 1.25) +
    ylim(-0.25, 0.5) +
    xlab((expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Pg~CO[2]*-eq*~yr^-1*')')))) +
    ylab((expression(atop(paste(Annual~Yield~Difference), '('*Pg~yr^-1*')')))) +
    # Customize theme and labels
    theme_bw() +
    theme(text = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.x = element_text(size = 7),
          legend.position = "right",
          legend.box.just = "left",
          legend.justification = c(0, 0),
          legend.box.margin = margin(0, 0, 0, 0)) +
    guides(color = guide_legend(override.aes = list(linetype = NULL)))
  gg
  return(gg)
}
#-----------------------------------------------------------------------------------------
# figures-si
#-----------------------------------------------------------------------------------------
cov_ghg_map_fig     = function(dt) {
  # set xy
  dt_r = as.data.frame(dt, xy = TRUE)
  setDT(dt_r)
  
  # create manual breaks
  quants.GHG  = dt_r[, quantile(cov_s_GHG,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  
  # make manual quantiles
  ghg.quants = c(0,10,20,30,40,50,60,70,80,90,100,Inf)
  dt_r[, quant.cuts.GHG := cut(cov_s_GHG, breaks = ghg.quants)]
  
  # create raster
  dt_r = as.data.frame(dt_r, xy = TRUE)
  r                = rast(nrow = 360, ncol = 720, nlyr = 1, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  crs(r)           = "epsg:4326"
  
  # original projection
  r_lat_ghg = rast(res = 0.5, nlyr = 1, extent = ext(r), crs = crs(r)) 
  r_lat_ghg[[1]][dt_r$gridid] = dt_r$quant.cuts.GHG
  names(r_lat_ghg) = c('cov_s_GHG')
  
  # equal area projection
  newcrs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  # get new output dimensions
  project(r_lat_ghg, newcrs)
  # dimensions  : 569, 1138, 1  (nrow, ncol, nlyr)
  # resolution  : 29727.52, 29727.52  (x, y)
  # extent      : -16921203, 16908719, -8454359, 8460601  (xmin, xmax, ymin, ymax)
  r_newcrs   = rast(ncols = 1138, nrows = 569, nlyr = 1, xmin = -16921203, xmax = 16908719, ymin = -8454359, ymax = 8460601, crs = newcrs)
  r_eckiv    = project(r_lat_ghg, r_newcrs)
  
  r_eckiv_dt = as.data.frame(r_eckiv, cells = TRUE, xy = TRUE)
  r_eckiv_dt = setDT(r_eckiv_dt)
  r_eckiv_dt_GHG = r_eckiv_dt[,.(cell, x, y, cov_s_GHG)]
  r_eckiv_dt_GHG[, cov_s_GHG := round(cov_s_GHG, digits = 0)]
  r_eckiv_dt_GHG[, cov_s_GHG := as.character(cov_s_GHG)]
  
  # create sf object
  data(wrld_simpl)
  wrld_simpl_sf = sf::st_as_sf(wrld_simpl)
  wrld_simpl_sf_eckiv = st_transform(wrld_simpl_sf, crs = newcrs)
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[wrld_simpl_sf_eckiv$NAME != 'Antarctica',]
  
  small_islands       = wrld_simpl_sf[wrld_simpl_sf$AREA < 10000,]
  remove              = c('Antigua and Barbuda', 'American Samoa', 'Barbados', 'Bermuda',
                          'Bahamas', 'Solomon Islands', 'Cayman Islands', 'Comoros','Cook Islands', 'Cape Verde',
                          'Dominica', 'Fiji','Falkland Islands (Malvinas)', 'Micronesia, Federated States of', 'Grenada',
                          'New Caledonia', 'Niue', 'Anguilla','French Polynesia', 'Guam', 'Kiribati', 'Martinique','Maldives', 'Aruba', 'Northern Mariana Islands',
                          'Faroe Islands', 'Mayotte', 'Mauritius','Aaland Islands', 'Norfolk Island', 'Cocos (Keeling) Islands',
                          'Bouvet Island', 'French Southern and Antarctic Lands', 'Heard Island and McDonald Islands',
                          'British Indian Ocean Territory', 'Christmas Island', 'Vanuatu','United States Minor Outlying Islands',
                          'Nauru', 'Reunion', 'Saint Kitts and Nevis', 'Seychelles', 'Saint Lucia', 'Tokelau', 'Tonga',
                          'Tuvalu','Saint Vincent and the Grenadines', 'British Virgin Islands', 'United States Virgin Islands',
                          'Wallis and Futuna Islands', 'Samoa', 'Guadeloupe', 'Netherlands Antilles', 'Pitcairn Islands','
                          Palau', 'Marshall Islands', 'Saint Pierre and Miquelon', 'Saint Helena', 'San Marino',
                          'Turks and Caicos Islands', 'Svalbard', 'Saint Martin', 'Saint Barthelemy', 'South Georgia South Sandwich Islands',
                          'Guernsey', 'Jersey')
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[!wrld_simpl_sf_eckiv$NAME %in% remove,]

  colors2 = c('1' = "#FCFDBFFF", '2' = "#FECE91FF",'3' = "#FE9F6DFF", '4'= "#F76F5CFF", 
              '5' = "#DE4968FF", '6' = "#B63679FF",'7' = "#8C2981FF", '8' = "#641A80FF", 
              '9' = "#3B0F70FF", '10'= "#150E37FF", '11' = "#000004FF")
  # BBOX
  # xmin      ymin      xmax      ymax 
  # -15861702  -6637514  15337601   8373855 
  
  # create maps
  gg_GHG = ggplot() + 
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_GHG[,.(cell, x, y, cov_s_GHG)],
              aes(x = x, y = y, fill = cov_s_GHG)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,0,0), "null"),
          axis.ticks = element_blank())
  gg_GHG
  
  
  colors_bar2 =c("#FCFDBFFF",  "#FECE91FF","#FE9F6DFF", "#F76F5CFF", 
                 "#DE4968FF", "#B63679FF","#8C2981FF", "#641A80FF", 
                 "#3B0F70FF", "#150E37FF", "#000004FF")
                          
  # create legend
  gg_legend1 = plot_discrete_cbar(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
                                  colors = colors_bar2,
                                  legend_title = expression(GHG~Emissions~CoV~'('~'%'~')'), # fix
                                  spacing = 'constant',
                                  font_size = 4)
  gg_legend1
  
  gg_maps = list(GHG = gg_GHG, legend1 = gg_legend1)
  return(gg_maps)
}
cov_yield_map_fig   = function(dt) {
  # set xy
  dt_r = as.data.frame(dt, xy = TRUE)
  setDT(dt_r)
  
  # create manual breaks
  gr.quants  = dt_r[, quantile(cov_s_cgrain,seq.int(0,1,length.out = 6), na.rm = TRUE)]
  
  # make manual quantiles
  gr.quants = c(0,10,20,30,40,50,60,70,80,90,100,Inf)
  dt_r[, quant.cuts.gr := cut(cov_s_cgrain, breaks = gr.quants)]
  
  # create raster
  dt_r = as.data.frame(dt_r, xy = TRUE)
  r                = rast(nrow = 360, ncol = 720, nlyr = 1, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  crs(r)           = "epsg:4326"
  
  # original projection
  r_lat_yield = rast(res = 0.5, nlyr = 1, extent = ext(r), crs = crs(r)) 
  r_lat_yield[[1]][dt_r$gridid] = dt_r$quant.cuts.gr
  names(r_lat_yield) = c('cov_s_cgrain')
  # equal area projection
  newcrs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # get new output dimensions
  project(r_lat_yield, newcrs)
  # dimensions  : 569, 1138, 1  (nrow, ncol, nlyr)
  # resolution  : 29727.52, 29727.52  (x, y)
  # extent      : -16921203, 16908719, -8454359, 8460601  (xmin, xmax, ymin, ymax)
  r_newcrs   = rast(ncols = 1138, nrows = 569, nlyr = 1, xmin = -16921203, xmax = 16908719, ymin = -8454359, ymax = 8460601, crs = newcrs)
  r_eckiv    = project(r_lat_yield, r_newcrs)
  
  r_eckiv_dt = as.data.frame(r_eckiv, cells = TRUE, xy = TRUE)
  r_eckiv_dt = setDT(r_eckiv_dt)
  r_eckiv_dt_yield = r_eckiv_dt[,.(cell, x, y, cov_s_cgrain)]
  r_eckiv_dt_yield[, cov_s_cgrain := round(cov_s_cgrain, digits = 0)]
  r_eckiv_dt_yield[, cov_s_cgrain := as.character(cov_s_cgrain)]
  
  # create sf object
  data(wrld_simpl)
  wrld_simpl_sf = sf::st_as_sf(wrld_simpl)
  wrld_simpl_sf_eckiv = st_transform(wrld_simpl_sf, crs = newcrs)
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[wrld_simpl_sf_eckiv$NAME != 'Antarctica',]
  
  small_islands       = wrld_simpl_sf[wrld_simpl_sf$AREA < 10000,]
  remove              = c('Antigua and Barbuda', 'American Samoa', 'Barbados', 'Bermuda',
                          'Bahamas', 'Solomon Islands', 'Cayman Islands', 'Comoros','Cook Islands', 'Cape Verde',
                          'Dominica', 'Fiji','Falkland Islands (Malvinas)', 'Micronesia, Federated States of', 'Grenada',
                          'New Caledonia', 'Niue', 'Anguilla','French Polynesia', 'Guam', 'Kiribati', 'Martinique','Maldives', 'Aruba', 'Northern Mariana Islands',
                          'Faroe Islands', 'Mayotte', 'Mauritius','Aaland Islands', 'Norfolk Island', 'Cocos (Keeling) Islands',
                          'Bouvet Island', 'French Southern and Antarctic Lands', 'Heard Island and McDonald Islands',
                          'British Indian Ocean Territory', 'Christmas Island', 'Vanuatu','United States Minor Outlying Islands',
                          'Nauru', 'Reunion', 'Saint Kitts and Nevis', 'Seychelles', 'Saint Lucia', 'Tokelau', 'Tonga',
                          'Tuvalu','Saint Vincent and the Grenadines', 'British Virgin Islands', 'United States Virgin Islands',
                          'Wallis and Futuna Islands', 'Samoa', 'Guadeloupe', 'Netherlands Antilles', 'Pitcairn Islands','
                          Palau', 'Marshall Islands', 'Saint Pierre and Miquelon', 'Saint Helena', 'San Marino',
                          'Turks and Caicos Islands', 'Svalbard', 'Saint Martin', 'Saint Barthelemy', 'South Georgia South Sandwich Islands',
                          'Guernsey', 'Jersey')
  wrld_simpl_sf_eckiv = wrld_simpl_sf_eckiv[!wrld_simpl_sf_eckiv$NAME %in% remove,]
  
  colors2 = c('1' = "#FCFDBFFF", '2' = "#FECE91FF",'3' = "#FE9F6DFF", '4'= "#F76F5CFF", 
              '5' = "#DE4968FF", '6' = "#B63679FF",'7' = "#8C2981FF", '8' = "#641A80FF", 
              '9' = "#3B0F70FF", '10'= "#150E37FF", '11' = "#000004FF")
  # BBOX
  # xmin      ymin      xmax      ymax 
  # -15861702  -6637514  15337601   8373855 
  
  gg_gr = ggplot() +
    geom_sf(data = wrld_simpl_sf_eckiv, fill = "grey75",
            colour = "grey45", size = 0.2) +
    theme_map() +
    geom_tile(data = r_eckiv_dt_yield[,.(cell, x, y, cov_s_cgrain)],
              aes(x = x, y = y, fill = cov_s_cgrain)) +
    scale_fill_manual(values = colors2) +
    theme(legend.position='none',
          plot.margin = unit(c(0,0,-2,0), "cm"))
  gg_gr
  
  colors_bar2 =c("#FCFDBFFF",  "#FECE91FF","#FE9F6DFF", "#F76F5CFF", 
                            "#DE4968FF", "#B63679FF","#8C2981FF", "#641A80FF", 
                            "#3B0F70FF", "#150E37FF", "#000004FF")
                            
  # create legend
  gg_legend2 = plot_discrete_cbar(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
                                  colors = colors_bar2,
                                  legend_title = expression(Yield~CoV~'('~'%'~')'),
                                  spacing = 'constant',
                                  font_size = 4)
  gg_legend2
  
  
  gg_maps = list(grain = gg_gr, legend2 = gg_legend2)
  return(gg_maps)
}
barplot_fig     = function(dt) {
  scenario_lbl = c('ccg-res' = c('Grass CC'),
                   'ccl-res' = c('Legume CC'), 
                   'ccg-ntill' = c('Grass CC + Ntill'),
                   'ccl-ntill' = c('Legume CC + Ntill'))
  dt$scenario  = factor(dt$scenario, levels = c('ccg-res', 'ccg-ntill', 'ccl-res', 'ccl-ntill'))
  ipcc_lbl     = c('GLB'   = c('GLOBE'),
                   'ADP'   = c('ADP'),
                   'AME'   = c('AME'),
                   'DEV'   = c('DEV'),
                   'EEWCA' = c('EEWCA'),
                   'LAC'   = c('LAC'))
  dt$IPCC_NAME = factor(dt$IPCC_NAME, levels = c('GLB', 'ADP', 'AME', 'DEV', 'EEWCA', 'LAC'))

  gg1 = ggplot(dt, aes(x = scenario, y = d_s_GHG, group = interaction(scenario, IPCC_NAME), 
                       fill = IPCC_NAME)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin= d_s_GHG - se_s_GHG, ymax = d_s_GHG + se_s_GHG), 
                  width=.4, position=position_dodge(0.8)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccl-ntill','ccl-res', 
                                                       'ccg-ntill', 'ccg-res')) +
    ylab((expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Mg~CO[2]*-eq*~ha^-1*~yr^-1*')')))) +
    scale_y_continuous(limits = c(-3,4), breaks = seq(-3,4, 0.5)) +
    scale_fill_manual(name = 'Region', labels = ipcc_lbl, values = c("#913640","#040404",
                                                                              "#4B4C40",
                                                                              "#CC79A7",
                                                                              "#928261",
                                                                              "#EAAD89")) +
   theme_bw() +
    theme(text         = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.y = element_blank(),
          legend.position  = 'bottom') +
    guides(fill = guide_legend(title.position = "top", ncol = 1))
  gg1
  
  gg2 = ggplot(dt, aes(x = scenario, y = d_s_SOC, group = interaction(scenario, IPCC_NAME), 
                       fill = IPCC_NAME)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin= d_s_SOC - se_s_SOC, ymax = d_s_SOC + se_s_SOC), 
                  width=.4, position=position_dodge(0.8)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccl-ntill','ccl-res', 
                                                       'ccg-ntill', 'ccg-res')) +
    ylab((expression(atop(paste(Annual~SOC~Sequestration), '('*Mg~CO[2]*-eq*~ha^-1*~yr^-1*')')))) +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5, 1)) +
    scale_fill_manual(name = 'Region', labels = ipcc_lbl, values = c("#913640","#040404",
                                                                              "#4B4C40",
                                                                              "#CC79A7",
                                                                              "#928261",
                                                                              "#EAAD89")) +
    theme_bw() +
    theme(text         = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.y = element_blank(),
          legend.position  = 'bottom') +
    guides(fill = guide_legend(title.position = "top", ncol = 1))
  gg2
  
  gg3 = ggplot(dt, aes(x = scenario, y = d_s_N2O, group = interaction(scenario, IPCC_NAME), 
                       fill = IPCC_NAME)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin= d_s_N2O - se_s_N2O, ymax = d_s_N2O + se_s_N2O), 
                  width=.4, position=position_dodge(0.8)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccl-ntill','ccl-res', 
                                                       'ccg-ntill', 'ccg-res')) +
    ylab((expression(atop(paste(Annual~N2O~Emissions), '('*Mg~CO[2]*-eq*~ha^-1*~yr^-1*')')))) +
    scale_y_continuous(limits = c(-4,2), breaks = seq(-4,2, 0.5)) +
    scale_fill_manual(name = 'Region', labels = ipcc_lbl, values = c("#913640","#040404",
                                                                              "#4B4C40",
                                                                              "#CC79A7",
                                                                              "#928261",
                                                                              "#EAAD89")) +
    theme_bw() +
    theme(text         = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.y = element_blank(),
          legend.position  = 'bottom') +
    guides(fill = guide_legend(title.position = "top", ncol = 1))
  gg3
  
  gg4 = ggplot(dt, aes(x = scenario, y = d_s_cgrain, group = interaction(scenario, IPCC_NAME), 
                       fill = IPCC_NAME)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin= d_s_cgrain - se_s_cgrain, ymax = d_s_cgrain + se_s_cgrain), 
                  width=.4, position=position_dodge(0.8)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccl-ntill','ccl-res', 
                                                       'ccg-ntill', 'ccg-res')) +
    ylab((expression(atop(paste(Annual~Yield~Difference), '('*Mg~ha^-1*~yr^-1*')')))) +
    scale_y_continuous(limits = c(-2,2), breaks = seq(-2,2, 0.5)) +
    scale_fill_manual(name = 'Region', labels = ipcc_lbl, values = c("#913640","#040404",
                                                                              "#4B4C40",
                                                                              "#CC79A7",
                                                                              "#928261",
                                                                              "#EAAD89")) +
    theme_bw() +
    theme(text         = element_text(color = 'black', size = 7),
          axis.text    = element_text(size = 7, color = 'black'),
          strip.text   = element_text(size = 6, color = 'black'),
          axis.title.y = element_blank(),
          legend.position  = 'bottom') +
    guides(fill = guide_legend(title.position = "top", ncol = 1))
  gg4
  
  return(list(ghg = gg1, soc = gg2, n2o = gg3, yield = gg4))
}
# feature_p
bmp_regional_fig    = function(dt, reg, g, y, constant) {
  dt_r = copy(dt)
  dt_r = dt_r[IPCC_NAME %in% reg & goal %in% g & y_block %in% y,]
  
  scenario_lbl = c('ccg-res' = c('Grass CC + Res + Till'),     
                   'ccl-res' = c('Legume CC + Res + Till'),    
                   'ccg-ntill' = c('Grass CC + Res + Ntill'),  
                   'ccl-ntill' = c('Legume CC + Res + Ntill'), 
                   'cp'        = c('Continued Management'))    
  dt$scenario  = factor(dt$scenario, levels = c('ccg-res', 'ccg-ntill', 'ccl-res', 'ccl-ntill', 'cp'))
  
  goal_lbl     = c('max-ghg'      = 'M', 
                   'max-ghg-yc'   = 'C',
                   'max-yield'    = 'M',
                   'max-yield-gc' = 'C')
  
  # GHG
  gg_GHG = ggplot(dt_r[!scenario == 'cp'], aes(x = scenario, y = s_GHG/constant, fill = scenario)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin = (s_GHG/constant) - (sd_s_GHG/constant), 
                      ymax = (s_GHG/constant) + (sd_s_GHG/constant)), 
                  width=.4, position=position_dodge(0.8)) +
    geom_hline(yintercept = 0, color = "grey50") +
    facet_grid(.~goal, labeller = as_labeller(goal_lbl)) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccg-res', 'ccg-ntill',
                                                       'ccl-res', 'ccl-ntill')) +
    ylab((expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Tg~CO[2]*-eq*~yr^-1*')')))) +
    scale_fill_manual(labels = scenario_lbl, values = c('ccg-res'   = "#8B0069",
                                                        'ccl-res'   = "#A75529",
                                                        'ccg-ntill' = "#9A9800", 
                                                        'ccl-ntill' = "#5DD291",
                                                        'cp'        = "#B0F4FA")) +
    theme_bw() +
    theme(legend.position = 'none',
          text = element_text(color = 'grey30', size = 5),
          axis.text    = element_text(size = 5, color = 'grey30'),
          strip.text   = element_text(size = 5, color = 'grey30'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_rect(fill='transparent'),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  gg_GHG
  
  # yield 
  gg_yield = ggplot(dt_r[!scenario == 'cp'], aes(x = scenario, y = s_grain/constant, fill = scenario)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin = (s_grain/constant) - (sd_s_grain/constant), 
                      ymax = (s_grain/constant) + (sd_s_grain/constant)), 
                  width=.4, position=position_dodge(0.8)) +
    geom_hline(yintercept = 0, color = "grey50") +
    facet_grid(.~goal, labeller = as_labeller(goal_lbl)) +
    scale_x_discrete(labels = scenario_lbl, limits = c('ccg-res', 'ccg-ntill',
                                                       'ccl-res', 'ccl-ntill')) +
    ylab((expression(atop(paste(Annual~Yield~Potential), '('*Tg~yr^-1*')')))) +
    scale_fill_manual(labels = scenario_lbl, values = c('ccg-res'   = "#8B0069",
                                                        'ccl-res'   = "#A75529",
                                                        'ccg-ntill' = "#9A9800", 
                                                        'ccl-ntill' = "#5DD291",
                                                        'cp'        = "#B0F4FA")) +
    theme_bw() +
    theme(legend.position = 'none',
          text = element_text(color = 'grey30', size = 5),
          axis.text    = element_text(size = 5, color = 'grey30'),
          strip.text   = element_text(size = 5, color = 'grey30'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_rect(fill='transparent'),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  gg_yield
  
  # hectares / stacked bplot 
  gg_area = ggplot(dt_r, aes(fill = scenario, y = IPCC_NAME, x = m_hectares)) +
    geom_bar(position = 'fill', stat = 'identity', width = 0.7) +
    coord_flip() +
    facet_grid(.~goal, labeller = as_labeller(goal_lbl)) +
    xlab('Proportion of Cropland Area') +
    scale_fill_manual(labels = scenario_lbl, values = c('ccg-res'   = "#8B0069",
                                                        'ccl-res'   = "#A75529",
                                                        'ccg-ntill' = "#9A9800", 
                                                        'ccl-ntill' = "#5DD291",
                                                        'cp'        = "#B0F4FA")) +
    theme_bw() +
    theme(legend.position = 'none',
          text = element_text(color = 'grey30', size = 5),
          axis.text    = element_text(size = 5, color = 'grey30'),
          strip.text   = element_text(size = 5, color = 'grey30'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_rect(fill='transparent'),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  gg_area
  
  ggplots = list(GHG = gg_GHG, YIELD = gg_yield, AREA = gg_area)
  return(ggplots)
}