# filename: figures-main.R
# created:     19 December 2024
# updated:     30 March 2025
# author:      S.C. McClelland
# description: This file creates figures included in the main manuscript.
#-----------------------------------------------------------------------------------------
# LIBRARIES 
#-----------------------------------------------------------------------------------------
library(colorspace)
library(cowplot)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(grid)
library(gridtext)
library(gridExtra)
library(maptools)
library(RColorBrewer)
library(rstudioapi)
library(patchwork)
library(scales)
library(sf)
library(shapviz)
library(stringr)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# DIRECTORIES, FUNCTIONS & FILES
#-----------------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
source('r/figure-functions.R')
source('r/plot_discrete_color_bar.R')
data_p  = paste(dir, 'data/analysis-output', sep = '/')
input_p = paste(dir, 'data/daycent-post-processed', sep = '/')
out_p   = paste(dir, 'figures/main', sep = '/')
#-----------------------------------------------------------------------------------------
# Figure 1. Two panel scatterplot
#-----------------------------------------------------------------------------------------
# load data
# GHG
ghg_ccg_res = fread(paste(data_p, 'ccg-res-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccg_res = ghg_ccg_res[, -c('m_hectares', 'se_hectares')]
ghg_ccl_res = fread(paste(data_p, 'ccl-res-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccl_res = ghg_ccl_res[, -c('m_hectares', 'se_hectares')]
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccg_ntill = ghg_ccg_ntill[, -c('m_hectares', 'se_hectares')]
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccl_ntill = ghg_ccl_ntill[, -c('m_hectares', 'se_hectares')]
# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# YIELD
yield_ccg_res = fread(paste(data_p, 'ccg-res-cumulative-yield-potential.csv', sep = '/'))
yield_ccg_res = yield_ccg_res[, -c('m_hectares', 'se_hectares')]
yield_ccl_res = fread(paste(data_p, 'ccl-res-cumulative-yield-potential.csv', sep = '/'))
yield_ccl_res = yield_ccl_res[, -c('m_hectares', 'se_hectares')]
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-cumulative-yield-potential.csv', sep = '/'))
yield_ccg_ntill = yield_ccg_ntill[, -c('m_hectares', 'se_hectares')]
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-cumulative-yield-potential.csv', sep = '/'))
yield_ccl_ntill = yield_ccl_ntill[, -c('m_hectares', 'se_hectares')]
# combine
yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)

# join ghg and yield
data = ghg[yield, on = .(scenario = scenario,
                         y_block = y_block,
                         IPCC_NAME = IPCC_NAME)]
# flip signs for ghg
data = data[, s_GHG := ifelse(s_GHG < 0, s_GHG*-1, s_GHG*-1)]

# make annual, Mg CO2-eq
data_2050 = data[y_block == 2050, lapply(.SD, function(x) {x/35}), .SDcols = c('s_GHG', 'se_s_GHG',
                                                                               's_grain', 'se_s_grain'),
                 by = .(scenario, y_block, IPCC_NAME)]
data_2100 = data[y_block == 2100, lapply(.SD, function(x) {x/85}), .SDcols = c('s_GHG', 'se_s_GHG',
                                                                               's_grain', 'se_s_grain'),
                 by = .(scenario, y_block, IPCC_NAME)]
# constant
Mg_t_Pg = 1e9
Mg_t_Tg = 1e6

  ## for extracting values only ##
# data_2050 = data_2050[y_block == 2050, lapply(.SD, function(x) {x/Mg_t_Pg}), .SDcols = c('s_GHG', 'sd_s_GHG',
#                                                                                's_grain', 'sd_s_grain'),
#                  by = .(scenario, y_block, IPCC_NAME)]
# data_2100 = data_2100[y_block == 2100, lapply(.SD, function(x) {x/Mg_t_Pg}), .SDcols = c('s_GHG', 'sd_s_GHG',
#                                                                                's_grain', 'sd_s_grain'),
#                  by = .(scenario, y_block, IPCC_NAME)]


# Create plot, left
fig1_t = scatterplot_fig(data_2050, Mg_t_Pg, Mg_t_Tg)
# rescale x, y lim

# Create plot, right
fig1_b = scatterplot_fig(data_2100, Mg_t_Pg, Mg_t_Tg)
# rescale x, y lim

# update legends, axes, and add labels to corners
fig1_tl = fig1_t$glb + theme(
                        # axis.title.x = element_blank(),
                        # axis.title.y = element_blank(),
                        legend.position = 'none') +
  xlim(-0.5, 1) + ylim(-0.2, 0.3) +
  guides(color = "none") +
  # Top left
  annotate("text", x = -0.5, y = 0.3, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 1, y = 0.3, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -0.5, y = -0.2, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 1, y = -0.2, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add a label
fig1_tl = fig1_tl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
ggtitle('Global, near-term (2016-2050)', '(a)')

fig1_bl = fig1_b$glb + theme(
                        # axis.title.y = element_blank(),
                        # axis.title.x = element_blank(),
                        legend.position = "none") +
  xlim(-0.5, 1) + ylim(-0.2, 0.3) +
  # Top left
  annotate("text", x = -0.5, y = 0.05, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 1, y = 0.3, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -0.5, y = -0.2, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 1, y = -0.2, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add a label
fig1_bl = fig1_bl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle('Global, medium-term (2016-2100)', '(c)')

fig1_tr = fig1_t$reg + theme(
                             # axis.title.x = element_blank(),
                             # axis.title.y = element_blank(),
                             legend.position = "right",
                             legend.box.just = "left",
                             legend.justification = c(0, 0),
                             legend.box.margin = margin(0, 0, 0, 0)) +
  guides(color = "none") +
  xlim(-200,450) + ylim(-100,150) +
  # Top left
  annotate("text", x = -200, y = 150, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 450, y = 150, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -200, y = -100, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 450, y = -100, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add a label
fig1_tr = fig1_tr + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle('Regional, near-term (2016-2050)', '(b)')

fig1_br = fig1_b$reg + theme(
                             # axis.title.y = element_blank(),
                             # axis.title.x = element_blank(),
                             legend.position = "right",
                             legend.box.just = "left",
                             legend.justification = c(0, 1),
                             legend.box.margin = margin(0, 0, 0, 0)) +
  guides(shape = "none") +
  xlim(-200,450) + ylim(-100,150) +
  # Top left
  annotate("text", x = -200, y = 150, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 450, y = 150, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -200, y = -100, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 450, y = -100, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add a label
fig1_br = fig1_br + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.005,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle('Regional, medium-term (2016-2100)', '(d)')
# get legends
color_legend = get_legend(fig1_br)
shape_legend = get_legend(fig1_tr)

fig1_tr = fig1_tr + theme(legend.position = 'none')
fig1_br = fig1_br + theme(legend.position = 'none')

# combine
fig1_final = grid.arrange(fig1_tl, fig1_tr, shape_legend, fig1_bl, fig1_br, color_legend, ncol = 3, nrow = 2,
                   widths = c(2,2,1), heights = c(2,2)
                   # , 
                   # left = yleft, bottom = x
                   )
ggsave(paste(out_p, 'figure1-main.pdf', sep = '/'), fig1_final, units = 'mm', width = 180, height = 180, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 2. Multi-panel maps
#-----------------------------------------------------------------------------------------
# load data | N.B. does not include imputed values
# GHG
ghg_ccg_res   = fread(paste(data_p, 'ccg-res-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccl_res   = fread(paste(data_p, 'ccl-res-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-weighted-gridid-ghg-responses.csv', sep = '/'))

# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# flip signs for ghg
ghg = ghg[, d_s_SOC := ifelse(d_s_SOC < 0, d_s_SOC*-1, d_s_SOC*-1)]
ghg = ghg[, d_s_N2O := ifelse(d_s_N2O < 0, d_s_N2O*-1, d_s_N2O*-1)]
ghg = ghg[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]
ghg = ghg[, -c('sd_s_SOC', 'sd_s_N2O', 'sd_s_GHG')]

# add xy coordinates
  ## input table ##
load(paste(input_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep coordinates
main_table      = main_table[, c('gridid', 'x', 'y')]
main_table      = unique(main_table)
# join
ghg = ghg[main_table, on = .(gridid = gridid)]
ghg = ghg[!is.na(scenario)]

# make annual, Mg CO2-eq ha-1 yr-1
ghg_2050 = ghg[y_block == 2050, lapply(.SD, function(x) {x/35}), 
               .SDcols = c('d_s_SOC', 'd_s_N2O','d_s_GHG'),
               by = .(scenario, y_block, gridid)]

# ccg-res
ccg_res_ghg_map   = ghg_map_fig(ghg_2050[scenario %in% 'ccg-res'])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
  #          hjust = 0, vjust = 1, size = 3) +
  ggtitle('Grass CC', '(a)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
ccg_res_ghg_map$legend1 = ccg_res_ghg_map$legend1 +
  theme(
    plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
    # Negative values will reduce the padding
    # Format is (top, right, bottom, left)
  ) +
  # scale_x_continuous(expand = c(0, 0)) +  # Remove expansion on x axis
  scale_y_continuous(expand = c(0, 0))    # Remove expansion on y axis
# ccl-res
ccl_res_ghg_map   = ghg_map_fig(ghg_2050[scenario %in% 'ccl-res'])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
  #          hjust = 0, vjust = 1, size = 3) +
  ggtitle('Legume CC', '(e)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_ghg_map = ghg_map_fig(ghg_2050[scenario %in% 'ccg-ntill'])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
  #          hjust = 0, vjust = 1, size = 3) +
  ggtitle('Grass CC + Ntill', '(c)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_ghg_map = ghg_map_fig(ghg_2050[scenario %in% 'ccl-ntill'])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  # annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
  #          hjust = 0, vjust = 1, size = 3) +
  ggtitle('Legume CC + Ntill', '(g)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))

# YIELD
# load data | N.B. does not include imputed values 
yield_ccg_res   = fread(paste(data_p, 'ccg-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_res   = fread(paste(data_p, 'ccl-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-weighted-gridid-yield-responses.csv', sep = '/'))

# combine
yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)

yield = yield[, -c('sd_s_cgrain')]

# add xy coordinates
# join
yield = yield[main_table, on = .(gridid = gridid)]
yield = yield[!is.na(scenario)]

# make annual, Mg ha-1 yr-1
yield_2050 = yield[y_block == 2050, lapply(.SD, function(x) {x/35}), 
               .SDcols = c('d_s_cgrain'),
               by = .(scenario, y_block, gridid)]

# ccg-res
ccg_res_y_map     = yield_map_fig(yield_2050[scenario %in% 'ccg-res'])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
  #          hjust = 0, vjust = 1, size = 3) 
  ggtitle('', '(b)') +
  theme(plot.subtitle = element_text(size = 6))
ccg_res_y_map$legend2 = ccg_res_y_map$legend2 +
  theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
) +
  # scale_x_continuous(expand = c(0, 0)) +  # Remove expansion on x axis
  scale_y_continuous(expand = c(0, 0))    # Remove expansion on y axis
# ccl-res
ccl_res_y_map     = yield_map_fig(yield_2050[scenario %in% 'ccl-res'])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
  #          hjust = 0, vjust = 1, size = 3) 
  ggtitle('', '(f)') +
  theme(plot.subtitle = element_text(size = 6))
# ccg-ntill
ccg_ntill_y_map   = yield_map_fig(yield_2050[scenario %in% 'ccg-ntill'])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
  #          hjust = 0, vjust = 1, size = 3)
  ggtitle('', '(d)') +
  theme(plot.subtitle = element_text(size = 6))
# ccl-ntill
ccl_ntill_y_map   = yield_map_fig(yield_2050[scenario %in% 'ccl-ntill'])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  # annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
  #          hjust = 0, vjust = 1, size = 3)
  ggtitle('', '(h)') +
  theme(plot.subtitle = element_text(size = 6))

# Multi-panel figure
fig2_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.10), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure2-main.pdf', sep = '/'), fig2_maps,  units = 'mm', width = 180, height = 210, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 3. Feature importance
#-----------------------------------------------------------------------------------------
# data
load(paste(data_p, 'ccg_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccg_ntill_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_ntill_SHAP.Rdata', sep = '/'))

# feature colors
f_colors = c(
             # 'Climate'    = "#201158",
             'Management' = "#007054",
             'Site'       = "#C19A1B",
             'Soil'       = "#FFCEF4"
               )

  ## ccg-res ##
# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Cash Crop', 'Water Management', 'Initial Residue Fraction',
                'Initial Soil Nitrate', 'Soil Bulk Density')
# types (actual order)
ccg_res_t   = c('Soil', 'Site', 'Management', 'Management', 'Management')
# plot
ccg_res_fig = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                        ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig = ccg_res_fig + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  )
) +
  ggtitle("Grass CC", "(a)")
  ## ccl-res ##
# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`w-w`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Initial Soil Nitrate', 'Nitrogen Inputs', 'Nitrate Fertilizer Fraction',
                'Water Management', 'Cash Crop')
# types (actual order)
ccl_res_t   = c('Management', 'Management', 'Management', 'Management', 'Site')
# plot
ccl_res_fig = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                        ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig = ccl_res_fig + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  ),
  legend.position = 'none'
) +
  ggtitle("Legume CC", "(c)")
  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Initial Residue Fraction', 'Water Management', 'Initial Soil Nitrate',
                'Cash Crop', 'Soil Bulk Density')
# types (actual order)
ccg_ntill_t   = c('Soil', 'Management', 'Site', 'Management', 'Management')
# plot
ccg_ntill_fig = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                        ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig = ccg_ntill_fig + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  ),
  legend.position = 'none'
) +
  ggtitle("Grass CC + Ntill","(b)")
  ## ccl-ntill ##
# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`w-w`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Initial Residue Fraction', 'Nitrogen Inputs', 'Soil Bulk Density',
                  'Initial Soil Nitrate', 'Cash Crop')
# types (actual order)
ccl_ntill_t   = c('Management', 'Site', 'Soil', 'Management', 'Management')
# plot
ccl_ntill_fig = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                          ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig = ccl_ntill_fig + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  ),
  plot.subtitle = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 6       # Match your other text size if needed
  ),
  legend.position = 'none'
) +
  ggtitle("Legume CC + Ntill", "(d)")
# common x-axis label
x     = textGrob(expression(paste("Mean ", "|", "SHAP Value", "|")),
                  gp = gpar(fontsize = 8))
# make combined figure
fig3_final = ggarrange(ccg_res_fig, ccg_ntill_fig, ccl_res_fig, ccl_ntill_fig,
          ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

# save
ggsave(paste(out_p, 'figure3-main.pdf', sep = '/'), fig3_final, units = 'mm', width = 180, height = 180, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 4. Partial dependence plots
#-----------------------------------------------------------------------------------------
# data
load(paste(data_p, 'ccg_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_res_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccg_ntill_SHAP.Rdata', sep = '/'))
load(paste(data_p, 'ccl_ntill_SHAP.Rdata', sep = '/'))
# feature colors
f_colors = c(
             'Climate'    = "#201158",
             'Management' = "#007054",
             'Site'       = "#C19A1B",
             'Soil'       = "#FFCEF4"
               )

  ## ccg-res ##
# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Cash Crop', 'Water Management', 'Initial Residue Fraction',
                'Initial Soil Nitrate', 'Soil Bulk Density')
# continuous features
# p1
ccg_res_p1  = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[1]),
                       f_colors[[4]], bquote(.(ccg_res_ft[[5]])~'('*g~cm^-3*')'), 'SLBLKD',
                       '(a)')
ccg_res_p1 = ccg_res_p1 + 
  ggtitle('Grass CC', '(a)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# p2
# check unit below
ccg_res_p2  = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[2]),
                       f_colors[[3]], bquote(.(ccg_res_ft[[4]])~'('*g~m^-2*')'), 'NITRAT_sum_',
                       '(b)')
# p3
ccg_res_p3 = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[3]),
                      f_colors[[2]], bquote(.(Initial~Residue~Fraction)), 'res.rtrn.amt', '(c)')
# categorical features
# p4
ccg_res_p4 = cat_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[4]),
                     f_colors[[2]], 'Water Management', 'irr', '(d)')
ccg_res_p4 = ccg_res_p4 + scale_x_discrete(labels = c('Rainfed', 'Irrigated'))
# p5
ccg_res_p5 = cat_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[5]),
                     f_colors[[2]], 'Cash Crop', 'crop', '(e)')
ccg_res_p5 = ccg_res_p5 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Initial Residue Fraction', 'Water Management', 'Initial Soil Nitrate',
                  'Cash Crop', 'Soil Bulk Density')
# continuous features
# p1
ccg_ntill_p1  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[1]),
                       f_colors[[4]], bquote(.(ccg_ntill_ft[[5]])~'('*g~cm^-3*')'), 'SLBLKD',
                       '(f)')
ccg_ntill_p1  = ccg_ntill_p1 + 
  ggtitle('Grass CC + Ntill', '(f)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# p3
ccg_ntill_p3  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[3]),
                         f_colors[[3]], bquote(.(ccg_ntill_ft[[3]])~'('*g~cm^-2*')'), 'NITRAT_sum_',
                         '(h)')
# p5
ccg_ntill_p5  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[5]),
                         f_colors[[2]], bquote(.(Initial~Residue~Fraction)), 
                         'res.rtrn.amt',
                         '(j)')
# categorical features
# p2
ccg_ntill_p2  = cat_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[2]),
                         f_colors[[2]], ccg_ntill_ft[[4]], 'crop',
                         '(g)')
ccg_ntill_p2 = ccg_ntill_p2 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
# p4
ccg_ntill_p4  = cat_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[4]),
                        f_colors[[2]], ccg_ntill_ft[[2]], 'irr',
                        '(i)')
ccg_ntill_p4 = ccg_ntill_p4 + scale_x_discrete(labels = c('Rainfed', 'Irrigated'))
  ## ccl-res ##
# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`w-w`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Initial Soil Nitrate', 'Nitrogen Inputs', 'Nitrate Fertilizer Fraction',
                'Water Management', 'Csh Crop')
# categorical features
# p1
ccl_res_p1 = cat_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[1]),
                     f_colors[[2]], 'Cash Crop', 'crop', '(k)')
ccl_res_p1 = ccl_res_p1 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
ccl_res_p1 = ccl_res_p1 + 
  ggtitle('Legume CC', '(k)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# p2
ccl_res_p2 = cat_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[2]),
                     f_colors[[2]], 'Water Management', 'irr','(l)')
ccl_res_p2 = ccl_res_p2 + scale_x_discrete(labels = c('Rainfed', 'Irrigated'))
# continuous features
# p3 
ccl_res_p3  = cont_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[3]),
                       f_colors[[2]], ccl_res_ft[[3]], 'frac_NO3',
                       '(m)')
# p4 
ccl_res_p4  = cont_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[4]),
                       f_colors[[2]], bquote(.(ccl_res_ft[[2]])~'('*g~m^-2*')'), 'Namt',
                       '(n)')
# p5 
ccl_res_p5  = cont_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[5]),
                       f_colors[[3]], bquote(.(ccl_res_ft[[1]])~'('*g~m^-2*')'), 'NITRAT_sum_',
                       '(o)')
  ## ccl-ntill ##
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`w-w`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Initial Residue Fraction', 'Nitrogen Inputs', 'Soil Bulk Density',
                  'Initial Soil Nitrate', 'Cash Crop')
# categorical features
# p1 
ccl_ntill_p1  = cat_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[1]),
                        f_colors[[2]], ccl_ntill_ft[[5]], 'crop',
                        '(p)')
ccl_ntill_p1 = ccl_ntill_p1 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
ccl_ntill_p1 = ccl_ntill_p1 + 
  ggtitle('Legume CC + Ntill', '(p)') +
  theme(plot.title    = element_text(size = 7),
        plot.subtitle = element_text(size = 6))
# continuous features
# p2 
ccl_ntill_p2  = cont_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[2]),
                         f_colors[[3]], bquote(.(ccl_ntill_ft[[4]])~'('*g~cm^-2*')'), 'NITRAT_sum_',
                         '(q)')
# p3 
ccl_ntill_p3  = cont_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[3]),
                         f_colors[[4]], bquote(.(ccl_ntill_ft[[3]])~'('*g~cm^-3*')'), 'SLBLKD',
                         '(r)')
# p4 
ccl_ntill_p4  = cont_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[4]),
                         f_colors[[2]], bquote(.(ccl_ntill_ft[[2]])~'('*g~cm^-2*')'), 'Namt',
                         '(s)')
# p5 
ccl_ntill_p5  = cont_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[5]),
                         f_colors[[2]], bquote(.(Initial~Residue~Fraction)), 'res.rtrn.amt',
                         '(t)')
# grid, 5 x 4, takes some time to load
grid_p = ggarrange(ccg_res_p1, ccg_res_p2, ccg_res_p3, ccg_res_p4, ccg_res_p5,
                   ccg_ntill_p1, ccg_ntill_p2, ccg_ntill_p3, ccg_ntill_p4, ccg_ntill_p5,
                   ccl_res_p1, ccl_res_p2, ccl_res_p3, ccl_res_p4, ccl_res_p5,   
                   ccl_ntill_p1, ccl_ntill_p2, ccl_ntill_p3, ccl_ntill_p4, ccl_ntill_p5,
                      ncol=5, nrow=4)
grid_p
# save
ggsave(paste(out_p, 'figure4-main.pdf', sep = '/'), grid_p, units = 'mm', width = 180, height = 180, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 5. Recommended practices by region (GHG focus)
#-----------------------------------------------------------------------------------------
# N.B. these plots are used in a pptx file to create the final figure
# load data
max_ghg_yc_dt   = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-practices.csv', sep = '/'))

# add xy coordinates
## input table ##
load(paste(input_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep coordinates
main_table      = main_table[, c('gridid', 'x', 'y')]
main_table      = unique(main_table)
# join
max_ghg_yc_dt   = max_ghg_yc_dt[main_table, on = .(gridid = gridid)]
max_ghg_yc_dt   = max_ghg_yc_dt[!is.na(scenario)]

# 2050
max_ghg_yc_2050_dt   = max_ghg_yc_dt[y_block %in% 2050,]

# 2100
max_ghg_yc_2100_dt   = max_ghg_yc_dt[y_block %in% 2100,]

# max ghg (yield constrained) maps
# 2050
max_ghg_yc_2050_map = bmp_map(max_ghg_yc_2050_dt)
max_ghg_yc_2050_map$bmp = max_ghg_yc_2050_map$bmp + theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
)
# 2100
max_ghg_yc_2100_map = bmp_map(max_ghg_yc_2100_dt)
max_ghg_yc_2100_map$bmp = max_ghg_yc_2100_map$bmp + theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
)

  ## IPCC MAP ##
background_map = IPCC_map(input_p, 'shp/WB_countries_Admin0_10m.shp', 'msw-masked-cropland-rf-ir-area.tif')
background_map$IPCC = background_map$IPCC + theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
)
# Save
ggsave(paste(out_p, 'figure5-near.pdf', sep = '/'), max_ghg_yc_2050_map$bmp,    units = 'mm', width = 60, height = 30, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure5-medium.pdf', sep = '/'), max_ghg_yc_2100_map$bmp,  units = 'mm', width = 60, height = 30, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure5-map.pdf', sep = '/'), background_map$IPCC, bg = 'transparent', units = 'mm', width = 180, height = 160, device='pdf', dpi=500)

# Estimate cropland area % by region, scenario
# GHG
s_max_ghg_yc_dt     = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential-by-scenario.csv', sep = '/'))
s_max_ghg_yc_dt[, t_hectares := lapply(.SD, sum), .SDcols = 'm_hectares', by = .(y_block, IPCC_NAME)]
setorder(s_max_ghg_yc_dt, y_block, IPCC_NAME)
s_max_ghg_yc_dt[, p_hectares := (m_hectares/t_hectares)*100]
s_max_ghg_yc_dt = s_max_ghg_yc_dt[, c('y_block', 'IPCC_NAME', 'scenario', 'p_hectares')]
