# filename: figures-main.R
# created:     19 December 2024
# updated:     13 January 2025
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
ghg_ccg_res = ghg_ccg_res[, -c('m_hectares', 'sd_hectares')]
ghg_ccl_res = fread(paste(data_p, 'ccl-res-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccl_res = ghg_ccl_res[, -c('m_hectares', 'sd_hectares')]
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccg_ntill = ghg_ccg_ntill[, -c('m_hectares', 'sd_hectares')]
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-cumulative-ghg-potential.csv', sep = '/'))
ghg_ccl_ntill = ghg_ccl_ntill[, -c('m_hectares', 'sd_hectares')]
# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# YIELD
yield_ccg_res = fread(paste(data_p, 'ccg-res-cumulative-yield-potential.csv', sep = '/'))
yield_ccg_res = yield_ccg_res[, -c('m_hectares', 'sd_hectares')]
yield_ccl_res = fread(paste(data_p, 'ccl-res-cumulative-yield-potential.csv', sep = '/'))
yield_ccl_res = yield_ccl_res[, -c('m_hectares', 'sd_hectares')]
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-cumulative-yield-potential.csv', sep = '/'))
yield_ccg_ntill = yield_ccg_ntill[, -c('m_hectares', 'sd_hectares')]
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-cumulative-yield-potential.csv', sep = '/'))
yield_ccl_ntill = yield_ccl_ntill[, -c('m_hectares', 'sd_hectares')]
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
data_2050 = data[y_block == 2050, lapply(.SD, function(x) {x/35}), .SDcols = c('s_GHG', 'sd_s_GHG',
                                                                               's_grain', 'sd_s_grain'),
                 by = .(scenario, y_block, IPCC_NAME)]
data_2100 = data[y_block == 2100, lapply(.SD, function(x) {x/85}), .SDcols = c('s_GHG', 'sd_s_GHG',
                                                                               's_grain', 'sd_s_grain'),
                 by = .(scenario, y_block, IPCC_NAME)]
# constant
Mg_t_Pg = 1e9

  ## for extracting values only ##
# data_2050 = data_2050[y_block == 2050, lapply(.SD, function(x) {x/Mg_t_Pg}), .SDcols = c('s_GHG', 'sd_s_GHG',
#                                                                                's_grain', 'sd_s_grain'),
#                  by = .(scenario, y_block, IPCC_NAME)]
# data_2100 = data_2100[y_block == 2100, lapply(.SD, function(x) {x/Mg_t_Pg}), .SDcols = c('s_GHG', 'sd_s_GHG',
#                                                                                's_grain', 'sd_s_grain'),
#                  by = .(scenario, y_block, IPCC_NAME)]


# Create plot, left
fig1_t = scatterplot_fig(data_2050, Mg_t_Pg)
# Create plot, right
fig1_b = scatterplot_fig(data_2100, Mg_t_Pg)

# Modify for final figure
# save common axis labels
x     = textGrob(expression(atop(paste(Annual~GHG~Mitigation~Potential), '('*Pg~CO[2]*-eq*~yr^-1*')')), 
                  gp = gpar(fontsize = 9))
yleft = textGrob(expression(atop(paste(Annual~Yield~Difference), '('*Pg~yr^-1*')')), 
                 rot = 90, gp = gpar(fontsize = 9))
# update legends, axes, and add labels to corners
fig1_t = fig1_t + theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.position = "right",
                        legend.box.just = "left",
                        legend.justification = "left",
                        legend.box.margin = margin(0, 0, 0, 0)) +
         guides(color = "none") +
  # Top left
  annotate("text", x = -1.2, y = 0.5, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 3) +
  
  # Top right
  annotate("text", x = 1.2, y = 0.5, label = "Jointly-favorable", 
           hjust = 1, vjust = 1, size = 3) +
  
  # Bottom left
  annotate("text", x = -1.2, y = -0.5, label = "Neither-favorable", 
           hjust = 0, vjust = 0, size = 3) +
  
  # Bottom right
  annotate("text", x = 1.2, y = -0.5, label = "Climate-favorable", 
           hjust = 1, vjust = 0, size = 3)
# add a label
fig1_t = fig1_t + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 9       # Match your other text size if needed
  )
) +
  ggtitle("(a)")
fig1_b = fig1_b + theme(axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        legend.position = "right",
                        legend.box.just = "left",
                        legend.justification = "left",
                        legend.box.margin = margin(0, 0, 0, 0)) +
  guides(shape = "none", 
         color = guide_legend(title.position = "top", nrow = 7)) +
  # Top left
  annotate("text", x = -1.2, y = 0.5, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 3) +
  
  # Top right
  annotate("text", x = 1.2, y = 0.5, label = "Jointly-favorable", 
           hjust = 1, vjust = 1, size = 3) +
  
  # Bottom left
  annotate("text", x = -1.2, y = -0.5, label = "Neither-favorable", 
           hjust = 0, vjust = 0, size = 3) +
  
  # Bottom right
  annotate("text", x = 1.2, y = -0.5, label = "Climate-favorable", 
           hjust = 1, vjust = 0, size = 3)
# add b label
fig1_b = fig1_b + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 9       # Match your other text size if needed
  )
) +
  ggtitle("(b)")

shape_legend = get_legend(fig1_t)
color_legend = get_legend(fig1_b)
fig1_t = fig1_t + theme(legend.position = 'none')
fig1_b = fig1_b + theme(legend.position = 'none')
# combine
fig1_final = grid.arrange(fig1_t, color_legend, fig1_b, shape_legend, ncol = 2, nrow = 2,
                   widths = c(2,1), heights = c(2,2), 
                   left = yleft, bottom = x)
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
  annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
           hjust = 0, vjust = 1, size = 5)
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
  annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
           hjust = 0, vjust = 1, size = 5)
# ccg-ntill
ccg_ntill_ghg_map = ghg_map_fig(ghg_2050[scenario %in% 'ccg-ntill'])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
           hjust = 0, vjust = 1, size = 5)
# ccl-ntill
ccl_ntill_ghg_map = ghg_map_fig(ghg_2050[scenario %in% 'ccl-ntill'])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
           hjust = 0, vjust = 1, size = 5)

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
  annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
           hjust = 0, vjust = 1, size = 5)
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
  annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
           hjust = 0, vjust = 1, size = 5)
# ccg-ntill
ccg_ntill_y_map   = yield_map_fig(yield_2050[scenario %in% 'ccg-ntill'])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
           hjust = 0, vjust = 1, size = 5)
# ccl-ntill
ccl_ntill_y_map   = yield_map_fig(yield_2050[scenario %in% 'ccl-ntill'])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
           hjust = 0, vjust = 1, size = 5)

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
f_colors = c('Climate'    = "#201158",
             'Management' = "#007054",
             'Site'       = "#C19A1B",
             'Soil'       = "#FFCEF4")

  ## ccg-res ##
# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Crop', 'Water Management', 'Initial Residue Retention Fraction',
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
  )
) +
  ggtitle("(a)")
  ## ccl-res ##
# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`w-w`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Initial Soil Nitrate', 'Nitrogen Inputs', 'Nitrate Fertilizer Fraction',
                'Water Management', 'Crop')
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
  legend.position = 'none'
) +
  ggtitle("(c)")
  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Initial Residue Retention Fraction', 'Water Management', 'Initial Soil Nitrate',
                'Crop', 'Soil Bulk Density')
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
  legend.position = 'none'
) +
  ggtitle("(b)")
  ## ccl-ntill ##
# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`w-w`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Initial Residue Retention Fraction', 'Nitrogen Inputs', 'Soil Bulk Density',
                  'Initial Soil Nitrate', 'Crop')
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
  legend.position = 'none'
) +
  ggtitle("(d)")
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
f_colors = c('Climate'    = "#201158",
             'Management' = "#007054",
             'Site'       = "#C19A1B",
             'Soil'       = "#FFCEF4")

  ## ccg-res ##
# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Crop', 'Water Management', 'Initial Residue Retention Fraction',
                'Initial Soil Nitrate', 'Soil Bulk Density')
# continuous features
# p1
ccg_res_p1  = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[1]),
                       f_colors[[4]], bquote(.(ccg_res_ft[[5]])~'('*g~cm^-3*')'), 'SLBLKD',
                       '(a)')
# p2
# check unit below
ccg_res_p2  = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[2]),
                       f_colors[[3]], bquote(.(ccg_res_ft[[4]])~'('*g~m^-2*')'), 'NITRAT_sum_',
                       '(b)')
# p3
ccg_res_p3 = cont_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[3]),
                      f_colors[[2]], bquote(atop(.(Initial~Residue), Retention~Fraction)), 'res.rtrn.amt', '(c)')
# categorical features
# p4
ccg_res_p4 = cat_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[4]),
                     f_colors[[2]], 'Water Management', 'irr', '(d)')
ccg_res_p4 = ccg_res_p4 + scale_x_discrete(labels = c('Rainfed', 'Irrigated'))
# p5
ccg_res_p5 = cat_pdp(ccg_res_shv, as.character(ccg_res_gg$data$feature[5]),
                     f_colors[[2]], 'Crop', 'crop', '(e)')
ccg_res_p5 = ccg_res_p5 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Initial Residue Retention Fraction', 'Water Management', 'Initial Soil Nitrate',
                  'Crop', 'Soil Bulk Density')
# continuous features
# p1
ccg_ntill_p1  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[1]),
                       f_colors[[4]], bquote(.(ccg_ntill_ft[[5]])~'('*g~cm^-3*')'), 'SLBLKD',
                       '(f)')
# p3
ccg_ntill_p3  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[3]),
                         f_colors[[3]], bquote(.(ccg_ntill_ft[[3]])~'('*g~cm^-2*')'), 'NITRAT_sum_',
                         '(h)')
# p5
ccg_ntill_p5  = cont_pdp(ccg_ntill_shv, as.character(ccg_ntill_gg$data$feature[5]),
                         f_colors[[2]], bquote(atop(.(Initial~Residue), Retention~Fraction)), 
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
                'Water Management', 'Crop')
# categorical features
# p1
ccl_res_p1 = cat_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[1]),
                     f_colors[[2]], 'Crop', 'crop', '(k)')
ccl_res_p1 = ccl_res_p1 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
# p2
ccl_res_p2 = cat_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[2]),
                     f_colors[[2]], 'Water Management', 'irr','(l)')
ccl_res_p2 = ccl_res_p2 + scale_x_discrete(labels = c('Rainfed', 'Irrigated'))
# continuous features
# p3 
ccl_res_p3  = cont_pdp(ccl_res_shv, as.character(ccl_res_gg$data$feature[3]),
                       f_colors[[3]], ccl_res_ft[[3]], 'frac_NO3',
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
ccl_ntill_ft  = c('Initial Residue Retention Fraction', 'Nitrogen Inputs', 'Soil Bulk Density',
                  'Initial Soil Nitrate', 'Crop')
# categorical features
# p1 
ccl_ntill_p1  = cat_pdp(ccl_ntill_shv, as.character(ccl_ntill_gg$data$feature[1]),
                        f_colors[[2]], ccl_ntill_ft[[5]], 'crop',
                        '(p)')
ccl_ntill_p1 = ccl_ntill_p1 + scale_x_discrete(labels = c('Maize', 'Soybean', 'Wheat'))
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
                         f_colors[[2]], bquote(atop(.(Initial~Residue), Retention~Fraction)), 'res.rtrn.amt',
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
# GHG
max_ghg_dt        = fread(paste(data_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential-by-scenario.csv', sep = '/'))
max_ghg_dt[,      goal := 'max-ghg']
max_yield_dt      = fread(paste(data_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential-by-scenario.csv', sep = '/'))
max_yield_dt[,    goal := 'max-yield']
max_ghg_yc_dt     = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential-by-scenario.csv', sep = '/'))
max_ghg_yc_dt[,   goal := 'max-ghg-yc']
max_yield_gc_dt   = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential-by-scenario.csv', sep = '/'))
max_yield_gc_dt[, goal := 'max-yield-gc']
# combine
bmp_ghg = rbind(max_ghg_dt, max_yield_dt,
                max_ghg_yc_dt, max_yield_gc_dt)
setcolorder(bmp_ghg, c('y_block', 'IPCC_NAME', 'goal','scenario'))
setorder(bmp_ghg, y_block, IPCC_NAME)

# YIELD
max_ghg_y_dt        = fread(paste(data_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential-by-scenario.csv', sep = '/'))
max_ghg_y_dt        = max_ghg_y_dt[, -c('m_hectares', 'sd_hectares')]
max_ghg_y_dt[,      goal := 'max-ghg']
max_yield_y_dt      = fread(paste(data_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential-by-scenario.csv', sep = '/'))
max_yield_y_dt      = max_yield_y_dt[, -c('m_hectares', 'sd_hectares')]
max_yield_y_dt[,    goal := 'max-yield']
max_ghg_yc_y_dt     = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential-by-scenario.csv', sep = '/'))
max_ghg_yc_y_dt     = max_ghg_yc_y_dt[, -c('m_hectares', 'sd_hectares')]
max_ghg_yc_y_dt[,   goal := 'max-ghg-yc']
max_yield_gc_y_dt   = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential-by-scenario.csv', sep = '/'))
max_yield_gc_y_dt   = max_yield_gc_y_dt[, -c('m_hectares', 'sd_hectares')]
max_yield_gc_y_dt[, goal:= 'max-yield-gc']
# combine
bmp_yield = rbind(max_ghg_y_dt, max_yield_y_dt,
                  max_ghg_yc_y_dt, max_yield_gc_y_dt)
setcolorder(bmp_yield, c('y_block', 'IPCC_NAME', 'goal','scenario'))
setorder(bmp_yield, y_block, IPCC_NAME)


# join
bmp_dt    = bmp_ghg[bmp_yield, on = .(y_block   = y_block,
                                      IPCC_NAME = IPCC_NAME,
                                      goal      = goal,
                                      scenario  = scenario)]
setcolorder(bmp_dt, c('y_block', 'IPCC_NAME', 'goal', 'scenario',
                      's_GHG', 'sd_s_GHG', 's_grain', 'sd_s_grain'))
# flip ghg sign
bmp_dt[, s_GHG := ifelse(s_GHG < 0, s_GHG*-1, s_GHG*-1)]

# make annual
# N.B. may not need to separate by time
bmp_2050 = bmp_dt[y_block == 2050, lapply(.SD, function(x) {x/35}),
                  .SDcols = c('s_GHG', 'sd_s_GHG','s_grain', 'sd_s_grain'),
                  by = .(y_block, IPCC_NAME, goal, scenario, m_hectares)]
bmp_2100 = bmp_dt[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                  .SDcols = c('s_GHG', 'sd_s_GHG','s_grain', 'sd_s_grain'),
                  by = .(y_block, IPCC_NAME, goal, scenario, m_hectares)]
# recombine
bmp_dt = rbind(bmp_2050, bmp_2100)

# constant
Mg_t_Tg = 1e6

  ## IPCC MAP ##
background_map = IPCC_map(input_p, 'shp/WB_countries_Admin0_10m.shp', 'msw-masked-cropland-rf-ir-area.tif')
ggsave(paste(out_p, 'figure5-map.png', sep = '/'), background_map$IPCC, bg = 'transparent', units = 'mm', width = 180, height = 225, device='png', dpi=500)

  ## NEAR-TERM ##
# ADP
bmp_adp_2050 = bmp_regional_fig(bmp_dt, 'ADP', c('max-ghg', 'max-ghg-yc'), 2050, Mg_t_Tg)
bmp_adp_t = grid.arrange(bmp_adp_2050$GHG, bmp_adp_2050$YIELD, bmp_adp_2050$AREA, nrow = 1)
# ccg-ntill

# AME
bmp_ame_2050 = bmp_regional_fig(bmp_dt, 'AME', c('max-ghg', 'max-ghg-yc'), 2050, Mg_t_Tg)
bmp_ame_t = grid.arrange(bmp_ame_2050$GHG, bmp_ame_2050$YIELD, bmp_ame_2050$AREA, nrow = 1)
# ccl-ntill (switch)

# DEV 
bmp_dev_2050 = bmp_regional_fig(bmp_dt, 'DEV', c('max-ghg', 'max-ghg-yc'), 2050, Mg_t_Tg)
bmp_dev_t = grid.arrange(bmp_dev_2050$GHG, bmp_dev_2050$YIELD, bmp_dev_2050$AREA, nrow = 1)
# ccg-ntill

# EEWCA 
bmp_eewca_2050 = bmp_regional_fig(bmp_dt, 'EEWCA', c('max-ghg', 'max-ghg-yc'), 2050, Mg_t_Tg)
bmp_eewca_t = grid.arrange(bmp_eewca_2050$GHG, bmp_eewca_2050$YIELD, bmp_eewca_2050$AREA, nrow = 1)
# ccl-ntill

# LAC 
bmp_lac_2050 = bmp_regional_fig(bmp_dt, 'LAC', c('max-ghg', 'max-ghg-yc'), 2050, Mg_t_Tg)
bmp_lac_t = grid.arrange(bmp_lac_2050$GHG, bmp_lac_2050$YIELD, bmp_lac_2050$AREA, nrow = 1)
# ccl-ntill (switch)

# save
ggsave(paste(out_p, 'figure5-adp-t.png', sep = '/'), bmp_adp_t, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-ame-t.png', sep = '/'), bmp_ame_t, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-dev-t.png', sep = '/'), bmp_dev_t, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-eewca-t.png', sep = '/'), bmp_eewca_t, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-lac-t.png', sep = '/'), bmp_lac_t, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)

  ## MEDIUM-TERM ##
# ADP
bmp_adp_2100 = bmp_regional_fig(bmp_dt, 'ADP', c('max-ghg', 'max-ghg-yc'), 2100, Mg_t_Tg)
bmp_adp_b = grid.arrange(bmp_adp_2100$GHG, bmp_adp_2100$YIELD, bmp_adp_2100$AREA, nrow = 1)
# ccg-ntill

# AME
bmp_ame_2100 = bmp_regional_fig(bmp_dt, 'AME', c('max-ghg', 'max-ghg-yc'), 2100, Mg_t_Tg)
bmp_ame_b = grid.arrange(bmp_ame_2100$GHG, bmp_ame_2100$YIELD, bmp_ame_2100$AREA, nrow = 1)
# ccg-ntill (switch)

# DEV 
bmp_dev_2100 = bmp_regional_fig(bmp_dt, 'DEV', c('max-ghg', 'max-ghg-yc'), 2100, Mg_t_Tg)
bmp_dev_b = grid.arrange(bmp_dev_2100$GHG, bmp_dev_2100$YIELD, bmp_dev_2100$AREA, nrow = 1)
# ccg-ntill

# EEWCA
bmp_eewca_2100 = bmp_regional_fig(bmp_dt, 'EEWCA', c('max-ghg', 'max-ghg-yc'), 2100, Mg_t_Tg)
bmp_eewca_b = grid.arrange(bmp_eewca_2100$GHG, bmp_eewca_2100$YIELD, bmp_eewca_2100$AREA, nrow = 1)
# ccl-ntill

# LAC
bmp_lac_2100 = bmp_regional_fig(bmp_dt, 'LAC', c('max-ghg', 'max-ghg-yc'), 2100, Mg_t_Tg)
bmp_lac_b = grid.arrange(bmp_lac_2100$GHG, bmp_lac_2100$YIELD, bmp_lac_2100$AREA, nrow = 1)
# ccg-ntill (switch)

ggsave(paste(out_p, 'figure5-adp-b.png', sep = '/'), bmp_adp_b, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-ame-b.png', sep = '/'), bmp_ame_b, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-dev-b.png', sep = '/'), bmp_dev_b, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-eewca-b.png', sep = '/'), bmp_eewca_b, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
ggsave(paste(out_p, 'figure5-lac-b.png', sep = '/'), bmp_lac_b, bg = 'transparent', units = 'mm', width = 65, height = 22, device='png', dpi=300)
