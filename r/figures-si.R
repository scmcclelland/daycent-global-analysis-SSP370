# filename:    figures-si.R
# created:     19 December 2025
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file creates figures included in the SI of the manuscript.
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
out_p   = paste(dir, 'figures/si', sep = '/')
#-----------------------------------------------------------------------------------------
# Figure S1-S4. Hectare responses
#-----------------------------------------------------------------------------------------
# GHG
ghg_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-ghg-responses.csv', sep = '/'))
ghg_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-ghg-responses.csv', sep = '/'))
ghg_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-ghg-responses.csv', sep = '/'))
ghg_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-ghg-responses.csv', sep = '/'))

# combine
ghg = rbind(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)
rm(ghg_ccg_res, ghg_ccl_res, ghg_ccg_ntill, ghg_ccl_ntill)

# flip signs for ghg
ghg = ghg[, d_s_SOC := ifelse(d_s_SOC < 0, d_s_SOC*-1, d_s_SOC*-1)]
ghg = ghg[, d_s_N2O := ifelse(d_s_N2O < 0, d_s_N2O*-1, d_s_N2O*-1)]
ghg = ghg[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]

# YIELD
yield_ccg_res   = fread(paste(data_p, 'ccg-res-hectare-yield-responses.csv', sep = '/'))
yield_ccl_res   = fread(paste(data_p, 'ccl-res-hectare-yield-responses.csv', sep = '/'))
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-hectare-yield-responses.csv', sep = '/'))
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-hectare-yield-responses.csv', sep = '/'))

# combine
yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)

# join ghg and yield
data = ghg[yield, on = .(scenario = scenario,
                         y_block = y_block,
                         IPCC_NAME = IPCC_NAME)]

# make annual, Mg CO2-eq ha-1 yr-1
data_2050 = data[y_block == 2050, lapply(.SD, function(x) {x/35}), 
                 .SDcols = c('d_s_SOC', 'd_s_N2O','d_s_GHG', 'd_s_cgrain',
                             'se_s_SOC', 'se_s_N2O', 'se_s_GHG', 'se_s_cgrain'),
                 by = .(scenario, y_block, IPCC_NAME)]
data_2100 = data[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                 .SDcols = c('d_s_SOC', 'd_s_N2O','d_s_GHG', 'd_s_cgrain',
                             'se_s_SOC', 'se_s_N2O', 'se_s_GHG', 'se_s_cgrain'),
                 by = .(scenario, y_block, IPCC_NAME)]
# create barplot, left
fig1_l    = barplot_fig(data_2050)
# create barplot, right
fig1_r    = barplot_fig(data_2100)

# add figure labels
fig1_l$soc = fig1_l$soc + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Near-term, 2016-2050", "(a)")
fig1_l$n2o = fig1_l$n2o + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Near-term, 2016-2050", "(a)")

fig1_l$ghg = fig1_l$ghg + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Near-term, 2016-2050", "(a)")

fig1_l$yield = fig1_l$yield + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Near-term, 2016-2050", "(a)")

fig1_r$soc = fig1_r$soc + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Medium-term, 2016-2100", "(b)")
fig1_r$n2o = fig1_r$n2o + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Medium-term, 2016-2100", "(b)")

fig1_r$ghg = fig1_r$ghg + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Medium-term, 2016-2100", "(b)")

fig1_r$yield = fig1_r$yield + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Medium-term, 2016-2100", "(b)")

# combine
fig1_l$soc = fig1_l$soc + theme(legend.position = 'none')
fig1_r$soc = fig1_r$soc + theme(legend.position = 'right', 
                                legend.box.just = "left",
                                legend.justification = "left",)
legend     = get_legend(fig1_r$soc)
fig1_final = fig1_l$soc + fig1_r$soc + legend +
  plot_layout(ncol = 3, widths = c(0.45, 0.45, 0.1), guides = 'collect') &
  theme(legend.position = 'none')
fig1_final

fig2_final = fig1_l$n2o + fig1_r$n2o + legend +
  plot_layout(ncol = 3, widths = c(0.45, 0.45, 0.1), guides = 'collect') &
  theme(legend.position = 'none')
fig2_final

fig3_final = fig1_l$ghg + fig1_r$ghg + legend +
  plot_layout(ncol = 3, widths = c(0.45, 0.45, 0.1), guides = 'collect') &
  theme(legend.position = 'none')
fig3_final

fig4_final = fig1_l$yield + fig1_r$yield + legend +
  plot_layout(ncol = 3, widths = c(0.45, 0.45, 0.1), guides = 'collect') &
  theme(legend.position = 'none')
fig4_final

ggsave(paste(out_p, 'figure1-si.pdf', sep = '/'), fig1_final, units = 'mm', width = 180, height = 100, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure2-si.pdf', sep = '/'), fig2_final, units = 'mm', width = 180, height = 100, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure3-si.pdf', sep = '/'), fig3_final, units = 'mm', width = 180, height = 100, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure4-si.pdf', sep = '/'), fig4_final, units = 'mm', width = 180, height = 100, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure S5-S6. CoV maps (2050, 2100)
#-----------------------------------------------------------------------------------------
# express as % (SD/abs(mean))*100
# plot 0 to > 100

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
ghg = ghg[, d_s_GHG := ifelse(d_s_GHG < 0, d_s_GHG*-1, d_s_GHG*-1)]

# keep only ghg
ghg = ghg[, -c('d_s_SOC', 'sd_s_SOC', 'd_s_N2O', 'sd_s_N2O')]

# add xy coordinates
## input table ##
load(paste(input_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep coordinates
main_table      = main_table[, c('gridid', 'x', 'y')]
main_table      = unique(main_table)
# join
ghg = ghg[main_table, on = .(gridid = gridid)]
ghg = ghg[!is.na(scenario)]

# estimate CoV as percent
ghg[, cov_s_GHG := (sd_s_GHG/abs(d_s_GHG))*100]

  ## 2050 ##
# ccg-res
ccg_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccg-res' &
                                           y_block == 2050])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC')
# ccl-res
ccl_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccl-res' &
                                          y_block == 2050])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC')
# ccg-ntill
ccg_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccg-ntill' &
                                      y_block == 2050])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC + Ntill')
# ccl-ntill
ccl_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccl-ntill' &
                                      y_block == 2050])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC + Ntill')

# YIELD
# load data | N.B. does not include imputed values 
yield_ccg_res   = fread(paste(data_p, 'ccg-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_res   = fread(paste(data_p, 'ccl-res-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccg_ntill = fread(paste(data_p, 'ccg-ntill-weighted-gridid-yield-responses.csv', sep = '/'))
yield_ccl_ntill = fread(paste(data_p, 'ccl-ntill-weighted-gridid-yield-responses.csv', sep = '/'))

# combine
yield = rbind(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)
rm(yield_ccg_res, yield_ccl_res, yield_ccg_ntill, yield_ccl_ntill)

# add xy coordinates
# join
yield = yield[main_table, on = .(gridid = gridid)]
yield = yield[!is.na(scenario)]

# estimate CoV as percent
yield[, cov_s_cgrain := (sd_s_cgrain/abs(d_s_cgrain))*100]

  ## 2050 ##
# ccg-res
ccg_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccg-res' &
                                                   y_block == 2050,])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccl-res
ccl_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccl-res' & 
                                              y_block == 2050])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccg-ntill
ccg_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccg-ntill' &
                                              y_block == 2050])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccl-ntill
ccl_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccl-ntill' &
                                              y_block == 2050])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
           hjust = 0, vjust = 1, size = 4)

# Multi-panel figure
figs5_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.15), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure5-si.pdf', sep = '/'), figs5_maps,  units = 'mm', width = 180, height = 225, device='pdf', dpi=300)

  ## 2100 ##
# GHG
# ccg-res
ccg_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccg-res' &
                                          y_block == 2100])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC')
# ccl-res
ccl_res_ghg_map   = cov_ghg_map_fig(ghg[scenario %in% 'ccl-res' &
                                          y_block == 2100])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC')
# ccg-ntill
ccg_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccg-ntill' &
                                          y_block == 2100])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Grass CC + Ntill')
# ccl-ntill
ccl_ntill_ghg_map = cov_ghg_map_fig(ghg[scenario %in% 'ccl-ntill' &
                                          y_block == 2100])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
           hjust = 0, vjust = 1, size = 4) +
  ggtitle('Legume CC + Ntill')
# YIELD
# ccg-res
ccg_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccg-res' &
                                              y_block == 2100,])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccl-res
ccl_res_y_map     = cov_yield_map_fig(yield[scenario %in% 'ccl-res' & 
                                              y_block == 2100])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccg-ntill
ccg_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccg-ntill' &
                                              y_block == 2100])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
           hjust = 0, vjust = 1, size = 4)
# ccl-ntill
ccl_ntill_y_map   = cov_yield_map_fig(yield[scenario %in% 'ccl-ntill' &
                                              y_block == 2100])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
           hjust = 0, vjust = 1, size = 4)

# Multi-panel figure
figs6_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.15), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure6-si.pdf', sep = '/'), figs6_maps,  units = 'mm', width = 180, height = 225, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure S7. Top features, climate-favorable, yield-favorable, neither favorable
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
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`w-l`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Initial Soil Nitrate', 'Nitrogen Inputs', 'Soil Bulk Density',
                'Initial Residue Fraction', 'Cash Crop')
# types (actual order)
ccg_res_t   = c('Management', 'Management', 'Soil', 'Management', 'Site')
# plot
ccg_res_fig_wl = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                        ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_wl = ccg_res_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Grass CC","(a)")

# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`l-w`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Water Management', 'Soil Bulk Density', 'Initial Residue Fraction',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccg_res_t   = c('Management', 'Management', 'Management', 'Soil', 'Management')
# plot
ccg_res_fig_lw = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                           ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_lw = ccg_res_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(b)")

# extract data and initial visualization
ccg_res_shv = shapviz(ccg_res_SHAP$class_SHAP$`l-l`)
ccg_res_gg  = sv_importance(ccg_res_shv)
# features (reverse order)
ccg_res_ft  = c('Initial Residue Fraction', 'Water Management', 'Nitrogen Inputs',
                'Initial Soil Nitrate', 'Soil Bulk Density')
# types (actual order)
ccg_res_t   = c('Soil', 'Site', 'Management', 'Management', 'Management')
# plot
ccg_res_fig_ll = feature_p(ccg_res_ft, ccg_res_gg$data$feature[1:5],
                           ccg_res_gg$data$value[1:5], f_colors, ccg_res_t)
ccg_res_fig_ll = ccg_res_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(c)")

  ## ccg-ntill ##
# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`w-l`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Nitrogen Inputs', 'Soil Bulk Density', 'Initial Soil Nitrate',
                'Initial Residue Fraction', 'Cash Crop')
# types (actual order)
ccg_ntill_t   = c('Management', 'Management', 'Site', 'Soil', 'Management')
# plot
ccg_ntill_fig_wl = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_wl = ccg_ntill_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Grass CC + Ntill","(d)")

# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`l-w`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Mean Diurnal Range', 'Initial Soil Nitrate', 'Initial Residue Fraction',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccg_ntill_t   = c('Management', 'Management', 'Management', 'Site', 'Climate')
# plot
ccg_ntill_fig_lw = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_lw = ccg_ntill_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(e)")

# extract data and initial visualization
ccg_ntill_shv = shapviz(ccg_ntill_SHAP$class_SHAP$`l-l`)
ccg_ntill_gg  = sv_importance(ccg_ntill_shv)
# features (reverse order)
ccg_ntill_ft  = c('Soil pH', 'Initial Residue Fraction', 'Initial Soil Water Content',
                'Cash Crop', 'Soil Bulk Density')
# types (actual order)
ccg_ntill_t   = c('Soil', 'Management', 'Site', 'Management', 'Soil')
# plot
ccg_ntill_fig_ll = feature_p(ccg_ntill_ft, ccg_ntill_gg$data$feature[1:5],
                           ccg_ntill_gg$data$value[1:5], f_colors, ccg_ntill_t)
ccg_ntill_fig_ll = ccg_ntill_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(f)")

  ## ccl-res ##
# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`w-l`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Soil Bulk Density', 'Soil pH', 'Nitrogen Inputs',
                'Initial Soil Nitrate', 'Cash Crop')
# types (actual order)
ccl_res_t   = c('Management', 'Site', 'Management', 'Soil', 'Soil')
# plot
ccl_res_fig_wl = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_wl = ccl_res_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Legume CC","(g)")

# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`l-w`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Initial Relative Soil Water', 'Water Management', 'Initial Soil Nitrate',
                'Nitrogen Inputs', 'Cash Crop')
# types (actual order)
ccl_res_t   = c('Management', 'Management', 'Site', 'Management', 'Site')
# plot
ccl_res_fig_lw = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_lw = ccl_res_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(h)")

# extract data and initial visualization
ccl_res_shv = shapviz(ccl_res_SHAP$class_SHAP$`l-l`)
ccl_res_gg  = sv_importance(ccl_res_shv)
# features (reverse order)
ccl_res_ft  = c('Soil Bulk Density', 'Cash Crop', 'Water Management',
                'Initial Soil Nitrate', 'Nitrogen Inputs')
# types (actual order)
ccl_res_t   = c('Management', 'Site', 'Management', 'Management', 'Soil')
# plot
ccl_res_fig_ll = feature_p(ccl_res_ft, ccl_res_gg$data$feature[1:5],
                           ccl_res_gg$data$value[1:5], f_colors, ccl_res_t)
ccl_res_fig_ll = ccl_res_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(i)")

  ## ccl-ntill ##
# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`w-l`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Water Management', 'Initial Residue Fraction', 'Nitrogen Inputs',
                  'Initial Soil Nitrate', 'Cash Crop')
# types (actual order)
ccl_ntill_t   = c('Management', 'Site', 'Management', 'Management', 'Management')
# plot
ccl_ntill_fig_wl = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_wl = ccl_ntill_fig_wl + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("Legume CC + Ntill", "(j)")

# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`l-w`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Mean Diurnal Range', 'Initial Residue Return', 'Initial Soil Nitrate',
                  'Nitrogen Inputs', 'Cash rop')
# types (actual order)
ccl_ntill_t   = c('Management', 'Management', 'Site', 'Management', 'Climate')
# plot
ccl_ntill_fig_lw = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_lw = ccl_ntill_fig_lw + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("", "(k)")

# extract data and initial visualization
ccl_ntill_shv = shapviz(ccl_ntill_SHAP$class_SHAP$`l-l`)
ccl_ntill_gg  = sv_importance(ccl_ntill_shv)
# features (reverse order)
ccl_ntill_ft  = c('Initial Soil Nitrate', 'Cash Crop', 'Nitrogen Inputs',
                  'Initial Soil Water', 'Soil Bulk Density')
# types (actual order)
ccl_ntill_t   = c('Soil', 'Site', 'Management', 'Management', 'Site')
# plot
ccl_ntill_fig_ll = feature_p(ccl_ntill_ft, ccl_ntill_gg$data$feature[1:5],
                             ccl_ntill_gg$data$value[1:5], f_colors, ccl_ntill_t)
ccl_ntill_fig_ll = ccl_ntill_fig_ll + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 7       # Match your other text size if needed
  )
) +
  ggtitle("","(l)")

fig7_final = ggarrange(ccg_res_fig_wl, ccg_res_fig_lw, ccg_res_fig_ll, 
                       ccg_ntill_fig_wl, ccg_ntill_fig_lw, ccg_ntill_fig_ll, 
                       ccl_res_fig_wl, ccl_res_fig_lw, ccl_res_fig_ll, 
                       ccl_ntill_fig_wl, ccl_ntill_fig_lw, ccl_ntill_fig_ll, 
                       ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
# save
ggsave(paste(out_p, 'figure7-si.pdf', sep = '/'), fig7_final, units = 'mm', width = 180, height = 225, device='pdf', dpi=300)
