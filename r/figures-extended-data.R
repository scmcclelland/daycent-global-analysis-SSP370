# filename:    figures-extended-data.R
# created:     19 December 2024
# updated:     21 February 2025
# author:      S.C. McClelland
# description: This file creates figures included in the extended data section of manuscript.
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
out_p   = paste(dir, 'figures/ext', sep = '/')
#-----------------------------------------------------------------------------------------
# Figure 1. Multi-panel maps | Medium-term
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
ghg_2100 = ghg[y_block == 2100, lapply(.SD, function(x) {x/85}), 
               .SDcols = c('d_s_SOC', 'd_s_N2O','d_s_GHG'),
               by = .(scenario, y_block, gridid)]

# ccg-res
ccg_res_ghg_map   = ghg_map_fig(ghg_2100[scenario %in% 'ccg-res'])
ccg_res_ghg_map$GHG = ccg_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(a)", # top left
           hjust = 0, vjust = 1, size = 3) +
  ggtitle('Grass CC')
ccg_res_ghg_map$legend1 = ccg_res_ghg_map$legend1 +
  theme(
    plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
    # Negative values will reduce the padding
    # Format is (top, right, bottom, left)
  ) +
  # scale_x_continuous(expand = c(0, 0)) +  # Remove expansion on x axis
  scale_y_continuous(expand = c(0, 0))    # Remove expansion on y axis
# ccl-res
ccl_res_ghg_map   = ghg_map_fig(ghg_2100[scenario %in% 'ccl-res'])
ccl_res_ghg_map$GHG = ccl_res_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(e)", # top left
           hjust = 0, vjust = 1, size = 3) +
  ggtitle('Legume CC')
# ccg-ntill
ccg_ntill_ghg_map = ghg_map_fig(ghg_2100[scenario %in% 'ccg-ntill'])
ccg_ntill_ghg_map$GHG = ccg_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(c)", # top left
           hjust = 0, vjust = 1, size = 3) +
  ggtitle('Grass CC + Ntill')
# ccl-ntill
ccl_ntill_ghg_map = ghg_map_fig(ghg_2100[scenario %in% 'ccl-ntill'])
ccl_ntill_ghg_map$GHG = ccl_ntill_ghg_map$GHG +
  annotate("text", x = -Inf, y = Inf, label = "(g)", # top left
           hjust = 0, vjust = 1, size = 3) +
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

yield = yield[, -c('sd_s_cgrain')]

# add xy coordinates
# join
yield = yield[main_table, on = .(gridid = gridid)]
yield = yield[!is.na(scenario)]

# make annual, Mg ha-1 yr-1
yield_2100 = yield[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                   .SDcols = c('d_s_cgrain'),
                   by = .(scenario, y_block, gridid)]

# ccg-res
ccg_res_y_map     = yield_map_fig(yield_2100[scenario %in% 'ccg-res'])
ccg_res_y_map$grain = ccg_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(b)", # top left
           hjust = 0, vjust = 1, size = 3) 
ccg_res_y_map$legend2 = ccg_res_y_map$legend2 +
  theme(
    plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
    # Negative values will reduce the padding
    # Format is (top, right, bottom, left)
  ) +
  # scale_x_continuous(expand = c(0, 0)) +  # Remove expansion on x axis
  scale_y_continuous(expand = c(0, 0))    # Remove expansion on y axis
# ccl-res
ccl_res_y_map     = yield_map_fig(yield_2100[scenario %in% 'ccl-res'])
ccl_res_y_map$grain = ccl_res_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(f)", # top left
           hjust = 0, vjust = 1, size = 3) 
# ccg-ntill
ccg_ntill_y_map   = yield_map_fig(yield_2100[scenario %in% 'ccg-ntill'])
ccg_ntill_y_map$grain = ccg_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(d)", # top left
           hjust = 0, vjust = 1, size = 3)
# ccl-ntill
ccl_ntill_y_map   = yield_map_fig(yield_2100[scenario %in% 'ccl-ntill'])
ccl_ntill_y_map$grain = ccl_ntill_y_map$grain +
  annotate("text", x = -Inf, y = Inf, label = "(h)", # top left
           hjust = 0, vjust = 1, size = 3)

# Multi-panel figure
fig1_maps = ccg_res_ghg_map$GHG + ccg_res_y_map$grain + 
  ccg_ntill_ghg_map$GHG + ccg_ntill_y_map$grain +
  ccl_res_ghg_map$GHG   + ccl_res_y_map$grain  + 
  ccl_ntill_ghg_map$GHG + ccl_ntill_y_map$grain +
  ccg_res_ghg_map$legend1 + ccg_res_y_map$legend2 +
  plot_layout(ncol = 2, heights = c(0.225, 0.225, 0.225, 0.225, 0.10), guides = 'collect') &
  theme(legend.position = 'none')

# Save
ggsave(paste(out_p, 'figure1-ext.pdf', sep = '/'), fig1_maps,  units = 'mm', width = 180, height = 210, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 2. Two-panel scatterplot | Balancing outcomes global potential
#-----------------------------------------------------------------------------------------
# GHG
max_ghg_dt        = fread(paste(data_p, 'balanced-outcomes-max-ghg-no-yield-mitigation-potential.csv', sep = '/'))
max_ghg_dt        = max_ghg_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_ghg_dt[,      goal := 'max-ghg']
max_yield_dt      = fread(paste(data_p, 'balanced-outcomes-max-yield-no-ghg-mitigation-potential.csv', sep = '/'))
max_yield_dt      = max_yield_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_yield_dt[,    goal := 'max-yield']
max_ghg_yc_dt     = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-mitigation-potential.csv', sep = '/'))
max_ghg_yc_dt     = max_ghg_yc_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_ghg_yc_dt[,   goal := 'max-ghg-yc']
max_yield_gc_dt   = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-mitigation-potential.csv', sep = '/'))
max_yield_gc_dt   = max_yield_gc_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_yield_gc_dt[, goal := 'max-yield-gc']
# combine
bmp_ghg = rbind(max_ghg_dt, max_yield_dt,
                  max_ghg_yc_dt, max_yield_gc_dt)

# YIELD
max_ghg_y_dt        = fread(paste(data_p, 'balanced-outcomes-max-ghg-no-yield-crop-potential.csv', sep = '/'))
max_ghg_y_dt        = max_ghg_y_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_ghg_y_dt[,      goal := 'max-ghg']
max_yield_y_dt      = fread(paste(data_p, 'balanced-outcomes-max-yield-no-ghg-crop-potential.csv', sep = '/'))
max_yield_y_dt      = max_yield_y_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_yield_y_dt[,    goal := 'max-yield']
max_ghg_yc_y_dt     = fread(paste(data_p, 'balanced-outcomes-max-ghg-yield-constrained-crop-potential.csv', sep = '/'))
max_ghg_yc_y_dt     = max_ghg_yc_y_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_ghg_yc_y_dt[,   goal := 'max-ghg-yc']
max_yield_gc_y_dt   = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential.csv', sep = '/'))
max_yield_gc_y_dt   = max_yield_gc_y_dt[IPCC_NAME %in% 'GLB', -c('m_hectares', 'se_hectares')]
max_yield_gc_y_dt[, goal:= 'max-yield-gc']
# combine
bmp_yield = rbind(max_ghg_y_dt, max_yield_y_dt,
                  max_ghg_yc_y_dt, max_yield_gc_y_dt)

# join
bmp_dt    = bmp_ghg[bmp_yield, on = .(y_block   = y_block,
                                      IPCC_NAME = IPCC_NAME,
                                      goal      = goal)]
setcolorder(bmp_dt, c('y_block', 'IPCC_NAME', 'goal'))
# flip ghg sign
bmp_dt[, s_GHG := ifelse(s_GHG < 0, s_GHG*-1, s_GHG*-1)]

# make annual
bmp_2050 = bmp_dt[y_block == 2050, lapply(.SD, function(x) {x/35}), 
                  .SDcols = c('s_GHG', 'se_s_GHG','s_grain', 'se_s_grain'),
                  by = .(y_block, IPCC_NAME, goal)]
bmp_2100 = bmp_dt[y_block == 2100, lapply(.SD, function(x) {x/85}), 
                  .SDcols = c('s_GHG', 'se_s_GHG','s_grain', 'se_s_grain'),
                  by = .(y_block, IPCC_NAME, goal)]
# constant
Mg_t_Pg = 1e9

# plot
fig2_t = bmp_scatterplot_fig(bmp_2050, Mg_t_Pg)
fig2_b = bmp_scatterplot_fig(bmp_2100, Mg_t_Pg)
# plot adjustments
# update legends, axes, and add labels to corners
fig2_t = fig2_t + 
  # Top left
  annotate("text", x = -0.5, y = 0.5, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 1.25, y = 0.5, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -0.5, y = -0.25, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 1.25, y = -0.25, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add a label
fig2_t = fig2_t + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 9       # Match your other text size if needed
  )
) +
  ggtitle('Global, near-term (2016-2050)', '(a)')
fig2_b = fig2_b + 
  # Top left
  annotate("text", x = -0.5, y = 0.5, label = "Yield-favorable", 
           hjust = 0, vjust = 1, size = 2.5) +
  
  # Top right
  annotate("text", x = 1.25, y = 0.5, label = "Both favorable", 
           hjust = 1, vjust = 1, size = 2.5) +
  
  # Bottom left
  annotate("text", x = -0.5, y = -0.25, label = "Both unfavorable", 
           hjust = 0, vjust = 0, size = 2.5) +
  
  # Bottom right
  annotate("text", x = 1.25, y = -0.25, label = "Mitigation-favorable", 
           hjust = 1, vjust = 0, size = 2.5)
# add b label
fig2_b = fig2_b + theme(
  plot.title.position = "plot",  # This moves the title to align with plot edge
  plot.title = element_text(
    hjust = -0.01,  # Slight adjustment left of the plot
    vjust = -0.5,   # Slight adjustment above the plot
    size = 9       # Match your other text size if needed
  )
) +
  ggtitle('Global, medium-term (2016-2100)', '(b)')
color_legend = get_legend(fig2_b)
plot(color_legend)
fig2_t = fig2_t + theme(legend.position = 'none')
fig2_b = fig2_b + theme(legend.position = 'none')
# combine
fig2_final = grid.arrange(fig2_t, color_legend, fig2_b, ncol = 2, nrow = 2,
                          heights = c(2,2), widths = c(2,1))

# save
ggsave(paste(out_p, 'figure2-ext.pdf', sep = '/'), fig2_final, units = 'mm', width = 180, height = 185, device='pdf', dpi=300)
#-----------------------------------------------------------------------------------------
# Figure 3. Recommended practices by region (Yield focus)
#-----------------------------------------------------------------------------------------
# N.B. these plots are used in a pptx file to create the final figure
# load data
max_yield_gc_dt = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-practices.csv', sep = '/'))

# add xy coordinates
## input table ##
load(paste(input_p, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
# keep coordinates
main_table      = main_table[, c('gridid', 'x', 'y')]
main_table      = unique(main_table)
# join
max_yield_gc_dt = max_yield_gc_dt[main_table, on = .(gridid = gridid)]
max_yield_gc_dt = max_yield_gc_dt[!is.na(scenario)]

# 2050
max_yield_gc_2050_dt = max_yield_gc_dt[y_block %in% 2050,]

# 2100
max_yield_gc_2100_dt = max_yield_gc_dt[y_block %in% 2100,]

# max yield (ghg constrained) maps
# 2050
max_yield_gc_2050_map = bmp_map(max_yield_gc_2050_dt)
max_yield_gc_2050_map$bmp = max_yield_gc_2050_map$bmp + theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
)
# 2100
max_yield_gc_2100_map = bmp_map(max_yield_gc_2100_dt)
max_yield_gc_2100_map$bmp = max_yield_gc_2100_map$bmp + theme(
  plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
  # Negative values will reduce the padding
  # Format is (top, right, bottom, left)
)

  ## IPCC MAP ##
# background_map = IPCC_map(input_p, 'shp/WB_countries_Admin0_10m.shp', 'msw-masked-cropland-rf-ir-area.tif')
# background_map$IPCC = background_map$IPCC + theme(
#   plot.margin = unit(c(-1, -0.5, -1, -0.5), "cm"),  # Adjust these values as needed
#   # Negative values will reduce the padding
#   # Format is (top, right, bottom, left)
# )
# Save
ggsave(paste(out_p, 'figure3-near.pdf', sep = '/'), max_yield_gc_2050_map$bmp,    units = 'mm', width = 60, height = 30, device='pdf', dpi=300)
ggsave(paste(out_p, 'figure3-medium.pdf', sep = '/'), max_yield_gc_2100_map$bmp,  units = 'mm', width = 60, height = 30, device='pdf', dpi=300)
# ggsave(paste(out_p, 'figure3-map.pdf', sep = '/'), background_map$IPCC, bg = 'transparent', units = 'mm', width = 180, height = 160, device='pdf', dpi=500)

# Estimate cropland area % by region, scenario
# GHG
s_max_yield_gc_dt     = fread(paste(data_p, 'balanced-outcomes-max-yield-ghg-constrained-crop-potential-by-scenario.csv', sep = '/'))
s_max_yield_gc_dt[, t_hectares := lapply(.SD, sum), .SDcols = 'm_hectares', by = .(y_block, IPCC_NAME)]
setorder(s_max_yield_gc_dt, y_block, IPCC_NAME)
s_max_yield_gc_dt[, p_hectares := (m_hectares/t_hectares)*100]
s_max_yield_gc_dt = s_max_yield_gc_dt[, c('y_block', 'IPCC_NAME', 'scenario', 'p_hectares')]
