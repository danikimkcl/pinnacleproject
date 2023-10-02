########## Script to create supplementary figure:
########## Associations between PD (excluding self-reported dx) and outcomes

# Libraries

library(cowplot)

# Open RDS objects 

plot1 = readRDS(here::here("Output", "PD outcomes paper", "plots", "temp_plot_JorgePD_communityHSU.rds"))
plot2 = readRDS(here::here("Output", "PD outcomes paper", "plots", "temp_plot_JorgePD_hospitaladm.rds"))
plot3 = readRDS(here::here("Output", "PD outcomes paper", "plots", "temp_plot_JorgePD_care.rds"))
plot4 = readRDS(here::here("Output", "PD outcomes paper", "plots", "temp_plot_JorgePD_died.rds"))

# Combine plots

plot_combined_adj = cowplot::plot_grid(plot1, plot2, plot3, plot4,
                                       align = "v",
                                       labels = "AUTO", 
                                       scale = 0.5,
                                       ncol = 1)

plot_combined_adj

# Save plot

ggsave(plot = plot_combined_adj, filename = here::here("Output", "PD outcomes paper", "plots", "supplementary.png"), width = 18, height = 27, units = "cm")
