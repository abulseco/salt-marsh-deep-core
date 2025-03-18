# Organic Matter Figure
# %C, N, LOI, BD, C13, FT-IR, NMR for six cores

library(ggplot2)
library(plyr) 
library(paletteer)
library(viridis)
library(patchwork)
library(here)

theme.geochem <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 14, color="black"),
          axis.text.y=element_text(size = 14, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=20),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"), 
          legend.position = "none")
}

theme.geochem.legend <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 14, color="black"),
          axis.text.y=element_text(size = 14, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=20),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"))
}

# upload necessary data files----
## BD, LOI----
bulk_LOI <- read.csv("deepcore_bd_LOI.csv", header = T)

## NMR pred----
nmr_data_predicted <- read.csv("predicted-nmr-mixed-model.csv", header = T)

## %C, %N, C:N----
deep_all_geochem <- read.csv("deepcore_geochem_ALL.csv") 

## C-13
carbon13 <- read.csv("13C_deepcore.csv")

# # Plots by depth----
# ## Bulk Density----
# my_colors <- c("#fdc527", "#fe9f6d", "#de4968", "#2a788e", "#414487", "#440154")
# 
# BD_plot <- ggplot(bulk_LOI, aes(y = Bulk_Density, x = Depth)) + 
#   geom_line(aes(color = Core), linewidth = 1.0) +  # Adjust line thickness if needed
#   geom_point(aes(fill = Core), shape = 21, color = "black", size = 3, stroke = 0.5) +  # Black outline for points
#   scale_color_manual(values = my_colors) +
#   scale_fill_manual(values = my_colors) +
#   scale_y_continuous(name = "Bulk Density", position = "right") +
#   xlab("Depth (cm)") +
#   theme.geochem.legend() +
#   coord_flip() +
#   scale_x_reverse() +
#   theme(axis.title.y = element_blank()) +
#   facet_wrap(~Site)
# BD_plot
# 
# ## LOI----
# LOI_plot <- ggplot(bulk_LOI, aes(y = LOI, x = Depth)) + 
#   geom_line(aes(color = Core), linewidth = 1.0) +  # Adjust line thickness if needed
#   geom_point(aes(fill = Core), shape = 21, color = "black", size = 3, stroke = 0.5) +  # Black outline for points
#   scale_color_manual(values = my_colors) +
#   scale_fill_manual(values = my_colors) +
#   scale_y_continuous(name="Loss-On-Ignition",position = "right") +
#   xlab("Depth (cm)") +
#   coord_flip() +
#   scale_x_reverse() +
#   theme.geochem() +
#   theme(axis.title.y = element_blank()) +
#   facet_wrap(~Site) 
# LOI_plot
# 
# ## C:N Ratio----
# CN_plot <- ggplot(deep_all_geochem, aes(y = CN, x = Depth)) + 
#   geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
#   geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 3, stroke = 0.5) +  # Black outline for points
#   scale_color_manual(values = my_colors) +
#   scale_fill_manual(values = my_colors) +
#   scale_y_continuous(name="C:N Ratio",position = "right") +
#   xlab("Depth (cm)") +
#   coord_flip() +
#   scale_x_reverse() +
#   theme.geochem() +
#   # theme(axis.title.y = element_blank()) +
#   facet_wrap(~Site)
# CN_plot
# 
# ## C13----
# my_colors_2 <- c("#fdc527", "#414487")
# 
# C13_plot <- ggplot(carbon13, aes(y = C13, x = Depth)) +
#   geom_line(aes(color = Site), linewidth = 1.0) +  # Adjust line thickness if needed
#   geom_point(aes(fill = Site), shape = 21, color = "black", size = 3, stroke = 0.5) +  # Black outline for points
#   scale_color_manual(values = my_colors_2) +
#   scale_fill_manual(values = my_colors_2) +
#   scale_y_continuous(name = expression(delta^13*C), position = "right") +
#   xlab("Depth (cm)") +
#   coord_flip() +
#   scale_x_reverse() +
#   theme.geochem() +
#   theme(axis.title.y = element_blank()) +
#   facet_wrap(~Site) 
# C13_plot
# 
# ## Combining plots----
# OM_combined_plot <- (BD_plot | LOI_plot) / (CN_plot | C13_plot) + 
#   plot_layout(guides = "collect") 
# OM_combined_plot

# Plots by Year----
## Bulk Density----
my_colors <- c("#fdc527", "#fe9f6d", "#de4968", "#2a788e", "#414487", "#440154")

BD_plot <- ggplot(bulk_LOI, aes(y = Bulk_Density, x = Years_CE)) + 
  geom_line(aes(color = Core), linewidth = 0.75) +  # Adjust line thickness if needed
  geom_point(aes(fill = Core), shape = 21, color = "black", size = 1, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name = "Bulk Density", position = "right") +
  xlab("Year (CE)") +
  theme.geochem.legend() +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~Site)
BD_plot

## LOI----
LOI_plot <- ggplot(bulk_LOI, aes(y = LOI, x = Years_CE)) + 
  geom_line(aes(color = Core), linewidth = 0.75) +  # Adjust line thickness if needed
  geom_point(aes(fill = Core), shape = 21, color = "black", size = 1, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Loss-On-Ignition",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~Site) 
LOI_plot

## C:N Ratio----
CN_plot <- ggplot(deep_all_geochem, aes(y = CN, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 0.75) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 1, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="C:N Ratio",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  # theme(axis.title.y = element_blank()) +
  facet_wrap(~Site)
CN_plot

## C13----
my_colors_2 <- c("#fdc527", "#414487")

C13_plot <- ggplot(carbon13, aes(y = C13, x = Years_CE)) +
  geom_line(aes(color = Site), linewidth = 0.75) +  # Adjust line thickness if needed
  geom_point(aes(fill = Site), shape = 21, color = "black", size = 1, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors_2) +
  scale_fill_manual(values = my_colors_2) +
  scale_y_continuous(name = expression(delta^13*C), position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  theme(axis.title.y = element_blank(), 
        axis.text.x = element_text(angle = 25, hjust = 0)) +
  facet_wrap(~Site) 
C13_plot

## Combining plots----
OM_combined_plot <- (BD_plot | LOI_plot) / (CN_plot | C13_plot) + 
  plot_layout(guides = "collect") 
OM_combined_plot

