# Fig. 3: FT-IR/NMR predictions

library(ggplot2)
library(plyr) 
library(paletteer)
library(viridis)
library(patchwork)
library(here)
library(lme4)

theme.geochem <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size = 14, color="black"),
          axis.text.y=element_text(size = 14, color="black"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          text=element_text(size=14),
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
          text=element_text(size=14),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.line = element_line(colour = "black"))
}

# upload necessary data files
nmr_data_predicted <- read.csv("predicted-nmr-mixed-model.csv", header = T)
str(nmr_data_predicted)

# NMR plots----
my_colors <- c("#fdc527", "#fe9f6d", "#de4968", "#2a788e", "#414487", "#440154")


## Carbohydrates----
carbohydrate_plot <- ggplot(nmr_data_predicted, aes(y = Carbohydrate_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Carbohydrates - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
carbohydrate_plot <- carbohydrate_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Carbohydrates), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Carbohydrates), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
carbohydrate_plot

## Proteins----
protein_plot <- ggplot(nmr_data_predicted, aes(y = Protein_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Proteins - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
protein_plot <- protein_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Protein), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Protein), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
protein_plot

## Lignin----
lignin_plot <- ggplot(nmr_data_predicted, aes(y = Lignin_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Lignin - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
lignin_plot <- lignin_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Lignin), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Lignin), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
lignin_plot

## Lipids----
lipid_plot <- ggplot(nmr_data_predicted, aes(y = Lipid_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Lipid - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
lipid_plot <- lipid_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Lipid), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Lipid), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
lipid_plot

## Char----
char_plot <- ggplot(nmr_data_predicted, aes(y = Char_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Char - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
char_plot <- char_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Char), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Char), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
char_plot

## Aromatics----
aromatics_plot <- ggplot(nmr_data_predicted, aes(y = Aromatic_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Aromatic Carbon - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
aromatics_plot <- aromatics_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Aromatic), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Aromatic), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
aromatics_plot

## Phenolics----
phenolics_plot <- ggplot(nmr_data_predicted, aes(y = Phenolic_PRED, x = Depth)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Phenolics - Weight %",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
phenolics_plot <- phenolics_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Phenolic), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Phenolic), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
phenolics_plot

## Degrataion----
degradation_plot <- ggplot(nmr_data_predicted, aes(y = Degradation_Index_PRED, x = Depth)) +
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Degradation Index",position = "right") +
  xlab("Depth (cm)") +
  coord_flip() +
  scale_x_reverse() +
  theme.geochem.legend() +
  facet_wrap(~CoreID, nrow = 1)

# Add on predicted data
degradation_plot <- degradation_plot +
  geom_line(data = nmr_data_predicted, aes(x = Depth, y = Degradation_Index), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Depth, y = Degradation_Index), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1) 
degradation_plot

# Combine plots----
NMR_combined_plot <- (carbohydrate_plot) / (protein_plot) / (lignin_plot) / (lipid_plot) /
  (char_plot) / (aromatics_plot) / (phenolics_plot) / (degradation_plot) + 
  plot_layout(guides = "collect") 
NMR_combined_plot

save_plot(
  filename = "final_NMR_plot.pdf",   # Output file name
  plot = NMR_combined_plot,             # Your combined plot
  base_width = 11,               # Adjust width
  base_height = 15                # Adjust height
)

# By Year----
## Carbohydrates----
carbohydrate_plot <- ggplot(nmr_data_predicted, aes(y = Carbohydrate_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Carbohydrates - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
carbohydrate_plot <- carbohydrate_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Carbohydrates), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Carbohydrates), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
carbohydrate_plot

## Proteins----
protein_plot <- ggplot(nmr_data_predicted, aes(y = Protein_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Proteins - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
protein_plot <- protein_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Protein), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Protein), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
protein_plot

## Lignin----
lignin_plot <- ggplot(nmr_data_predicted, aes(y = Lignin_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Lignin - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
lignin_plot <- lignin_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Lignin), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Lignin), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
lignin_plot

## Lipids----
lipid_plot <- ggplot(nmr_data_predicted, aes(y = Lipid_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Lipid - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
lipid_plot <- lipid_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Lipid), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Lipid), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
lipid_plot

## Char----
char_plot <- ggplot(nmr_data_predicted, aes(y = Char_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Char - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
char_plot <- char_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Char), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Char), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
char_plot

## Aromatics----
aromatics_plot <- ggplot(nmr_data_predicted, aes(y = Aromatic_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Aromatic Carbon - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
aromatics_plot <- aromatics_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Aromatic), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Aromatic), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
aromatics_plot

## Phenolics----
phenolics_plot <- ggplot(nmr_data_predicted, aes(y = Phenolic_PRED, x = Years_CE)) + 
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Phenolics - Weight %",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem() +
  facet_wrap(~CoreID, nrow = 1) 

# Add on predicted data
phenolics_plot <- phenolics_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Phenolic), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Phenolic), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1)
phenolics_plot

## Degrataion----
degradation_plot <- ggplot(nmr_data_predicted, aes(y = Degradation_Index_PRED, x = Years_CE)) +
  geom_line(aes(color = CoreID), linewidth = 1.0) +  # Adjust line thickness if needed
  geom_point(aes(fill = CoreID), shape = 21, color = "black", size = 2, stroke = 0.5) +  # Black outline for points
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(name="Degradation Index",position = "right") +
  xlab("Year (CE)") +
  coord_flip() +
  theme.geochem.legend() +
  facet_wrap(~CoreID, nrow = 1)

# Add on predicted data
degradation_plot <- degradation_plot +
  geom_line(data = nmr_data_predicted, aes(x = Years_CE, y = Degradation_Index), linetype = "dashed") +
  geom_point(data = nmr_data_predicted, aes(x = Years_CE, y = Degradation_Index), shape = 21, color = "black", fill = "NA", size = 2) +
  facet_wrap(~CoreID, nrow = 1) 
degradation_plot

# Combine plots----
NMR_combined_plot <- (carbohydrate_plot) / (protein_plot) / (lignin_plot) / (lipid_plot) /
  (char_plot) / (aromatics_plot) / (phenolics_plot) / (degradation_plot) + 
  plot_layout(guides = "collect") 
NMR_combined_plot

save_plot(
  filename = "NMR-by-year-2.pdf",   # Output file name
  plot = NMR_combined_plot,             # Your combined plot
  base_width = 11,               # Adjust width
  base_height = 15                # Adjust height
)

# Models----
carb_lmm1 <- lmer(Carbohydrate_PRED ~ Depth + (1 | CoreID), data = nmr_data_predicted)
carb_lmm2 <- lmer(Carbohydrate_PRED ~ 1 + (1 | CoreID), data = nmr_data_predicted)
carb_lm <- lm(Carbohydrate_PRED ~ Depth, data = nmr_data_predicted)
AIC(carb_lmm1, carb_lmm2, carb_lm)


