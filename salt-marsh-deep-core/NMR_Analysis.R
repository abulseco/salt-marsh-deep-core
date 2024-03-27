# NMR Analysis for FICUS Project
# Samples were run at EMSL between March-April 2018
# Integrated by ABM using nmrj software
# Code updated by ABM on 11-27-2018

# Load libraries----
library(ggplot2)
library(patchwork)
library(reshape2)

# Functions----
theme.ficus <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle = 45, vjust=1, hjust=1, color = "black"),
          axis.text.y=element_text(size=12, color = "black"),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.title = element_text(size=14, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(), 
          legend.position = "none")
}

# No title
theme.ficus2 <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle = 45, vjust=1, hjust=1, color = "black"),
          axis.text.y=element_text(size=12, color = "black"),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.title = element_blank(),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(), 
          legend.position = "none")
}

# Plots by functional group per core----
data_int <- read.csv("integrations_all.csv", header = T)
str(data_int)

# Data organization----
data_SW1601 <- subset(data_int, CoreID == "SW1601") # Subset by core
data_SW1602 <- subset(data_int, CoreID == "SW1602")
data_SW1603 <- subset(data_int, CoreID == "SW1603")
data_WE1602 <- subset(data_int, CoreID == "WE1602")
data_WE1603 <- subset(data_int, CoreID == "WE1603")
data_WE1604 <- subset(data_int, CoreID == "WE1604")

data_ref <- subset(data_int, Site == "Reference")
data_enr <- subset(data_int, Site == "Enriched")

data_10 <- subset(data_int, Depth == "10")
data_30 <- subset(data_int, Depth == "30")
data_40 <- subset(data_int, Depth == "40")
data_80 <- subset(data_int, Depth == "80")
data_130 <- subset(data_int, Depth == "130")
data_190 <- subset(data_int, Depth == "190")

# Scatter plots by core---- 
as.factor(data_int$Depth)

# Just temporarily 
data_int <- data_enr

pdf("groups_by_core.pdf")

plot_paraffin <- ggplot(data_int, aes(x = Depth, y = paraffin, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() +
  ggtitle("% Paraffin") +
  ylab("") 
  #xlab("") 
plot_paraffin <- plot_paraffin + coord_flip() + scale_x_reverse()
plot_paraffin

plot_methoxy <- ggplot(data_int, aes(x = Depth, y = methoxy, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() +
  ggtitle("% Methoxy") +
  ylab("") +
  xlab("") 
plot_methoxy <- plot_methoxy + coord_flip() + scale_x_reverse()
plot_methoxy

plot_carbohydrate <- ggplot(data_int, aes(x = Depth, y = carbohydrate, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() + 
  ggtitle("% Carbohydrate") +
  ylab("") +
  xlab("") 
plot_carbohydrate <- plot_carbohydrate + coord_flip() + scale_x_reverse()
plot_carbohydrate

plot_aromatic_C <- ggplot(data_int, aes(x = Depth, y = aromatic_C, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() + 
  ggtitle("% Aromatic C") +
  ylab("") +
  xlab("") 
plot_aromatic_C <- plot_aromatic_C + coord_flip() + scale_x_reverse()
plot_aromatic_C

plot_sub_aromatic_C <- ggplot(data_int, aes(x = Depth, y = sub_aromatic_C, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() + 
  ggtitle("% Sub. Aromatic C") +
  ylab("") 
  #xlab("") 
plot_sub_aromatic_C <- plot_sub_aromatic_C + coord_flip() + scale_x_reverse()
plot_sub_aromatic_C

plot_Osub_aromatic_C <- ggplot(data_int, aes(x = Depth, y = O.sub_aromatic_C, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() + 
  ggtitle("% O Sub. Aromatic C") +
  ylab("") +
  xlab("") 
plot_Osub_aromatic_C <- plot_Osub_aromatic_C + coord_flip() + scale_x_reverse()
plot_Osub_aromatic_C

plot_carboxyl <- ggplot(data_int, aes(x = Depth, y = carboxyl, fill = CoreID)) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 2) +
  geom_line(aes(color = CoreID), linetype = 2) +
  scale_color_manual(values=c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  scale_fill_manual(values = c("#005a32", "#74c476", "#c7e9c0","#084594","#6baed6","#c6dbef")) +
  theme.ficus() +
  ggtitle("% Carboxyl") +
  ylab("") +
  xlab("") 
plot_carboxyl <- plot_carboxyl + coord_flip() + scale_x_reverse()
plot_carboxyl

plot_paraffin + plot_methoxy + plot_carbohydrate + plot_aromatic_C + 
  plot_sub_aromatic_C + plot_Osub_aromatic_C + plot_carboxyl +
  plot_layout(ncol=4)

dev.off()

# Boxplots individually by depth, both sites----
data_box <- read.csv("integrations_boxplots.csv", check.names = F, header = T)
# In this dataset, all the same as above but got rid of 190cm depth for now
# Also trying it with an "x" in front of the depth to make factor
str(data_box)

# Then subset it [by depth] and melt the heck out of it
data_box_10 <- subset(data_box, Depth == "10")
data_box_10m <- melt(data_box_10[,2:9], id.var = "Site")

data_box_30 <- subset(data_box, Depth == "30")
data_box_30m <- melt(data_box_30[,2:9], id.var = "Site")

data_box_40 <- subset(data_box, Depth == "40")
data_box_40m <- melt(data_box_40[,2:9], id.var = "Site")

data_box_80 <- subset(data_box, Depth == "80")
data_box_80m <- melt(data_box_80[,2:9], id.var = "Site")

data_box_130 <- subset(data_box, Depth == "130")
data_box_130m <- melt(data_box_130[,2:9], id.var = "Site")

data_box_190 <- subset(data_box, Depth == "190")
data_box_190m <- melt(data_box_190[,2:9], id.var = "Site")

dodge = 0.5

# Plotting

pdf("boxplots_by_compound.pdf")
jpeg("boxplots_by_compound.jpg")

bp_10 <- ggplot(data = data_box_10m, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_10 <- bp_10 + ggtitle("10 cm") 
bp_10

bp_30 <- ggplot(data = data_box_30m, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_30 <- bp_30 + ggtitle("30 cm")
bp_30

bp_40 <- ggplot(data = data_box_40m, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_40 <- bp_40 + ggtitle("40 cm")
bp_40

bp_80 <- ggplot(data = data_box_80m, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_80 <- bp_80 + ggtitle("80 cm")
bp_80

bp_130 <- ggplot(data = data_box_130m, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_130 <- bp_130 + ggtitle("130 cm")
bp_130

bp_10 + bp_30 + bp_40 + bp_80 + bp_130 + plot_layout(ncol = 3)

dev.off()

# Boxplots by compound, across all depths within each site----
# Redefining this variable because having issues with melt function
# Will fix this later
# Alright, can't figure this out. Leaving it as is for now
data_box_enr <- read.csv("integrations_boxplots_enriched.csv", check.names = F, header = T)
data_box_ref <- read.csv("integrations_boxplots_reference.csv", check.names = F, header = T)

data_box_enr.m <- melt(data_box_enr, id.var = "Depth")
data_box_enr.m
data_box_ref.m <- melt(data_box_ref, id.var = "Depth")
data_box_ref.m

bp_enr_aromatic <- ggplot(data = data_box_enr.m, aes(x = Depth, y = value)) +
  geom_boxplot() +
  scale_fill_manual(values=c("forestgreen", "darkblue")) +
  theme.ficus() +
  xlab("") + 
  ylab("")
bp_enr_aromatic

