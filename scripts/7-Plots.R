#########################################!
#### Code for plotting results and figures
#### 
#### Marcel Montanyes, DTU Aqua, 17-11-2021
#######################################!

library(dplyr)
library(ggplot2)
library(Hmsc)


nChains <- 4
samples <- 250
thin <-  100
transient <- round(0.5*samples*thin)
filename <- paste0("Model fit/", "chains_",as.character(nChains), "_samples_",as.character(samples), "_thin_",as.character(thin))
load(filename)



# Figure 1. Study area ----------------------------------------------------
load("My data/numcpue_nemo_medusa_fishing.RData")
Data <- numcpue_nm_f

# All NAs are accounted for and known:
sum(is.na(Data)) == sum(is.na(Data$TotalFishingHours)) + sum(is.na(Data$BeamHours)) + sum(is.na(Data$OtterHours)) + sum(is.na(Data$TotalFishingHours_lag)) + sum(is.na(Data$BeamHours_lag)) + sum(is.na(Data$OtterHours_lag)) + sum(is.na(Data$Area.swept))*(279-31) + sum(is.na(Data$Sediment_type)) + sum(is.na(Data$Sediment_ID))

Data <- Data[!is.na(Data$Area.swept) & !is.na(Data$Sediment_type) & !is.na(Data$TotalFishingHours_lag) & Data$Salinity_floor > 0,] # We can't use those observations without data

nrow(Data[Data$Sediment_type == "No data at this level of folk",])
Data <- droplevels(subset(Data, Sediment_type != "No data at this level of folk"))


# Define study area
my_studyArea <- data.frame(ShootLong = Data$ShootLong, ShootLat = Data$ShootLat, StatRec = Data$StatRec)
my_studyArea[,c("Longitude", "Latitude")] <- mapplots::ices.rect(my_studyArea$StatRec)

# Plot
library("rnaturalearth")
world <- ne_countries(scale = "medium", returnclass = "sf")


q1 <- ggplot(data = world) +
  geom_sf(fill = "grey80", color = "grey20") +
  coord_sf(xlim = c(-12,30), ylim = c(35,71), expand = FALSE)+
  geom_rect(aes(xmin = min(my_studyArea$Longitude) - 1, xmax = max(my_studyArea$Longitude) + 1,
                ymin = min(my_studyArea$Latitude) - 1, ymax = max(my_studyArea$Latitude) + 1), color = "#D82230", fill = "transparent", linewidth = 1) +
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"), # "aliceblue"
        plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
q1

q2 <- ggplot(data = world) +
  geom_tile(data = my_studyArea, aes(x = Longitude, y = Latitude, color = ""), fill = NA, size = 0.5) + 
  scale_color_manual(values = c("grey10")) +
  geom_point(data = my_studyArea, aes(x = ShootLong, y = ShootLat, fill = ""), size = 0.5, color = "#3B5C86") + 
  geom_sf(fill = "grey80", color = "grey20") +
  coord_sf(xlim = range(my_studyArea$Longitude) + c(-1,1.5), ylim = range(my_studyArea$Latitude) + c(-1, 1.5), expand = FALSE) +
  guides(fill = guide_legend(title = "Haul", title.position = "right", override.aes = list(size = 4)),
         color = guide_legend(title = "ICES rectangle", title.position = "right", override.aes = list(size = 4))) +
  labs(x = "", y = "") +
  guides(shape = guide_legend(override.aes = list(size = 0.2)))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"), # "aliceblue"
        plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 6),
        legend.title = element_text(size = 8), legend.position = "bottom")
q2  

library(cowplot)
gg_inset_map2 <-  ggdraw() +
  draw_plot(q2) +
  draw_plot(q1, x = 0.65, y = 0.71, width = 0.4, height = 0.3)

gg_inset_map2


tiff("figures/Figure_1.tiff", units = "mm", width = 100, height = 150, res = 300)
gg_inset_map2
dev.off()



# Figure 2. Variance partitioning -----------------------------------------
round(head(models$fullPa$X),2)
colnames(models$fullPa$X)
groupnames <- c("Temperature", "Salinity", "Nutrients.productivity", "Sediment", "Fishing") # Define the groups of variables
group <- c(1,1,1,1,
           2,
           3,3,3,
           4,4,4,4,
           5) # Assign each variable to a group)
# The intercept can be assigned to either group as it doesn't make any difference
VPfull <- computeVariancePartitioning(models$fullPa, group = group, groupnames = groupnames)
VPfull <- data.frame(t(VPfull$vals))
head(VPfull)
VPfull$Species <- rownames(VPfull)
VPfull <- reshape2::melt(VPfull%>%group_by(Species)%>%
                           summarise(
                             Temperature = Temperature,
                             Nutrients.productivity = Nutrients.productivity,
                             Fishing = Fishing,
                             Salinity = Salinity,
                             Sediment = Sediment,
                             StatRec = Random..StatRec, Year = Random..Year, Season = Random..Season),
                         id = "Species")

vpSummary <- VPfull%>%group_by(variable)%>%summarise(Mean = round(mean(value),2), SD = round(sd(value),2))
vpSummary$var_type <- c(rep("Fixed", times = 5), rep("Random", times = 3))
myOrder <- vpSummary$variable[order(vpSummary$var_type, decreasing = T, vpSummary$Mean)]


library(vioplot)
library(colorspace)
vioplot(VPfull$value~VPfull$variable, col = rainbow_hcl(length(levels(VPfull$variable))), ylim = c(0, max(VPfull$value)),
        xlab ="", ylab = "Proportion")


prePlot <- VPfull
prePlot$variable <- recode(prePlot$variable,
                           Nutrients.productivity = "Productivity",
                           StatRec = "ICES\nRectangle")

dMean <- prePlot%>%group_by(variable)%>%summarise(Mean = round(mean(value), 2))
dMean$Text <- paste(dMean$variable, "=", dMean$Mean)

myViolin <- ggplot(prePlot, aes(x = variable, y = value, fill = variable), color = "black") + geom_violin(scale = "width", show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", size = 3, show.legend = FALSE, color = "black") +
  scale_fill_manual(values = c("#466D8E", "#466D8E", "#466D8E", "#466D8E", "#466D8E", "#B11D1D", "#B11D1D", "#B11D1D")) +
  geom_text(data = dMean, aes(label = Mean, y = -0.05, x = c(1:8)), color = "black", size = 5) +
  labs(y = "Proportion of explained variation\n", x = "") +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  annotate("text", x = c(3, 7), y = c(0.9, 0.9), label = c("Fixed", "Random"), size = 6) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")

myViolin

tiff("figures/Figure_2.tiff", units = "mm", width = 200, height = 150, res=300)
myViolin
dev.off()

# Figure 3. Beta plot -----------------------------------------------------
postBeta <-  getPostEstimate(models$fullPa, parName = "Beta")
supportLevel <- 0.95

plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))


# ggplot beta and gamma plot
library(ggimage)
library(ggtree)
# here the complete guide for ggtree https://yulab-smu.top/treedata-book/chapter8.html

# phylogeny
# data$phy is the df you give the model for the phylogeny 
load("Data sources/fish_phylogenetic_tree.RData")
fish_phylo$tip.label <- gsub("_", " ", fish_phylo$tip.label)
setdiff(colnames(models$fullPa$Y), fish_phylo$tip.label)


par(mfrow = c(1,1), mar = c(4, 10, 3, 0))
plot(fish_phylo, type = "phylogram", show.node.label = TRUE)
par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

tree_plot_a <- ggtree(fish_phylo, branch.length="none") + geom_tiplab(fontface = "italic") + xlim(0, 30) #+ labs(tag = "A")
tree_plot_a
# tree_plot_b <- ggtree(fish_phylo) + geom_tiplab(fontface = "italic", align = TRUE) + xlim(0, 4.5) + labs(tag = "A")
# tree_plot_b



### NEED TO CHECK if the precedure is correct! Are the +/- responses correct?
# beta heatmap
postBeta <-  getPostEstimate(models$fullPa, parName = "Beta")

temp.beta <- as.data.frame(postBeta$mean) # 'temp' stands for temporary
temp.beta$param <- models$fullPa$covNames
temp.beta 
temp.beta <- temp.beta %>% tidyr::pivot_longer(.,-param, names_to = 'species', values_to ='value')

sup.beta <- as.data.frame(postBeta$support) # n.beta because I'm extracting the negative values
sup.beta$param <- models$fullPa$covNames
sup.beta 
sup.beta <- sup.beta %>% tidyr::pivot_longer(.,-param, names_to = 'species', values_to ='value')

temp.beta$support <- sup.beta$value

supportLevel <- 0.95
temp.beta$pos.neg <- 0
temp.beta$pos.neg[temp.beta$support > supportLevel] <- 1
temp.beta$pos.neg[temp.beta$support < (1-supportLevel)] <- -1

temp.beta$pos.neg <- factor(temp.beta$pos.neg, levels = c(1,0,-1)) # this is needed to always plot the '+' and '-' in the legend


windows(800, 600, pointsize = 12); #opens a separate window with the size you want
par(mar = c(4, 12, 0, 0));
plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))

windows(800, 600, pointsize = 12); #opens a separate window with the size you want
ggplot(temp.beta, aes(x = param, y = species)) +
  geom_tile(aes(fill=pos.neg),color='black')+
  scale_fill_manual(values=c('#E31B23','white','#003366'),labels=c('+','','-'), name='',guide=guide_legend(keyheight =4, keywidth = 1)) + xlab('') + ylab('')+
  theme_bw()+
  theme(text = element_text(family = "sans",size = 12),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank())


# NOTE THAT the order of the environmental variables is different and that species are sorted opposite

# Seems about right!



# Rename variables
temp.beta$Environment <- factor(temp.beta$param)
levels(temp.beta$Environment)
temp.beta$Environment <- dplyr::recode(temp.beta$Environment, 
                                       "Detritus_floor" = "Detritus",
                                       "DIN_surf" = "DIN",
                                       "Fishing_lag" = "Fishing",
                                       "Phy_surf" = "Chlorophyll a",
                                       "poly(SBT, degree = 2, raw = TRUE)1" = "SBT",
                                       "poly(SBT, degree = 2, raw = TRUE)2" = "SBT2",
                                       "SBS_seasonality" = "SBS seasonality",
                                       "SBT_seasonality" = "SBT seasonality",
                                       "SedimentCoarse substrate" = "Coarse substrate",
                                       "SedimentMixed sediment" = "Mixed sediment",
                                       "SedimentROck and boulders" = "Rock and boulders",
                                       "SedimentSand" = "Sand")
temp.beta$Environment <-  factor(temp.beta$Environment,
                                 levels = c("(Intercept)", "SBT", "SBT2", "SBT seasonality", "SBS seasonality",
                                            "Detritus", "Chlorophyll a", "DIN",
                                            "Sand", "Coarse substrate", "Mixed sediment", "Rock and boulders", 
                                            "Fishing"))
levels(temp.beta$Environment)

# Set species order same as phylogenetic tree
sp_order <- get_taxa_name(tree_plot_a)
temp.beta$Species_sort <- as.factor(temp.beta$species)
temp.beta$Species_sort <-  factor(temp.beta$Species_sort, levels = rev(sp_order)) # Needs to be reversed
levels(temp.beta$Species_sort)

beta.heatmap <- ggplot(temp.beta, aes(x = Environment, y = Species_sort)) +
  geom_tile(aes(fill=pos.neg),color='black') +
  scale_fill_manual(values=c('#E31B23','white','#003366'),labels=c('+','','-'), name='',guide=guide_legend(keyheight =4, keywidth = 1)) + xlab('') + ylab('')+
  theme_bw()+
  theme(text = element_text(family = "sans",size = 12),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank())
# scale_x_discrete(limits=c('Intercept','Seabed: sand','Depth', "Bottom temperature"), expand=c(0,0))
# scale_y_discrete(limits=rev(c('Brown skate','Starry skate','Thornback skate','Marbled torpedo','Common eagle ray','Small-spotted catshark','Nursehound','Smooth-hound','Spurdog')),expand=c(0,0),position='right') # position ='right' is needed for having a nicer viz

beta.heatmap_nonames <- ggplot(temp.beta, aes(x = Environment, y = Species_sort)) +
  geom_tile(aes(fill=pos.neg),color='black')+
  scale_fill_manual(values=c('#E31B23','white','#003366'),labels = c('+','','-'), name = '', guide = guide_legend(keyheight = 4, keywidth = 1)) +
  theme_bw()+
  labs(y = "", x = "") +
  # labs(tag = "B") +
  theme(text = element_text(family = "sans", size = 10),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank(),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
        # axis.text.x  = element_blank(),
        axis.text.y = element_blank())

library(patchwork)
# Check if species names match
tree_plot_a + beta.heatmap + plot_layout(widths = c(2, 3))
tree_plot_a$data$label <- paste0("  ", tree_plot_a$data$label) # add space between tree and sp names
tree_plot_a + beta.heatmap + plot_layout(widths = c(2, 3))

tree_plot_a + beta.heatmap_nonames + plot_layout(widths = c(3, 4))
# tree_plot_b + beta.heatmap_nonames + plot_layout(widths = c(2, 3))

tree_plot_a <- ggtree(fish_phylo, branch.length = "none") + geom_tiplab(fontface = "italic", size = 3) + xlim(0, 30) #+ labs(tag = "A")

# If they match, continue

tiff("figures/Figure_3.tiff", units = "mm", width = 220, height = 250, res = 300)
# tiff("figures/Figure_3.tiff", units="mm", width = 300, height = 300, res = 300)
tree_plot_a + beta.heatmap_nonames + plot_layout(widths = c(4.5, 5))
dev.off()
# This figure should be 11 cm width


par(mar = c(4, 12, 0, 0))
plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))


# 
# # Add proportion of responses at the bottom
# betaSummary <- ggplot(temp.beta, aes(x = Environment, fill = pos.neg)) + geom_bar(colour = "black") +
#   scale_fill_manual(values = c('#E31B23','white','#003366'), labels = c('+','','-'),
#                     name = '',guide = guide_legend(keyheight = 4, keywidth = 1)) +
#   labs(y = "# Species", x = "", tag = "C") +
#   theme_bw() +
#   theme(text = element_text(family = "sans",size = 12),
#         strip.text = element_text(family = "sans",face = 'bold'),
#         axis.text = element_text(family = "sans"),
#         legend.position = "none",
#         legend.title = element_text(family = "sans"),
#         legend.text = element_text(family = "sans"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
#         panel.background = element_blank(), strip.background = element_blank(),
#         axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
#         axis.text.y = element_text(family = "sans"),
#         plot.margin = unit(c(0,1,1,1), "cm"))
# betaSummary
# 
# tiff("figures/Figure_3_test.tiff", units = "mm", width = 300, height = 350, res = 300)
# tree_plot_a + beta.heatmap_nonames + ggplot() + theme_void() + betaSummary + plot_layout(widths = c(3, 4), heights = c(7, 1))
# dev.off()
# 




# Figure 4. Gamma plot -----------------------------------------------------
postGamma <-  getPostEstimate(models$fullPa, parName = "Gamma")
supportLevel <- 0.95

plotGamma(models$fullPa, postGamma, main = paste0("Gamma\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))


# ggplot beta and gamma plot
library(ggimage)
library(ggtree)
# here the complete guide for ggtree https://yulab-smu.top/treedata-book/chapter8.html

### NEED TO CHECK if the precedure is correct! Are the +/- responses correct?
# beta heatmap
postGamma <-  getPostEstimate(models$fullPa, parName = "Gamma")

temp.gamma <- as.data.frame(postGamma$mean) # 'temp' stands for temporary
names(temp.gamma) <- models$fullPa$trNames
temp.gamma$param <- models$fullPa$covNames
temp.gamma 
temp.gamma <- temp.gamma %>% tidyr::pivot_longer(.,-param, names_to = 'traits', values_to ='value')

sup.gamma <- as.data.frame(postGamma$support) # n.beta because I'm extracting the negative values
names(sup.gamma) <- models$fullPa$trNames
sup.gamma$param <- models$fullPa$covNames
sup.gamma 
sup.gamma <- sup.gamma %>% tidyr::pivot_longer(.,-param, names_to = 'traits', values_to ='value')

temp.gamma$support <- sup.gamma$value

supportLevel <- 0.95
temp.gamma$pos.neg <- 0
temp.gamma$pos.neg[temp.gamma$support > supportLevel] <- 1
temp.gamma$pos.neg[temp.gamma$support < (1-supportLevel)] <- -1

temp.gamma$pos.neg <- factor(temp.gamma$pos.neg, levels = c(1,0,-1)) # this is needed to always plot the '+' and '-' in the legend



# Rename variables
temp.gamma$Environment <- factor(temp.gamma$param)
levels(temp.gamma$Environment)
temp.gamma$Environment <- dplyr::recode(temp.gamma$Environment, 
                                        "Detritus_floor" = "Detritus",
                                        "DIN_surf" = "DIN",
                                        "Fishing_lag" = "Fishing",
                                        "Phy_surf" = "Chlorophyll a",
                                        "poly(SBT, degree = 2, raw = TRUE)1" = "SBT",
                                        "poly(SBT, degree = 2, raw = TRUE)2" = "SBT2",
                                        "SBS_seasonality" = "SBS seasonality",
                                        "SBT_seasonality" = "SBT seasonality",
                                        "SedimentCoarse substrate" = "Coarse substrate",
                                        "SedimentMixed sediment" = "Mixed sediment",
                                        "SedimentROck and boulders" = "Rock and boulders",
                                        "SedimentSand" = "Sand")
temp.gamma$Environment <-  factor(temp.gamma$Environment,
                                  levels = c("(Intercept)", "SBT", "SBT2", "SBT seasonality", "SBS seasonality",
                                             "Detritus", "Chlorophyll a", "DIN",
                                             "Sand", "Coarse substrate", "Mixed sediment", "Rock and boulders", 
                                             "Fishing"))
levels(temp.gamma$Environment)



windows(800, 600, pointsize = 12) #opens a separate window with the size you want
par(mar = c(4, 12, 0, 0))
plotGamma(models$fullPa, postGamma, main = paste0("Gamma\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))

windows(800, 600, pointsize = 12) #opens a separate window with the size you want
ggplot(temp.gamma, aes(x = Environment, y = traits)) +
  geom_tile(aes(fill=pos.neg),color='black')+
  scale_fill_manual(values=c('#E31B23','white','#003366'),labels=c('+','','-'), name='',guide=guide_legend(keyheight =4, keywidth = 1)) + xlab('') + ylab('')+
  theme_bw()+
  theme(text = element_text(family = "sans",size = 12),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank())


# Seems about right!



# Rename traits:
temp.gamma$Traits <- factor(temp.gamma$traits)
levels(temp.gamma$Traits)
temp.gamma$Traits <- dplyr::recode(temp.gamma$Traits, 
                                   "body.shapeeel-like" = "Eel-like (body shape)",
                                   "body.shapeelongated" = "Elongated (body shape)",
                                   "body.shapeflat" = "Flat  (body shape)",
                                   "body.shapefusiform" = "Fusiform  (body shape)",
                                   "feeding.modegeneralist" =  "Generalist (feeding mode)",
                                   "feeding.modepiscivorous" = "Piscivorous (feeding mode)",
                                   "feeding.modeplanktivorous" = "Planktivorous (feeding mode)",
                                   "fin.shapepointed" = "Pointed (caudal fin shape)",
                                   "fin.shaperounded" = "Rounded (caudal fin shape)",
                                   "fin.shapetruncate" = "Truncated (caudal fin shape)",
                                   "habitatbathypelagic" = "Bathypelagic (habitat)",
                                   "habitatbenthopelagic" = "Benthopleagic (habitat)",
                                   "habitatdemersal" = "Demersal (habitat)",
                                   "habitatpelagic" = "Pelagic (habitat)",
                                   "log_age.maturity" = "Age at maturity",
                                   "log_fecundity" = "Fecundity",
                                   "log_growth" = "Growth (K)",
                                   "log_length.max" = "Maximum length",
                                   "log_offspring.size" = "Offspring size",
                                   "spawning.typeguarder" = "Guarder (spawning type)",
                                   "spawning.typenon-guarder" = "Non-guarder (spawning type)",
                                   "trophic_level" = "Trophic level")
sort(levels(temp.gamma$Traits))
temp.gamma$Traits <-  factor(temp.gamma$Traits,
                             levels = rev(c("Age at maturity", "Fecundity", "Growth (K)", "Maximum length", "Offspring size", "Trophic level",
                                            "Eel-like (body shape)", "Elongated (body shape)", "Flat  (body shape)", "Fusiform  (body shape)",
                                            "Pointed (caudal fin shape)", "Rounded (caudal fin shape)", "Truncated (caudal fin shape)",
                                            "Generalist (feeding mode)", "Piscivorous (feeding mode)", "Planktivorous (feeding mode)",
                                            "Bathypelagic (habitat)", "Benthopleagic (habitat)", "Demersal (habitat)", "Pelagic (habitat)",
                                            "Guarder (spawning type)", "Non-guarder (spawning type)",
                                            "(Intercept)"
                             )))
levels(temp.gamma$Traits)

# 
# gamma.heatmap <- ggplot(temp.gamma, aes(x = Environment, y = Traits)) +
#   geom_tile(aes(fill=pos.neg),color='black')+
#   scale_fill_manual(values=c('#E31B23','white','#003366'),labels=c('+','','-'), name='',guide=guide_legend(keyheight = 4, keywidth = 1)) + xlab('') + ylab('')+
#   theme_bw()+
#   theme(text = element_text(family = "sans",size = 10),
#         strip.text = element_text(family = "sans",face = 'bold'),
#         axis.text = element_text(family = "sans"),
#         legend.title = element_text(family = "sans"),
#         legend.text = element_text(family = "sans"),
#         axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
#         axis.text.y  = element_text(size = 10),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid = element_blank(),
#         panel.background = element_blank(), strip.background = element_blank())
# # scale_x_discrete(limits=c('Intercept','Seabed: sand','Depth', "Bottom temperature"), expand=c(0,0))
# # scale_y_discrete(limits=rev(c('Brown skate','Starry skate','Thornback skate','Marbled torpedo','Common eagle ray','Small-spotted catshark','Nursehound','Smooth-hound','Spurdog')),expand=c(0,0),position='right') # position ='right' is needed for having a nicer viz
# 
# 
# tiff("figures/Figure_4b.tiff", units = "mm", width = 200, height = 150, res=300)
# gamma.heatmap
# dev.off()




myGamma <- temp.gamma
myGamma$Group <- stringr::str_extract(string = myGamma$Traits, pattern = "(?<=\\().*(?=\\))")
myGamma$Group[myGamma$Group == "K"] <- NA

myGamma$Only_traits <- gsub("\\s*\\([^\\)]+\\)", "", myGamma$Traits)
myGamma$Only_traits[myGamma$Only_traits == "Growth"] <- "Growth (K)"
myGamma$Only_traits[myGamma$Group == "Intercept"] <- "Intercept"
myGamma$Group[myGamma$Group == "Intercept"] <- ""
myGamma$Group[is.na(myGamma$Group)] <- ""
myGamma$Group <- gsub(" ", "\n", myGamma$Group)
myGamma$Group <- stringr::str_to_title(myGamma$Group) 
myGamma$Group[myGamma$Group == "Caudal\nFin\nShape"] <- "Caudal Fin\nShape"

# Add grouping to continuous variables:
unique(myGamma$Group)
myGamma$Group[myGamma$Group == ""] <- "Continuous\nTraits"


# Set level order
myGamma$Group <- factor(myGamma$Group, levels = c("Continuous\nTraits", "Body\nShape", "Caudal Fin\nShape", "Feeding\nMode", "Habitat", "Spawning\nType"))
myGamma$Only_traits <- factor(myGamma$Only_traits, levels = rev(sort(unique(myGamma$Only_traits))))

gamma.plot.opt2 <- ggplot(myGamma, aes(x = Environment, y = Only_traits, fill = pos.neg)) + geom_tile(color = 'black')+
  scale_fill_manual(values = c('#E31B23','white','#003366'), labels = c('+','','-'), name='',guide = guide_legend(keyheight = 4, keywidth = 1)) +
  facet_grid(Group ~., scales = "free_y", space = "free_y") +
  # facet_grid(Group ~., scales = "free_y", space = "free_y", switch = "y") +
  xlab('') + ylab('') +
  theme_bw() +
  theme(text = element_text(family = "sans", size = 14),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans", size = 16),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y  = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(family = "sans",face = 'bold'),
        strip.text.y = element_text(size = 12, face = "bold", hjust = 0.5, angle = 0),
        strip.placement = "outside", strip.background = element_blank())


tiff("figures/Figure_4.tiff", units = "mm", width = 200, height = 175, res = 300)
gamma.plot.opt2
dev.off()



# Figure 5. Species associations ------------------------------------------
supportLevel <- 0.95
library(ggcorrplot)
library(corrplot)
OmegaCor <-  computeAssociations(models$fullPa)
names(OmegaCor) <- names(models$fullPa$ranLevels)

toPlot <- ((OmegaCor$StatRec$support>supportLevel) + (OmegaCor$StatRec$support<(1-supportLevel))>0)*OmegaCor$StatRec$mean 
plotOrder <-  corrMatOrder(OmegaCor$StatRec$mean, order="AOE") # Euclidian distances

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

corr <- as.matrix(toPlot[plotOrder,plotOrder])
# corr <- base::round(x = corr, digits = 2)
corr[corr > 0] <- 1
corr[corr < 0] <- -1
upper_tri <- get_upper_tri(corr)

melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
melted_cormat$value <- factor(melted_cormat$value, levels = c("1", "0", "-1"))

# Heatmap
omega.plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) + scale_x_discrete(limits = rev(levels(melted_cormat$Var2)))+
  geom_tile(color = "black") +
  labs(x = "", y = "") +
  scale_fill_manual(values = c('#E31B23','white','#003366'), labels = c('+','','-'), name ='', guide = guide_legend(keyheight = 4, keywidth = 1)) +
  theme_bw() +
  theme(text = element_text(family = "sans", size = 10),
        strip.text = element_text(family = "Helvetica", face = 'bold'),
        axis.text = element_text(face = "italic", color = "black", size = 8),
        legend.text = element_text(size = 10),
        axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.25),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), strip.background = element_blank(),
        legend.position = c(0.96,0.6),
        plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"))

omega.plot

tiff("figures/Figure_5.tiff", units = "mm", width = 250, height = 230, res = 300)
omega.plot
dev.off()



# Figure 6. Latent factors ------------------------------------------------
# Latent variables (StatRec)
postEta <- getPostEstimate(models$fullPa, parName = "Eta")
dim(postEta$mean)
postEtamean1 <- postEta$mean[,1] # Latent factor 1
postEtamean2 <- postEta$mean[,2] # Latent factor 2
postEtamean3 <- postEta$mean[,3] # Latent factor 3
postEtamean4 <- postEta$mean[,4] # Latent factor 4
postEtamean5 <- postEta$mean[,5] # Latent factor 5
dataLf <- cbind(as.data.frame(models$fullPa$ranLevels$StatRec$s), postEtamean1, postEtamean2, postEtamean3, postEtamean4, postEtamean5)

range(dataLf$postEtamean1)
range(dataLf$postEtamean2)
range(dataLf$postEtamean3)
lv_range <- range(dataLf[,c("postEtamean1", "postEtamean2", "postEtamean3")])

# xlim <- range(dataLf$Longitude) + c(-0.5, 0.5)
# ylim <- range(dataLf$Latitude) + c(-0.5, 0.5)
# 
# xbreaks<- seq(floor(min(dataLf$Longitude)), ceiling(max(dataLf$Longitude)))
# ybreaks<- seq(floor(min(dataLf$Latitude)), ceiling(max(dataLf$Latitude)))
# 

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) + geom_sf(fill = "grey80", color = "grey20") +
  coord_sf(xlim = range(dataLf$Longitude) + c(-1,1.5), ylim = range(dataLf$Latitude) + c(-1,1.5), expand = FALSE)
  

library(paletteer)
# library(ggpubr)
latfac1 <- ggplot(data = world) + geom_sf(fill = "grey80", color = "grey20") +
    coord_sf(xlim = range(dataLf$Longitude) + c(-1,1.5), ylim = range(dataLf$Latitude) + c(-1,1.5), expand = FALSE) +
    labs(x = "", y = "") +
    geom_tile(data = dataLf, aes(x = Longitude, y = Latitude, fill = postEtamean1), color = "black") + 
    scale_fill_gradient2(low = "#003366", mid = "white", high = "#E31B23", midpoint = 0, name = "Latent\nfactor 1") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.tag.position = c(0,1),
          plot.tag = element_text(vjust = 2, hjust = -3)) # + labs(tag = "(a)")
  
latfac2 <- ggplot(data = world) + geom_sf(fill = "grey80", color = "grey20") +
    coord_sf(xlim = range(dataLf$Longitude) + c(-1,1.5), ylim = range(dataLf$Latitude) + c(-1,1.5), expand = FALSE) +
    labs(x = "", y = "") +
    geom_tile(data = dataLf, aes(x = Longitude, y = Latitude, fill = postEtamean2), color = "black") + 
    scale_fill_gradient2(low = "#003366", mid = "white", high = "#E31B23", midpoint = 0, name = "Latent\nfactor 2") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.tag.position = c(0,1),
          plot.tag = element_text(vjust = 2, hjust = -3)) # + labs(tag = "(b)")
  
latfac3 <- ggplot(data = world) + geom_sf(fill = "grey80", color = "grey20") +
    coord_sf(xlim = range(dataLf$Longitude) + c(-1,1.5), ylim = range(dataLf$Latitude) + c(-1,1.5), expand = FALSE) +
    labs(x = "", y = "") +
    geom_tile(data = dataLf, aes(x = Longitude, y = Latitude, fill = postEtamean3), color = "black") + 
    scale_fill_gradient2(low = "#003366", mid = "white", high = "#E31B23", midpoint = 0, name = "Latent\nfactor 3") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.tag.position = c(0,1),
          plot.tag = element_text(vjust = 2, hjust = -3)) # + labs(tag = "(c)")


library(gridExtra)
grid.arrange(latfac1, latfac2,latfac3,
             layout_matrix = rbind(c(1, 1, 2, 2), 
                                   c(NA, 4, 4, NA)))

tiff("figures/Figure_6.tiff", units = "mm", width = 200, height = 200, res = 300)
grid.arrange(latfac1, latfac2,latfac3,
             layout_matrix = rbind(c(1, 1, 2, 2), 
                                   c(NA, 4, 4, NA)))
dev.off()




# # Latent factors 4 and 5
# ggarrange(
#   ggplot() +
#     geom_tile(data = dataLf, aes(x = Longitude, y = Latitude, fill = postEtamean4), color = "black") + 
#     scale_fill_gradient2(low = "#003366", mid = "white", high = "#E31B23", midpoint = 0, name = "Latent factor 4") +
#     borders(fill="gray44", colour="gray61") + coord_quickmap(xlim = xlim, ylim = ylim) +
#     scale_x_continuous(name='Longitude (º)', breaks = xbreaks) + 
#     scale_y_continuous(name='Latitude (º)', breaks = ybreaks),
#   
#   ggplot() +
#     geom_tile(data = dataLf, aes(x = Longitude, y = Latitude, fill = postEtamean5), color = "black") + labs(y = "") +
#     scale_fill_gradient2(low = "#003366", mid = "white", high = "#E31B23", midpoint = 0, name = "Latent factor 5") +
#     borders(fill="gray44", colour="gray61") + coord_quickmap(xlim = xlim, ylim = ylim) +
#     scale_x_continuous(name = 'Longitude (º)', breaks = xbreaks) + 
#     scale_y_continuous(name = '', breaks = ybreaks),
#   ncol = 2)


# Supplementary figure 1. Environment correlations ------------------------
load("My data/numcpue_nemo_medusa_fishing.RData")
Data <- numcpue_nm_f

# All NAs are accounted for and known:
sum(is.na(Data)) == sum(is.na(Data$TotalFishingHours)) + sum(is.na(Data$BeamHours)) + sum(is.na(Data$OtterHours)) + sum(is.na(Data$TotalFishingHours_lag)) + sum(is.na(Data$BeamHours_lag)) + sum(is.na(Data$OtterHours_lag)) + sum(is.na(Data$Area.swept))*(279-31) + sum(is.na(Data$Sediment_type)) + sum(is.na(Data$Sediment_ID))

Data <- Data[!is.na(Data$Area.swept) & !is.na(Data$Sediment_type) & !is.na(Data$TotalFishingHours_lag) & Data$Salinity_floor > 0,] # We can't use those observations without data

nrow(Data[Data$Sediment_type == "No data at this level of folk",])
Data <- droplevels(subset(Data, Sediment_type != "No data at this level of folk"))

# Select Species
my_species <- as.vector(na.omit(names(Data)[c(which(colnames(Data) == "Area.swept")+1:ncol(Data))]))

# Correlations
cor.all <- as.data.frame(cor(Data[,c(# "Year", "Month",
  "Salinity_surf", "Temperature_surf", "Tseasonlaity_surf", "Sseasonlaity_surf",
  "DIN_surf", "Detritus_surf", "Phytoplankton_surf", #"Meridional_surf", "Zonal_surf",
  "Salinity_floor", "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
  "DIN_floor", "Detritus_floor", "Phytoplankton_floor",#"Meridional_floor", "Zonal_floor",
  "Bathymetry", "TotalFishingHours_lag")]))

toPlot <- round(cor.all,2)
all(names(toPlot) == rownames(toPlot))
names(toPlot)
names(toPlot) <- rownames(toPlot) <- c("Salinity (S)", "Temperature (S)", "Temperature seasonality (S)", "Salinity seasonality (S)", "DIN (S)", "Detritus (S)", "Chlorophyll a (S)", 
                                       "Salinity (B)", "Temperature (B)", "Temperature seasonality (B)", "Salinity seasonality (B)", "DIN (B)", "Detritus (B)", "Chlorophyll a (B)", 
                                       "Bathymetry","Fishing effort")


library(ggcorrplot)
# Green for surface variables, orange for bottom, black for non applicable
environment_plot <- ggcorrplot(toPlot, show.diag = T, hc.order = F, type = "lower", legend.title = "Correlation",
                               outline.col = "lightgrey", lab = TRUE, digits = 2) +
  scale_fill_stepsn(name = "Correlation", breaks = c(-1, -0.7, 0.7, 1), limits = c(-1, 1), show.limits = T,
                    colours = c('#003366','white','#E31B23')) +
  theme(axis.text.x = element_text(colour = c("#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73",
                                              "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00",
                                              "#000000", "#000000", "#000000")),
        axis.text.y = element_text(colour = c("#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73",
                                              "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00",
                                              "#000000", "#000000", "#000000")),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size = 14), #change legend title font size
        legend.text = element_text(size = 10))

tiff("figures/Figure_suppl_1.tiff", units = "mm", width = 300, height = 250, res=300)
environment_plot
dev.off()


# Supplementary figure 2. MCMC convergence --------------------------------
# Potential scale reduction factor
mpostfullPa <-  convertToCodaObject(models$fullPa, spNamesNumbers = c(T,F), covNamesNumbers = c(T,F))
betaGD <- gelman.diag(mpostfullPa$Beta, multivariate=FALSE)$psrf
gammaGD <- gelman.diag(mpostfullPa$Gamma, multivariate=FALSE)$psrf

# Effective sample size
betaESS <- effectiveSize(mpostfullPa$Beta)
gammaESS <- effectiveSize(mpostfullPa$Gamma)


par(mfrow = c(1,2))
vioplot(betaESS, ylim = c(0.95, max(betaESS)), ylab = "Effective sample size", xaxt = "n")
mtext("a", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Mean effective sample size = ", round(mean(betaESS), 2), "\n Actual sample size = ", nChains*samples), cex = 1, side = 1, line = 2)
vioplot(gammaESS, ylim = c(0.95, max(gammaESS)), ylab = "Effective sample size", xaxt = "n")
mtext("a", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Mean effective sample size = ", round(mean(gammaESS), 2), "\n Actual sample size = ", nChains*samples), cex = 1, side = 1, line = 2)


library(vioplot)
library(colorspace)
tiff("figures/Figure_suppl_2.tiff", units = "mm", width = 300, height = 150, res = 300)
par(mfrow = c(2,2))
vioplot(betaGD, ylim = c(0.95, max(betaGD)), ylab = "PSRF", xaxt = "n")
axis(1, at = 1:2, labels = c("", ""))
mtext("a", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Point estimate\n(mean = ", round(mean(betaGD[,1]), 2), ")"), at = 1, side = 1, line = 2)
mtext(paste0("Upper confidence interval\n(mean = ", round(mean(betaGD[,2]), 2), ")"), at = 2, side = 1, line = 2)
vioplot(gammaGD, ylim = c(0.99, 1.2), ylab = "", xaxt = "n")
axis(1, at = 1:2, labels = c("", ""))
mtext("b", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Point estimate\n(mean = ", round(mean(gammaGD[,1]), 2), ")"), at = 1, side = 1, line = 2)
mtext(paste0("Upper confidence interval\n(mean = ", round(mean(gammaGD[,2]), 2), ")"), at = 2, side = 1, line = 2)
vioplot(betaESS, ylim = c(0.95, max(betaESS)), ylab = "Effective sample size", xaxt = "n")
mtext("c", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Mean effective sample size = ", round(mean(betaESS), 2), "\n Actual sample size = ", nChains*samples), cex = 1, side = 1, line = 2)
vioplot(gammaESS, ylim = c(0.95, max(gammaESS)), ylab = "", xaxt = "n")
mtext("d", cex = 2, side = 3, line = -3, adj = 0.05)
mtext(paste0("Mean effective sample size = ", round(mean(gammaESS), 2), "\n Actual sample size = ", nChains*samples), cex = 1, side = 1, line = 2)
par(mfrow=c(1,1))
dev.off()

min(betaGD)
min(gammaGD)

# Supplementary figure 3. Discriminatory power ----------------------------
library(colorspace)
epower <- paste0("Model fit/", "Expl_power_model_chains_",as.character(nChains), "_samples_",as.character(samples), "_thin_",as.character(thin))
load(epower)
crossval <- paste0("Model fit/", "Crossvlidation_5fold_model_chains_",as.character(nChains), "_samples_",as.character(samples), "_thin_",as.character(thin))
load(crossval)
mean(MF_cross$AUC)


dPower <- data.frame(Species = rep(models$fullPa$spNames, times = 2),
                     AUC = c(EPow$fullPa$AUC, MF_cross$AUC),
                     TjurR2 = c(EPow$fullPa$TjurR2, MF_cross$TjurR2),
                     RMSE = c(EPow$fullPa$RMSE, MF_cross$RMSE),
                     Power = rep(c("Explanatory power", "Predictive power"), each = length(models$fullPa$spNames)))

AUCorder <- dPower%>%
  filter(Power == "Explanatory power")%>%
  arrange(desc(AUC))
Tjurorder <- dPower%>%
  filter(Power == "Explanatory power")%>%
  arrange(desc(TjurR2))
RMSEorder <- dPower%>%
  filter(Power == "Explanatory power")%>%
  arrange(desc(RMSE))

dAUC <- dPower%>%
  arrange(factor(Species, levels = AUCorder$Species))%>%
  mutate(orderCol = rep(c(1:67), each = 2))
dTjur <- dPower%>%
  arrange(factor(Species, levels = Tjurorder$Species))%>%
  mutate(orderCol = rep(c(1:67), each = 2))
dRMSE <- dPower%>%
  arrange(factor(Species, levels = RMSEorder$Species))%>%
  mutate(orderCol = rep(c(1:67), each = 2))


modelPower <- ggpubr::ggarrange(
  ggplot(dAUC, aes(x = orderCol, y = AUC, fill = Power)) + geom_bar(width = 0.8, stat="identity", position = "dodge") +
    labs(x = "", fill = "", tag = "(a)") +
    scale_fill_manual(values = c("black", "grey")) +
    coord_cartesian(ylim = c(0.7, 1)) + 
    theme_bw() +
    theme(legend.position = c(0.85, 0.95)),
  ggplot(dTjur, aes(x = orderCol, y = TjurR2, fill = Power)) + geom_bar(width = 0.8, stat="identity", position = "dodge") +
    labs(x = "", fill = "", tag = "(b)") +
    scale_fill_manual(values = c("black", "grey")) +
    theme_bw() +
    theme(legend.position = c(0.85, 0.95)),
  ggplot(dRMSE, aes(x = orderCol, y = RMSE, fill = Power)) + geom_bar(width = 0.8, stat="identity", position = "dodge") +
    labs(x = "Species", fill = "", tag = "(b)") +
    scale_fill_manual(values = c("black", "grey")) +
    theme_bw() +
    theme(legend.position = c(0.85, 0.95)),
  common.legend = TRUE, ncol = 1, nrow = 3)



tiff("figures/Figure_suppl_3.tiff", units = "mm", width = 200, height = 300, res = 300)
modelPower
dev.off()


# Table with mean results
dTable <- dPower%>%
  group_by(Power)%>%
  summarise(AUC = round(mean(AUC), 3),
            TjurR2 = round(mean(TjurR2), 3),
            RMSE = round(mean(RMSE), 3))
dTable



overfitCheck <- dPower <- data.frame(Species = models$fullPa$spNames,
                                     AUC_diff = c(EPow$fullPa$AUC - MF_cross$AUC),
                                     TjurR2 = c(EPow$fullPa$TjurR2 - MF_cross$TjurR2),
                                     RMSE = c(EPow$fullPa$RMSE - MF_cross$RMSE))



# Supplementary figure 4. Response to environment by traits -----------------------------
round(head(models$fullPa$X),2)
colnames(models$fullPa$X)
groupnames <- c("Climate", "Nutrients.productivity", "Sediment", "Fishing") # Define the groups of variables
group <- c(1,1,1,1,1,
           2,2,2,
           3,3,3,3,
           4) # Assign each variable to a group)

variancePartitioning <- computeVariancePartitioning(models$fullPa, group = group, groupnames = groupnames)
trait.beta <- data.frame(FullPa = c(variancePartitioning$R2T$Beta*100))
trait.beta <- round(trait.beta, 2)
trait.beta$Environment <- c("(Intercept)","SBT", "SBT2", "SBT seasonality", "SBS seasonality", "Detritus", "Chlorophyll a", "DIN", 
                            "Sand", "Coarse substrate", "Mixed sediment", "Rock and boulders", "Fishing")

trait.beta <- trait.beta%>%
  mutate(Environment = as.factor(Environment))%>%
  arrange(-FullPa)
# Reset level order according to Proportion order
trait.beta$Environment <-  factor(trait.beta$Environment, levels = trait.beta$Environment)
levels(trait.beta$Environment)

trait.beta

tiff("figures/Figure_suppl_4.tiff", units = "mm", width = 200, height = 100, res = 300)
par(mar = c(6.1, 4.1, 2.1, 2.1))
plt <- barplot(trait.beta$FullPa, xaxt = "n", ylab = "%")
text(plt, par("usr")[3], labels = paste(trait.beta$Environment), srt = 45, adj = c(1.1, 1.1), xpd = TRUE) 
dev.off()



# Supplementary figure 5. Variance partitioning by species ----------------
round(head(models$fullPa$X),2)
colnames(models$fullPa$X)
groupnames <- c("Temperature", "Salinity", "Nutrients.productivity", "Sediment", "Fishing") # Define the groups of variables
group <- c(1,1,1,1,
           2,
           3,3,3,
           4,4,4,4,
           5) # Assign each variable to a group)
# The intercept can be assigned to either group as it doesn't make any difference
VPfull <- computeVariancePartitioning(models$fullPa, group = group, groupnames = groupnames)
VPfull <- data.frame(t(VPfull$vals))
head(VPfull)
VPfull$Species <- rownames(VPfull)
VPfull <- reshape2::melt(VPfull%>%group_by(Species)%>%
                           summarise(
                             Temperature = Temperature,
                             Nutrients.productivity = Nutrients.productivity,
                             Fishing = Fishing,
                             Salinity = Salinity,
                             Sediment = Sediment,
                             StatRec = Random..StatRec, Year = Random..Year, Season = Random..Season),
                         id = "Species")



suppl5 <- ggplot(VPfull, aes(x=Species, y=value, fill=variable)) + geom_bar(position="fill", stat="identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Set3",
                    labels=c(
                      Temperature = paste0("Temperature (", round(mean(
                        VPfull$value[which(VPfull$variable == "Temperature")])*100), ")"), 
                      Nutrients.productivity = paste0("Productivity (", round(mean(
                        VPfull$value[which(VPfull$variable == "Nutrients.productivity")])*100), ")"), 
                      Sediment = paste0("Sediment (", round(mean(
                        VPfull$value[which(VPfull$variable == "Sediment")])*100), ")"),
                      Fishing = paste0("Fishing (", round(mean(
                        VPfull$value[which(VPfull$variable == "Fishing")])*100), ")"),
                      Salinity = paste0("Salinity (", round(mean(
                        VPfull$value[which(VPfull$variable == "Salinity")])*100), ")"),
                      StatRec = paste0("ICES\nRectangle (", round(mean(
                        VPfull$value[which(VPfull$variable == "StatRec")])*100), ")"),
                      Season = paste0("Season (", round(mean(
                        VPfull$value[which(VPfull$variable == "Season")])*100), ")"),
                      Year = paste0("Year (", round(mean(
                        VPfull$value[which(VPfull$variable == "Year")])*100), ")")
                    )) +
  labs(#title = "Variance partitioning", 
    y = "Variance proportion", fill = "Group variance")

suppl5

tiff("figures/Figure_suppl_5.tiff", units = "mm", width = 300, height = 200, res = 300)
suppl5
dev.off()



# Supplementary figure 6. Beta coefficients -----------------------------------------------------
postBeta <-  getPostEstimate(models$fullPa, parName = "Beta")
supportLevel <- 0.95

plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))


# ggplot beta and gamma plot
library(ggimage)
library(ggtree)
# here the complete guide for ggtree https://yulab-smu.top/treedata-book/chapter8.html

# phylogeny
# data$phy is the df you give the model for the phylogeny 
load("Data sources/fish_phylogenetic_tree.RData")
fish_phylo$tip.label <- gsub("_", " ", fish_phylo$tip.label)
setdiff(colnames(models$fullPa$Y), fish_phylo$tip.label)


par(mfrow = c(1,1), mar = c(4, 10, 3, 0))
plot(fish_phylo, type = "phylogram", show.node.label = TRUE)
par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

tree_plot_a <- ggtree(fish_phylo, branch.length="none") + geom_tiplab(fontface = "italic") + xlim(0, 30)
tree_plot_a
# tree_plot_b <- ggtree(fish_phylo) + geom_tiplab(fontface = "italic", align = TRUE) + xlim(0, 4.5)
# tree_plot_b



### NEED TO CHECK if the precedure is correct! Are the +/- responses correct?
# beta heatmap
postBeta <-  getPostEstimate(models$fullPa, parName = "Beta")

temp.beta <- as.data.frame(postBeta$mean) # 'temp' stands for temporary
temp.beta$param <- models$fullPa$covNames
temp.beta 
temp.beta <- temp.beta %>% tidyr::pivot_longer(.,-param, names_to = 'species', values_to ='value')

sup.beta <- as.data.frame(postBeta$support) # n.beta because I'm extracting the negative values
sup.beta$param <- models$fullPa$covNames
sup.beta 
sup.beta <- sup.beta %>% tidyr::pivot_longer(.,-param, names_to = 'species', values_to ='value')

temp.beta$support <- sup.beta$value

supportLevel <- 0.95
temp.beta$coeff <- temp.beta$value


# Standardize values:
temp.beta <- temp.beta%>%
  group_by(param)%>%
  mutate(coeff_standard = scale(coeff)[,1],
         coeff_standard_0 = coeff_standard)
ggplot(temp.beta, aes(x = coeff_standard, color = param)) + geom_density()

temp.beta$coeff_standard_0[temp.beta$support < supportLevel & temp.beta$support > (1-supportLevel)] <- 0 # Not supported to 0


ggplot(temp.beta, aes(x = coeff_standard_0, color = param)) + geom_density()


windows(800, 600, pointsize = 12); #opens a separate window with the size you want
par(mar = c(4, 12, 0, 0));
plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))

windows(800, 600, pointsize = 12); #opens a separate window with the size you want
ggplot(temp.beta, aes(x = param, y = species)) +
  geom_tile(aes(fill = coeff_standard_0), color ='black')+
  scale_fill_gradient2(low = '#003366', mid = "white", high = '#E31B23', name = '') + xlab('') + ylab('')+
  geom_tile(data = temp.beta[temp.beta$coeff_standard_0 == 0,], fill = "grey", color='black')+
  theme_bw()+
  theme(text = element_text(family = "sans",size = 12),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank())


# NOTE THAT the order of the environmental variables is different and that species are sorted opposite

# Seems about right!



# Rename variables
temp.beta$Environment <- factor(temp.beta$param)
levels(temp.beta$Environment)
temp.beta$Environment <- dplyr::recode(temp.beta$Environment, 
                                       "Detritus_floor" = "Detritus",
                                       "DIN_surf" = "DIN",
                                       "Fishing_lag" = "Fishing",
                                       "Phy_surf" = "Chlorophyll a",
                                       "poly(SBT, degree = 2, raw = TRUE)1" = "SBT",
                                       "poly(SBT, degree = 2, raw = TRUE)2" = "SBT2",
                                       "SBS_seasonality" = "SBS seasonality",
                                       "SBT_seasonality" = "SBT seasonality",
                                       "SedimentCoarse substrate" = "Coarse substrate",
                                       "SedimentMixed sediment" = "Mixed sediment",
                                       "SedimentROck and boulders" = "Rock and boulders",
                                       "SedimentSand" = "Sand")
temp.beta$Environment <-  factor(temp.beta$Environment,
                                 levels = c("(Intercept)", "SBT", "SBT2", "SBT seasonality", "SBS seasonality",
                                            "Detritus", "Chlorophyll a", "DIN",
                                            "Sand", "Coarse substrate", "Mixed sediment", "Rock and boulders", 
                                            "Fishing"))
levels(temp.beta$Environment)

# Set species order same as phylogenetic tree
sp_order <- get_taxa_name(tree_plot_b)
temp.beta$Species_sort <- as.factor(temp.beta$species)
temp.beta$Species_sort <-  factor(temp.beta$Species_sort, levels = rev(sp_order)) # Needs to be reversed
levels(temp.beta$Species_sort)



beta.heatmap <- ggplot(temp.beta, aes(x = Environment, y = Species_sort)) +
  geom_tile(aes(fill = coeff_standard_0), color ='black')+
  scale_fill_gradient2(low = '#003366', mid = "white", high = '#E31B23', name = 'Standardized\neffect size') + 
  xlab('') + ylab('')+
  geom_tile(data = temp.beta[temp.beta$coeff_standard_0 == 0,], fill = "grey50", color='black')+
  theme_bw()+
  theme(text = element_text(family = "sans",size = 12),
        strip.text = element_text(family = "sans",face = 'bold'),
        axis.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(), strip.background = element_blank(),
        axis.text.y  = element_blank())
# scale_x_discrete(limits=c('Intercept','Seabed: sand','Depth', "Bottom temperature"), expand=c(0,0))
# scale_y_discrete(limits=rev(c('Brown skate','Starry skate','Thornback skate','Marbled torpedo','Common eagle ray','Small-spotted catshark','Nursehound','Smooth-hound','Spurdog')),expand=c(0,0),position='right') # position ='right' is needed for having a nicer viz


library(patchwork)
# Check if species names match
tree_plot_a + beta.heatmap + plot_layout(widths = c(2, 3))
tree_plot_a$data$label <- paste0("  ", tree_plot_a$data$label) # add space between tree and sp names
tree_plot_a + beta.heatmap + plot_layout(widths = c(2, 3))

# If they match, continue

tiff("figures/Figure_suppl_6.tiff", units="mm", width = 300, height = 300, res=300)
tree_plot_a + beta.heatmap + plot_layout(widths = c(2, 3))
dev.off()

par(mar = c(4, 12, 0, 0))
plotBeta(models$fullPa, postBeta, main = paste0("Beta\n", "support level ", supportLevel), supportLevel = supportLevel, param = "Sign", plotTree = FALSE, spNamesNumbers = c(T,F), covNamesNumbers = c(T, FALSE), colors = colorRampPalette(c("deepskyblue2","white","firebrick1")))






# Supplementary figure 7. Trait - environment plots -----------------------
#Construct gradient where you define the variable you want to plot against, here "enviornmental variable"
m <- models$fullPa
models$fullPa$XFormula

Gradient <- constructGradient(m, focalVariable = "Phy_surf", ngrid = 20)
predYgradient <- predict(m, XData = Gradient$XDataNew, studyDesign = Gradient$studyDesignNew, ranLevels = Gradient$rLNew, expected = TRUE)
tiff("figures/Figure_suppl_7a.tiff", units = "mm", width = 400, height = 400, res = 300)
# Chlorophyll a
par(mfrow = c(4,3), mar = c(5.1, 5.1, 4.1, 2.1))
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 2, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Bathypelagic (habitat)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 3, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Benthopelagic (habitat)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 4, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Demersal (habitat)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 5, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Pelagic (habitat)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 6, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Eel-like (body shape)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 7, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Elongated (body shape)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 8, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Flat (body shape)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 9, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Fusiform (body shape)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 10, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Pointed (fin shape)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 11, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Rounded (fin shape)", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 12, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Truncate (fin shape)", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 13, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Guarder (spawning type)", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
dev.off()

tiff("figures/Figure_suppl_7b.tiff", units = "mm", width = 400, height = 400, res = 300)
par(mfrow = c(4,3), mar = c(5.1, 5.1, 4.1, 2.1))
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 14, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Non-guarder (spawning type)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 15, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Generalist (feeding mode)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 16, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Piscivorous (feeding mode)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 17, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Planktivorous (feeding mode)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 18, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "log(Age at maturity)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 19, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "log(Maximum length)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 20, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "log(Offspring size)", xlabel = "", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 21, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "log(Fecundity)", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 22, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "log(Growth (K))", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
plotGradient(m, Gradient, pred = predYgradient, measure = "T", index = 23, showData = T, cicol = rgb(0.5,0.5,0, alpha = 0.8), ylabel = "Trophic level", xlabel = "Chlorophyll a", cex.lab = 2, cex.axis = 2)
dev.off()