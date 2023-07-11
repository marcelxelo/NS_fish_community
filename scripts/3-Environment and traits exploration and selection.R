#########################################!
#### Environmental covariates and trait data exploration
#### 
#### Marcel Montanyes, DTU Aqua, 08-12-2021
#######################################!

library(ggplot2)
library(dplyr)


# 1 - ENVIRONMENTAL VARIABLES ---------------------------------------------
load("My data/numcpue_nemo_medusa_fishing.RData")
Data <- numcpue_nm_f

# Check variables
names(Data[1:45])

# All NAs are accounted for and known:
sum(is.na(Data)) == 
  sum(is.na(Data$TotalFishingHours)) + 
  sum(is.na(Data$BeamHours)) + 
  sum(is.na(Data$OtterHours)) +
  sum(is.na(Data$TotalFishingHours_lag)) + 
  sum(is.na(Data$BeamHours_lag)) + 
  sum(is.na(Data$OtterHours_lag)) +
  sum(is.na(Data$Area.swept))*(279-31) +
  sum(is.na(Data$Sediment_type)) +
  sum(is.na(Data$Sediment_ID))

# Some observations with bottom salinity = 0; it comes straight from the NEMO-Medusa data (not a mistake of mine)
ggplot(Data, aes(x = ShootLong, y = ShootLat, color = Salinity_floor > 0)) + geom_point(size = 1.5) +
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim = range(Data$ShootLong), ylim = range(Data$ShootLat))


Data <- Data[!is.na(Data$Area.swept) &
               !is.na(Data$Sediment_type) &
               !is.na(Data$TotalFishingHours_lag) &
               Data$Salinity_floor > 0,] # We can't use those observations without data

# levels(Data$Sediment_type)
nrow(Data[Data$Sediment_type == "No data at this level of folk",])
Data <- droplevels(subset(Data, Sediment_type != "No data at this level of folk"))


# Select Species
my_species <- as.vector(na.omit(names(Data)[c(which(colnames(Data) == "Area.swept")+1:ncol(Data))]))


# a) Check for correlated variables ---------------------------------------
cor.all <- as.data.frame(cor(Data[,c(# "Year", "Month",
  "Salinity_surf", "Temperature_surf", "Tseasonlaity_surf", "Sseasonlaity_surf",
  "DIN_surf", "Detritus_surf", "Phytoplankton_surf", #"Meridional_surf", "Zonal_surf",
  "Salinity_floor", "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
  "DIN_floor", "Detritus_floor", "Phytoplankton_floor",#"Meridional_floor", "Zonal_floor",
  "Bathymetry", "TotalFishingHours_lag")]))
Over.7 <- round(cor.all,2)
Over.7[abs(Over.7) < 0.7] <- NA
View(Over.7)

test <- Over.7
# test <- filter(test, rowSums(is.na(test)) != ncol(test))

# flattenCorrMatrix <- function(cormat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut]
#   )
# }
# upper.tri(test)
# testFlat <- flattenCorrMatrix(test)
# ggplot(testFlat, aes(x = row, y = column, fill = cor)) + geom_tile()


toPlot <- round(cor.all,2)
# toPlot[abs(toPlot) < 0.7] <- 0
# plotOrder <-  corrMatOrder(toPlot, order="AOE") # Euclidian distances

# corrplot(as.matrix(toPlot[plotOrder,plotOrder]), method = "color", col = colorRampPalette(c("deepskyblue2",rep("white", 7),"firebrick1"))(20), mar = c(0,0,0,0), type = "lower", tl.col="black", tl.cex = 1)
all(names(toPlot) == rownames(toPlot))
names(toPlot)
names(toPlot) <- rownames(toPlot) <- c("Salinity (S)", "Temperature (S)", "Temperature seasonality (S)", "Salinity seasonality (S)", "DIN (S)", "Detritus (S)", "Chlorophyll a (S)", 
                                       "Salinity (B)", "Temperature (B)", "Temperature seasonality (B)", "Salinity seasonality (B)", "DIN (B)", "Detritus (B)", "Chlorophyll a (B)", 
                                       "Bathymetry","Fishing effort")


library(ggcorrplot)
ggcorrplot(toPlot, show.diag = T, hc.order = F, type = "lower", legend.title = "Correlation",
           outline.col = "lightgrey", colors = c("deepskyblue2", "white","firebrick1"), lab = TRUE) +
  scale_fill_stepsn(name = "Correlation", breaks = c(-1,-0.7,0.7,1), limits = c(-1,1), show.limits = T,
                    colours = c("deepskyblue2","white","firebrick1"))



# Green for surface variables, orange for bottom, black for non applicable
ggcorrplot(toPlot, show.diag = T, hc.order = F, type = "lower", legend.title = "Correlation",
           outline.col = "lightgrey", colors = c("deepskyblue2", "white","firebrick1"), lab = TRUE, digits = 2) +
  scale_fill_stepsn(name = "Correlation", breaks = c(-1,-0.7,0.7,1), limits = c(-1,1), show.limits = T,
                    colours = c("deepskyblue2","white","firebrick1")) +
  theme(axis.text.x = element_text(colour = c("#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73",
                                                                     "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00",
                                                                     "#000000", "#000000", "#000000")),
        axis.text.y = element_text(colour = c("#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73",
                                              "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00",
                                              "#000000", "#000000", "#000000")),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10))


# Temperature_floor - Temperature_surf OK
# Tseasonlaity_floor - corr with salinity_floor, salinity_surf, DIN_floor, Bathymetry SHIT
# Salinity_floor - DIN_floor, Bathymetry, Tseasonality_surf, Tseasonality_floor SHIT
# Sseasonlaity_floor - Sseasonality_surf OK
# Detritus_floor - // OK
# Phytoplankton_surf - Detritus_surf OK
# DIN_surf - // OK
# Sediment_type - NA OKS
# Bathymetry - corr with Phyto_floor, Tseasonality_surf, Tseasonality_floor, salinity_surf, salinity_floor, DIN_floor SHIT
# TotalFishingHours_lag - // OK

XFormula <-  ~  poly(SBT,degree = 2,raw = TRUE) + SBT_seasonality +
  SBS_seasonality +
  Detritus_floor + Phy_surf + DIN_surf + 
  Sediment + Fishing_lag
# Maybe also include DEPTH: although is correlated with other variables it's a good proxy for missing covariates


# Limit set at 0.7:
# According to the correlation matrix, DIN_surf, Salinity_floor and Detritus_floor are not correlated with anythig.
# Then we have that seafloor and surface temperature are positively correlated; and so are Phytoplankton_surf and Detritus_surf
# Lastly, Salinity_surf, DIN_floor, Phytoplankton_floor and Bathymetry are all positively correlated.



cor.check <- as.data.frame(cor(Data[,c(# "Year", "Month",
  "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
  "Detritus_floor", "Phytoplankton_surf", "DIN_surf", 
  "TotalFishingHours_lag" #, "Bathymetry"
  )]))
Over72 <- round(cor.check,2)
Over72[abs(Over72) < 0.7] <- 0
View(Over72)

ggcorrplot(Over72, show.diag = T, hc.order = TRUE, type = "lower", legend.title = "Correlation",
           outline.col = "lightgrey", colors = c("deepskyblue2", "white","firebrick1"), lab = TRUE) +
  scale_fill_stepsn(name = "Correlation", breaks = c(-1,-0.7,0.7,1), limits = c(-1,1), show.limits = T,
                    colours = c("deepskyblue2","white","firebrick1"))






cor.red <- as.data.frame(cor(Data[,c(# "Year", "Month",
  "DIN_surf", "Phytoplankton_surf",
  "Salinity_floor", "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
  "DIN_floor", "Detritus_floor", "Phytoplankton_floor","Meridional_floor", "Zonal_floor",
  "Bathymetry", "HaulDur")]))
Over72 <- round(cor.red,2)
Over72[abs(Over72) < 0.7] <- NA
View(Over72)
# Tseasonality_floor, DIN_floor, Phytoplankton_floor & Bathymetry are highly correlated. Might be interesting to choose only one of those.



cor.redoble <- as.data.frame(cor(Data[,c(# "Year", "Month",
  "DIN_surf", "Phytoplankton_surf",
  "Salinity_floor", "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
  "Detritus_floor","Meridional_floor", "Zonal_floor",
  "HaulDur")]))
Over73 <- round(cor.redoble,2)
Over73[abs(Over73) < 0.7] <- NA
View(Over73)
# Non of the above are correlated


library(vegan)
envPCA <- rda(Data[,c("DIN_surf", "Phytoplankton_surf",
                      "Salinity_floor", "Temperature_floor", "Tseasonlaity_floor", "Sseasonlaity_floor",
                      "Detritus_floor","Meridional_floor", "Zonal_floor",
                      "HaulDur")],
              scale = TRUE)
envPCAsum <- summary(envPCA)
head(envPCAsum)
envPCAsum$cont$importance
envPCAsum$species
propExpl <- as.data.frame(round(envPCAsum$cont$importance,3))
envPCAsum$species[order(envPCAsum$species[,1]),1:2]
par(mfrow=c(1,2),mar=c(4,4,2,2))
biplot(envPCA, display = c("species"), type = c("text"), cex = 2, main="Environment",
       xlab = paste0("PC1 ", "(",round(propExpl$PC1[2],3)*100, "%)"),
       ylab = paste0("PC2 ", "(",round(propExpl$PC2[2],3)*100, "%)"))
biplot(envPCA, choices = c(1,3),  display = c("species"), type = c("text"), cex = 2, main="Environment",
       xlab = paste0("PC1 ", "(",round(propExpl$PC1[2],3)*100, "%)"),
       ylab = paste0("PC3 ", "(",round(propExpl$PC3[2],3)*100, "%)"))
par(mfrow=c(1,1),mar=c(4,4,0.2,2))



# Are the different sampling gears related to depth?
# Data$Gear <- as.factor(Data$Gear)
# model <- lm(Bathymetry~Gear, Data)
# anova(model) # It seems that gears change with depth
# summary(model)
# 
# library(multcomp)
# gear <- summary(model)$coef
# my.coefficients<-data.frame(gear[,1:2],lower=gear[,1]-1.96*gear[,2],upper=gear[,1]+1.96*gear[,2])
# names(my.coefficients) <- c("Estimate", "SE", "Lower CI", "Upper CI")
# #round to two decimals:
# round(my.coefficients,digits=2)
# tuk <- glht(model, linfct = mcp(Gear = "Tukey"))
# summary(tuk)
# tuk.cld <- cld(tuk)
# # Plot the compact-letter-display:
# old.par <- par(no.readonly=TRUE) # Save current graphics parameters
# par(mai=c(1,1,1.5,1)) # Use sufficiently large upper margin such that the letters can be seen
# plot(tuk.cld)
# par(old.par)






# 2 - SPECIES TRAITS ------------------------------------------------------
Trait_collection <- read.table("Data Sources/Fish traits/TraitCollectionFishNAtlanticNEPacificContShelf.txt", 
                               header=TRUE, sep = "\t", dec=",", fill= TRUE, na.strings=c("","NA"))

a_trait <- read.table("Data sources/Fish traits/NS_traits_working.txt", header=TRUE, sep = "\t", dec=",", fill= TRUE)
# a_trait <- a_trait%>%select(species, trophic_level)
# write.table(a_trait, file = "Data sources/Fish traits/NS_traits_working.txt", sep = "\t", dec=",")
a_trait$Species <-  gsub("_", " ", a_trait$species)
a_trait$species <- NULL
#Add Burrow's temperature as another trait
# Burrow <- read.csv("Data sources/Fish traits/Burrows_Species range temperature.csv")
# Burrow$X <- NULL
# a_trait <- merge(a_trait, Burrow[, c("speciesName","en4sbtwannp50")], by.x="Species", by.y="speciesName", all.x=TRUE, all.y = FALSE)
# rownames(a_trait) <- NULL

sort(names(Trait_collection))
# Select traits of interest:
# Morphological: Body size, body shape, caudal fin shape
# Life history: age at maturity
# Reproductive: offspring size, fecundity, spawning behavior
# Dietary: diet

myTraits <- merge(Trait_collection[,c("taxon", 
                                      "body.shape", "fin.shape", "length.max", # Morphological; body size missing, instead i chose length maximum
                                      "age.maturity", # Life history
                                      "offspring.size", "fecundity", "spawning.type",  # Reproductive
                                      "feeding.mode", # Dietary
                                      "growth.coefficient")],
                  a_trait[,c("Species",
                               "trophic_level" # Dietary, Life history "en4sbtwannp50")], # Temperature
                  )],
                  by.x = "taxon", by.y = "Species",
                  all = TRUE)

names(myTraits)
names(myTraits)[1] <- "Species"

# Reassign variable class
# myTraits$body.shape <- as.factor(myTraits$body.shape)
# myTraits$fin.shape <- as.factor(myTraits$fin.shape)
# myTraits$length.max <- as.numeric(myTraits$length.max)
myTraits$age.maturity <- as.numeric(gsub(",", ".", myTraits$age.maturity))
# myTraits$offspring.size <- as.numeric(myTraits$offspring.size)
# myTraits$fecundity <- as.numeric(myTraits$fecundity)
# myTraits$spawning.type <- as.factor(myTraits$spawning.type)
# myTraits$feeding.mode <- as.factor(myTraits$feeding.mode)

# Some numerical traits have several values. Calculate the mean
myTraits <- myTraits%>%group_by(Species, body.shape, fin.shape, spawning.type, feeding.mode)%>%
  summarise(age.maturity = mean(age.maturity, na.rm=T),
            length.max = mean(length.max, na.rm=T),
            offspring.size = mean(offspring.size, na.rm=T),
            fecundity = mean(fecundity, na.rm=T),
            growth.coefficient = mean(growth.coefficient, na.rm = T),
            trophic_level = mean(trophic_level, na.rm=T),
            # en4sbtwannp50 = mean(en4sbtwannp50, na.rm=T),
            log_age.maturity = log(mean(age.maturity, na.rm=T)),
            log_length.max = log(mean(length.max, na.rm=T)),
            log_offspring.size = log(mean(offspring.size, na.rm=T)),
            log_fecundity = log(mean(fecundity, na.rm=T)),
            log_growth = log(mean(growth.coefficient, na.rm=T))
  )




# Select species
load("My data/numcpue_nemo_medusa_fishing.RData")
Data <- numcpue_nm_f
# Check variables
names(Data[1:40])

# All NAs are accounted for and known:
sum(is.na(Data)) == sum(is.na(Data$TotalFishingHours)) + # No fishing data otside 1985-2015
  sum(is.na(Data$BeamHours)) + sum(is.na(Data$OtterHours)) +
  sum(is.na(Data$Area.swept))*(278-30) # Some missing swept area data, and therefore, missing cpue for each species

Data <- Data[!is.na(Data$Area.swept),] # We can't use those observations without data

# Select Species
my_species <- as.vector(na.omit(names(Data)[c(which(colnames(Data) == "Area.swept")+1:ncol(Data))]))

# Species:
YData <- Data[, c("Year", "Season", "StatRec", my_species)]
YData[,my_species] <- as.numeric(YData[,my_species] > 0)

library(dplyr)
Time <- YData[, c("Year", my_species)]%>%
  group_by(Year)%>%
  summarise(across(.cols = my_species, sum))
Time$Year <- NULL

Time <- colSums(Time > 0)
Time <- as.data.frame(Time)
sp.time <- rownames(Time)[Time$Time > 9]

Space <- YData[, c("StatRec", my_species)]%>%
  group_by(StatRec)%>%
  summarise(across(.cols = my_species, sum))
# sort(Space$`Salmo salar`, decreasing = T)
Space$StatRec <- NULL

Space <- colSums(Space > 0)
Space <- as.data.frame(Space)
sp.space <- rownames(Space)[Space$Space > 9]


filtred.sp <- intersect(sp.time, sp.space)
length(filtred.sp)

# View(colSums(YData[,filtred.sp] > 0)) # Total presences/species
# sort(YData$`Conger conger`, decreasing = T)



# Redefine trait data

myTraits[sapply(myTraits, is.nan)] <- NA
sum(is.na(myTraits))
nrow(myTraits)
TrData <- na.omit(myTraits)
nrow(TrData)

TrData <- subset(TrData, Species %in% filtred.sp)
nrow(TrData)
TrData$Species[duplicated(TrData$Species)] 
# View(TrData[which(TrData$Species %in% TrData$Species[duplicated(TrData$Species)]),])
TrData <- droplevels(TrData%>%filter(!(Species %in%  TrData$Species[duplicated(TrData$Species)])))
rownames(TrData) <- TrData$Species
nrow(TrData)

final_sp <- as.vector(TrData$Species)




# cor.tr <- as.data.frame(cor(TrData[ ,c("age.maturity", "length.max", "offspring.size", "fecundity", "growth.coefficient", "trophic_level", "en4sbtwannp50")]))
cor.tr <- as.data.frame(cor(TrData[ ,c("log_age.maturity", "log_length.max", "log_offspring.size",
                                       "log_fecundity", "log_growth", "trophic_level", "en4sbtwannp50")]))
tr.7 <- round(cor.tr,2)
tr.7[abs(tr.7) < 0.7] <- 0
tr.7


# ggcorrplot(tr.7, show.diag = T, hc.order = TRUE, type = "lower", legend.title = "Correlation",
#            outline.col = "lightgrey", colors = c("deepskyblue2", "white","firebrick1"), lab = TRUE) +
#   scale_fill_stepsn(name = "Correlation", breaks = c(-1,-0.7,0.7,1), limits = c(-1,1), show.limits = T,
#                     colours = c("deepskyblue2","white","firebrick1"))

