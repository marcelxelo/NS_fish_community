#########################################!
#### Prepare data input for Presence-absence HMSC model fitting
#### 
#### Marcel Montanyes, DTU Aqua, 18-01-2022
#######################################!

library(Hmsc)
library(abind)
library(ggplot2)
library(dplyr)
library(ape) # Phylogenetic tree
library(mapplots)
library(snow)
theme_set(theme_bw())

# 1 - Load data and check that everything is alright ---------------------
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

nrow(Data[Data$Sediment_type == "No data at this level of folk",])
Data <- droplevels(subset(Data, Sediment_type != "No data at this level of folk"))

deepRec <- subset(Data, Bathymetry > 250)
ggplot(deepRec, aes(x = ShootLong, y = ShootLat)) + geom_point(size = 1.5) +
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim = range(Data$ShootLong), ylim = range(Data$ShootLat))


# 2 - Select species with 10 or more obs over space & time ----------------
# Select Species
my_species <- as.vector(na.omit(names(Data)[c(which(colnames(Data) == "Area.swept")+1:ncol(Data))]))
spData <- Data[, c("Year", "Season", "StatRec", my_species)]
spData[,my_species] <- as.numeric(spData[,my_species] > 0)

# Species with 10 or more obs over time
length(unique(Data$Year))
Time <- spData[, c("Year", all_of(my_species))]%>%
  group_by(Year)%>%
  summarise(across(.cols = my_species, sum))
Time$Year <- NULL
Time <- as.data.frame(colSums(Time > 0))
sp.time <- rownames(Time)[Time[1] > 9]

# Species with 10 or more obs over space
length(unique(Data$StatRec))
Space <- spData[, c("StatRec", all_of(my_species))]%>%
  group_by(StatRec)%>%
  summarise(across(.cols = my_species, sum))
Space$StatRec <- NULL
Space <- as.data.frame(colSums(Space > 0))
sp.space <- rownames(Space)[Space[1] > 9]

selected_sp <- intersect(sp.time, sp.space)
length(selected_sp)



# 3 - Prepare species Traits ----------------------------------------------
Trait_collection <- read.table("Data Sources/Fish traits/TraitCollectionFishNAtlanticNEPacificContShelf.txt", header=TRUE, sep = "\t", dec=",", fill= TRUE, na.strings=c("","NA"))
Arn_trait <- read.table("Data sources/Fish traits/NS_traits_Arnaud_working.txt", header=TRUE, sep = "\t", dec=",", fill= TRUE)
Arn_trait$Species <-  gsub("_", " ", Arn_trait$species)
Arn_trait$species <- NULL
#Add Burrow's temperature as another trait
Burrow <- read.csv("Data sources/Fish traits/Burrows_Species range temperature.csv")
Burrow$X <- NULL
# Burrow_description <- read.csv("Data sources/Fish traits/Burrows_dataset description.csv")
# Burrow_description[4,]
Arn_trait <- merge(Arn_trait, Burrow[, c("speciesName","en4sbtwannp50")], by.x="Species", by.y="speciesName", all.x=TRUE, all.y = FALSE)
rownames(Arn_trait) <- NULL

sort(names(Trait_collection))
# Select traits of interest:
# Morphological: Body size, body shape, caudal fin shape
# Life history: age at maturity
# Reproductive: offspring size, fecundity, spawning behavior
# Dietary: diet

myTraits <- merge(Trait_collection[,c("taxon", 
                                      "habitat",
                                      "body.shape", "fin.shape", "length.max", # Morphological; body size missing, instead i chose length maximum
                                      "age.maturity", # Life history
                                      "offspring.size", "fecundity", "spawning.type",  # Reproductive
                                      "feeding.mode", # Dietary
                                      "growth.coefficient")],
                  Arn_trait[,c("Species",
                               "trophic_level", # Dietary, Life history
                               "en4sbtwannp50")], # Temperature
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
myTraits <- myTraits%>%group_by(Species, habitat, body.shape, fin.shape, spawning.type, feeding.mode)%>%
  summarise(age.maturity = mean(age.maturity, na.rm = T),
            length.max = mean(length.max, na.rm = T),
            offspring.size = mean(offspring.size, na.rm = T),
            fecundity = mean(fecundity, na.rm = T),
            growth.coefficient = mean(growth.coefficient, na.rm = T),
            trophic_level = mean(trophic_level, na.rm = T),
            en4sbtwannp50 = mean(en4sbtwannp50, na.rm = T),
            log_age.maturity = log(mean(age.maturity, na.rm = T)),
            log_length.max = log(mean(length.max, na.rm = T)),
            log_offspring.size = log(mean(offspring.size, na.rm = T)),
            log_fecundity = log(mean(fecundity, na.rm = T)),
            log_growth = log(mean(growth.coefficient, na.rm = T))
  )

# Redefine trait data
myTraits[sapply(myTraits, is.nan)] <- NA
sum(is.na(myTraits))
nrow(myTraits)
TrData <- na.omit(myTraits)
nrow(TrData)

trait_species <- intersect(TrData$Species, selected_sp)

TrData <- subset(TrData, Species %in% trait_species)
nrow(TrData); length(unique(TrData$Species))
TrData$Species[duplicated(TrData$Species)] 
# View(TrData[which(TrData$Species %in% TrData$Species[duplicated(TrData$Species)]),])
TrData <- droplevels(TrData%>%filter(!(Species %in%  TrData$Species[duplicated(TrData$Species)])))
rownames(TrData) <- TrData$Species
nrow(TrData)
final_species <- TrData$Species

# 4 - Prepare input data --------------------------------------------------
XData <- data.frame(SBT = Data$Temperature_floor,
                    SBT_seasonality = Data$Tseasonlaity_floor,
                    SBS = Data$Salinity_floor,
                    SBS_seasonality = Data$Sseasonlaity_floor,
                    Detritus_floor = Data$Detritus_floor,
                    # Meridional_floor = Data$Meridional_floor,
                    # Zonal_floor = Data$Zonal_floor,
                    
                    Phy_surf = Data$Phytoplankton_surf,
                    DIN_surf = Data$DIN_surf,
                    
                    Sediment = Data$Sediment_type,
                    Depth =  Data$Bathymetry,
                    
                    Fishing_lag = log(Data$TotalFishingHours_lag + 1))


# Y Data
table(final_species %in% colnames(Data))
YData <- Data[, colnames(Data) %in% final_species]
all(rownames(TrData) == colnames(YData))

P <- colMeans(YData > 0)
A <- colSums(YData)/sum(YData)
par(mfrow=c(1,2))
hist(P)
hist(A)
par(mfrow=c(1,1))

Ypa <- 1*(YData>0) # Presence-absence
Yabun <- log(YData + 1) # log-transform 0s and abundances



# Define our sampling structure
S <- data.frame(StatRec = Data$StatRec,
                Year = Data$Year,
                Season = Data$Season)
S[,c("Longitude", "Latitude")] <- ices.rect(S$StatRec)


# Define the study design from which the Random effects will be defined
studyDesign <- data.frame(StatRec = as.factor(S$StatRec),
                          Year = as.factor(S$Year),
                          Season = as.factor(S$Season))


# The studyDesign has to match each of the observations that we have, and therefore, we will have StatRec repeated for 
# the different time-points (year-month), and the other way arround. Therefore, the number of rows from the studyDesign 
# should be the same as for the X & Y matrices (but not the number of columns)
table(nrow(studyDesign) == c(nrow(XData), nrow(YData)))


# Define spatial random effect
xy <- data.frame(S[match(unique(S$StatRec), S$StatRec), c("Longitude", "Latitude")]) # Spatial coordinats
rownames(xy) <- unique(S$StatRec) # StatRec of those coordinates
rL.rectangle <- HmscRandomLevel(sData = xy, longlat = TRUE, sMethod = "NNGP") # Ignore warning
rL.rectangle <- setPriors(rL.rectangle, nfMin = 1, nfMax = 5) # setting the minimum and maximum number of latent variables



# Define temporal random effect
# For Month
rL.season <- data.frame(Season = unique(S$Season))
rownames(rL.season) <- rL.season$Month
sum(duplicated(rL.season))
rL.season <- HmscRandomLevel(units = rL.season$Season, longlat = FALSE)
rL.season <- setPriors(rL.season, nfMin = 1, nfMax = 5) # setting the minimum and maximum number of latent variables
# For Year
rL.year <- data.frame(Year = unique(S$Year))
rownames(rL.year) <- rL.year$Year
sum(duplicated(rL.year))
rL.year <- HmscRandomLevel(units = rL.year$Year, longlat = FALSE)
rL.year <- setPriors(rL.year, nfMin = 1, nfMax = 5) # setting the minimum and maximum number of latent variables



# 5 - Load taxonomic data -------------------------------------------------
library(fishtree)
fish_phylo <- fishtree_phylogeny(species = final_species, type = "phylogram")

# save(fish_phylo, file = "Data sources/fish_phylogenetic_tree.RData")
load("Data sources/fish_phylogenetic_tree.RData")


fish_phylo$tip.label <- gsub("_", " ", fish_phylo$tip.label)
par(mfrow = c(1,1), mar = c(4, 10, 3, 0))
plot(fish_phylo, type = "phylogram", show.node.label = TRUE)
par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

setdiff(colnames(Ypa), fish_phylo$tip.label)

Ypa <- Ypa[,colnames(Ypa) %in% fish_phylo$tip.label]
Yabun <- Yabun[,colnames(Yabun) %in% fish_phylo$tip.label]

TrData <- droplevels(TrData[TrData$Species %in% fish_phylo$tip.label,])
rownames(TrData) <- TrData$Species


# 6 - Define formulas and model parameters --------------------------------
# Define fixed effect formula
# save(XData,
#      Ypa,
#      Yabun,
#      TrData,
#      studyDesign,
#      fish_phylo,
#      rL.rectangle,
#      rL.season,
#      rL.year,
#      file = "HPC model fit/HMSC_bf_fit.RData")
load("HPC model fit/HMSC_bf_fit.RData")

