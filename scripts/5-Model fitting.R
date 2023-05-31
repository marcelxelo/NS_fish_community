#########################################!
#### HMSC Presence-absence model fitting - to be run on the HPC
#### 
#### Marcel Montanyes, DTU Aqua, 08-03-2022
#######################################!

library(Hmsc)
library(snow)

origin_path <- "/zhome/36/a/157232/Desktop/Environment_corrected_HMSCrun/"




# Define fitting parameters
nChains <- 4
samples <- 250
thin <- 100
transient <-  round(0.5*samples*thin)



# 1 - Load data (X, Y, T, C) ----------------------------------------------
load(paste0(origin_path, "HMSC_bf_fit.RData"))
head(XData)
head(TrData)
head(Ypa[,c(1:10)])
head(Yabun[,c(1:10)])
table(rownames(TrData) == c(colnames(Ypa), colnames(Yabun)))
head(studyDesign)
# par(mar = c(0,0,0,0))
# plot(fish_phylo)
# par(mar=c(5.1, 4.1, 4.1, 2.1))


# 2 - Define formulas and model parameters --------------------------------
# Define fixed effect formula
names(XData)
XFormula <-  ~  poly(SBT,degree = 2,raw = TRUE) + SBT_seasonality +
  SBS_seasonality +
  Detritus_floor + Phy_surf + DIN_surf + 
  Sediment + Fishing_lag


# Define trait formula
names(TrData)
TrFormula <- ~ habitat + body.shape + fin.shape + spawning.type + feeding.mode +
  log_age.maturity + log_length.max + log_offspring.size + log_fecundity + log_growth + trophic_level


# 3 - Setting up the HMSC models ------------------------------------------
fullPa <- Hmsc(Y = Ypa, XData = XData, XFormula = XFormula, TrData = TrData, TrFormula = TrFormula, phyloTree = fish_phylo,
               studyDesign = studyDesign, ranLevels = list(StatRec = rL.rectangle, Season = rL.season, Year = rL.year), 
               distr = "probit")


# 4 - Fitting the models --------------------------------------------------
nParallel <- nChains

Sys.time();tfullpa0 <- Sys.time()
fullPa <-  sampleMcmc(fullPa, initPar = "fixed effects", nParallel = nParallel,
                      thin = thin, samples = samples, transient = transient, nChains = nChains)
tfullpa1 <- Sys.time(); tfullpa1-tfullpa0
fullPa$RunTime <- tfullpa1-tfullpa0 # Keep track of running time

models <-  list(fullPa = fullPa)

save_path <- "/zhome/36/a/157232/Desktop/Environment_corrected_HMSCrun/Results/"
filename <- paste0(save_path, "chains_",as.character(nChains), "_samples_",as.character(samples), "_thin_",as.character(thin))
save(models, file = filename)
# load(filename)