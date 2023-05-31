#########################################!
#### Explanatory and predictive powers - to be run on the HPC
#### 
#### Marcel Montanyes, DTU Aqua, 02-08-2022
#######################################!

library(Hmsc)
library(snow)

nChains <- 4
samples <- 250
thin <- 100
transient <-  round(0.5*samples*thin)


save_path <- "/zhome/36/a/157232/Desktop/Environment_corrected_HMSCrun/Results/"
filename <- paste0(save_path, "chains_",as.character(nChains), "_samples_",as.character(samples), "_thin_",as.character(thin))
load(filename)

# Compute explanatory power ------------------------------------------
predYfullPa <- computePredictedValues(models$fullPa, expected = FALSE)
EPowfullPa <- evaluateModelFit(hM = models$fullPa, predY = predYfullPa)

EPow <- list(EPowfullPa)
names(EPow) <- names(models)

epowername <- paste0(save_path, "Expl_power", "_model_chains_",as.character(nChains),
                     "_samples_",as.character(samples), "_thin_",as.character(thin))
save(EPow, file = epowername)
# load(epowername)


# Compute predictive power ------------------------------------------
partition <- createPartition(models$fullPa, nfolds = 5)
preds <- computePredictedValues(models$fullPa, partition = partition, nParallel = 4)
MF_cross <- evaluateModelFit(models$fullPa, predY = preds)

save(MF_cross, file = predictivename)
