#########################################!
#### Code for comparing environmental variables from haul observations with NEMO-MEDUSA models
#### 
#### Marcel Montanyes, DTU Aqua, 17-11-2021
#######################################!

library(dplyr)
library(ggplot2)

load("My data/ICES_environment.RData")
load("My data/numcpue_nemo_medusa_fishing.RData")

head(myEnv)
myEnv <- myEnv%>%
  ungroup()%>%
  filter(Survey == "NS-IBTS")%>%
  select(Survey, HaulID, Year, Month, ShootLat, ShootLong, SBT, SST)%>%
  unique()
sum(!is.na(myEnv$SBT))/nrow(myEnv)*100

envdat <- myEnv%>%filter(!is.na(SBT))%>%select(-Survey)

nemodat <- numcpue_nm_f%>%
  select(HaulID, Year, Month, ShootLat, ShootLong, Temperature_floor, Temperature_surf)%>%
  unique()


coupled <- merge(nemodat, envdat, by = c("HaulID", "Year", "Month", "ShootLat", "ShootLong"), all = FALSE)


cor.test(coupled$Temperature_floor, coupled$SBT)
# cor.test(coupled$Temperature_surf, coupled$SST)
qqplot(coupled$Temperature_floor, coupled$SBT)
ggplot(coupled, aes(x = SBT, y = Temperature_floor)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1)

ggplot(coupled, aes(x = SBT, y = Temperature_floor)) + geom_point() +
  geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
  theme_bw()

