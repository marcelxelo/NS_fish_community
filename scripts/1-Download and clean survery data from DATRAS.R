#############################################################!
#### Code to download and clean survey data from DATRAS
#### Coding: Aurore Maureaud, March 2021
####
#### Modified: Marcel Montanyès 03-08-2021
#############################################################!

# Downloaded from:
# https://github.com/AquaAuma/CleanTrawlNAmEUr

# Variables meaning:
# https://datras.ices.dk/Data_products/ReportingFormat.aspx


##########################################################################################!
#### LOAD LIBRARIES ----
##########################################################################################!
library(data.table)
library(dplyr)
library(icesDatras)
library(worms)
library(worrms)
library(crul)
library(urltools)


##########################################################################################!
#### LOAD FILES ----
##########################################################################################!
last.year <- 2019


# Haul info from Datras
hh.ns <- getDATRAS(record='HH', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hh.baltic <- getDATRAS(record='HH', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hh.evhoe <- getDATRAS(record='HH', survey='EVHOE', years=c(1997:last.year), quarters=4)
hh.cgfs <- getDATRAS(record='HH', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hh.igfs <- getDATRAS(record='HH', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hh.nigfs <- getDATRAS(record='HH', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hh.pt <- getDATRAS(record='HH', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hh.rock <- getDATRAS(record='HH', survey='ROCKALL', years=c(1999:2009), quarters=3)
hh.scorock <- getDATRAS(record='HH', survey='SCOROC', years=c(2011:last.year), quarters=3)
hh.swc <- getDATRAS(record='HH', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hh.scowcgfs <- getDATRAS(record='HH', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))

hh <- rbind(hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock,
            hh.swc, hh.scowcgfs)

# Length info from DATRAS
hl.ns <- getDATRAS(record='HL', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hl.baltic <- getDATRAS(record='HL', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hl.evhoe <- getDATRAS(record='HL', survey='EVHOE', years=c(1997:last.year), quarters=4)
hl.cgfs <- getDATRAS(record='HL', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hl.igfs <- getDATRAS(record='HL', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hl.nigfs <- getDATRAS(record='HL', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hl.pt <- getDATRAS(record='HL', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hl.rock <- getDATRAS(record='HL', survey='ROCKALL', years=c(1999:2009), quarters=3)
hl.scorock <- getDATRAS(record='HL', survey='SCOROC', years=c(2011:last.year), quarters=3)
hl.swc <- getDATRAS(record='HL', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hl.scowcgfs <- getDATRAS(record='HL', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))

hl <- rbind(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock,
            hl.swc, hl.scowcgfs)

rm(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.swc, hl.scowcgfs, 
   hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.swc, hh.scowcgfs)


##########################################################################################!
#### CREATE A UNIQUE HAUL ID ----
##########################################################################################!
hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo)
hl$SweepLngt <- hl$SpecCodeType <- hl$SpecCode <- hl$Sex <- hl$DateofCalculation <- hl$RecordType <- NULL
hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo)

# Is the HaulID unique?
hhn <- unique(hh$HaulID)
length(hhn)==nrow(hh)

# pb <- c()
# for (i in 1:length(hhn)){
#  j <- which(hh$HaulID==hhn[i])
#  if(length(j)>1){pb <- hhn[i]}
# }

hh$DateofCalculation <- hh$ThClineDepth <- hh$ThermoCline <- hh$SwellHeight <- hh$SwellDir <- hh$WindSpeed <- hh$WindDir <- hh$BotCurSpeed <- NULL
hh$BotCurDir <- hh$SurCurSpeed <- hh$SurCurDir <- hh$SpeedWater <- hh$TowDir <- hh$WgtGroundRope <- hh$KiteDim <- hh$Buoyancy <- NULL
hh$DoorWgt <- hh$DoorSurface <- hh$WarpDen <- hh$Warpdia <- hh$Warplngt <- hh$Tickler <- hh$Rigging <- hh$Netopening <- NULL
hh$HydroStNo <- hh$HaulLat <- hh$SweepLngt <- hh$HaulLong <- hh$DayNight <- hh$Stratum <- hh$TimeShot <- hh$Day <- hh$RecordType <- hh$GearExp <- hh$DoorType <- NULL

hh <- hh %>% filter(HaulID!="NS-IBTS 1995 1 NA AA36 GOV 999 999") # remove the non-unique HaulID in hh and hl
hl <- hl %>% filter(HaulID!="NS-IBTS 1995 1 NA AA36 GOV 999 999")

# Only keep hauls where there is the length composition. 60162 hauls in hh and 60135 in hl
hh <- subset(hh, hh$HaulID %in% hl$HaulID)
hl <- subset(hl, hl$HaulID %in% hh$HaulID)

# Is the HaulID unique?
hhn <- unique(hh$HaulID)
length(hhn)==nrow(hh)

if(identical(sort(unique(hh$HaulID)),sort(unique(hl$HaulID)))){
#  save(hh, file='Data sources/DATRAS/HH.28.02.2021.RData')
# save(hl, file='Data sources/DATRAS/HL.28.02.2021.RData')
}


load('Data sources/DATRAS/HH.28.02.2021.RData')
load('Data sources/DATRAS/HL.28.02.2021.RData')

unique(hl$Gear)
unique(hh$Gear)
##########################################################################################!
#### MERGE HH and HL FILES ----
##########################################################################################!

haulidhl <- sort(unique(hl$HaulID))
haulidhh <- sort(unique(hh$HaulID))
identical(haulidhh, haulidhl)

# survey <- merge(hh, hl, by='HaulID', all.x=FALSE, all.y=TRUE)
survey <- right_join(hh, hl, by=c('HaulID','Survey','Quarter','Country','Ship','Gear','StNo','HaulNo','Year'))
length(unique(survey$HaulID[which(survey$Survey == "NS-IBTS" & is.na(survey$Depth))],))
# View(subset(survey, HaulID == "NS-IBTS 2019 1 NO 58G2 GOV 60006 6" & Valid_Aphia == 126436))
nrow(survey)==nrow(hl)

survey <- survey %>% 
  dplyr::rename(SBT = BotTemp,
                SST = SurTemp,
                Speed = GroundSpeed,
                AphiaID = Valid_Aphia)
head(survey)
### Check if the HaulID is unique
### Not the case for the baltic sea, a lot of duplicates!!!
# ids <- unique(hh$HaulID)
# pb <- vector()
# for(i in 1:length(ids)){
#   x <- which(hh$HaulID==ids[i])
#   if(length(x)>1){pb[length(pb)+1] <- ids[i]}
# }
# print(pb) # dim 0 ok!


##########################################################################################!
#### REMOVE INVALID DATA ----
##########################################################################################!
unique(hh$HaulVal)
survey <- survey %>% 
  filter(HaulVal %in% 'V', #Remove invalid hauls. "V" stands for valid haul.
         !is.na(AphiaID), # Remove invalid species records
         SpecVal %in% c(1,10,4,7),
         DataType %in% c('S','R','C'))
# SpecVal: Validity of species records. 0	(Invalid information), 1 (Valid information for use in DATRAS data products), 
# 4 (No length measurements only total number), 5 (Observed only, not measured, not counted, but only presence/absence is registered), 
# 6 (No length measurements, only category catch weight), 7 (No length measurements, only total number and category catch weight).

# View(subset(survey[,c("Survey", "DataType", "TotalNo")], Survey == "NS-IBTS"))

##########################################################################################!
#### RESCALE DATA INTO ABUNDANCE FOR THE HAUL DURATION AND ABUNDANCE AT LENGTH ----
##########################################################################################!
# If Data Type=='C', abundance at length already readjusted with time so get back the abundance for the actual duration of the haul.
# If data type=='R', abundance at length is mulitplied by subfactor and adjusted to time
survey <- survey %>% 
  mutate(HLNoAtLngt = case_when(DataType == 'C' ~ HLNoAtLngt*SubFactor*HaulDur/60,
                                DataType %in% c('S','R') ~ HLNoAtLngt*SubFactor),
         TotalNo = case_when(DataType =='C' ~ ceiling(TotalNo*HaulDur/60), # The TotalNo were already adjusted to the haul duration. 
                             # Now get them back to actual TotalNo. ceiling() to avoid decimals -so if its >0 there is a presence-
                             # which don't make sense here. Divides /60 to convert from min to hours (as 10fish/30min*60min/1h = 5 fish/h)
                             DataType %in% c('S','R') ~ TotalNo),
         CatCatchWgt = as.numeric(CatCatchWgt),
         CatCatchWgt = case_when(DataType =='C' ~ CatCatchWgt*HaulDur/60,
                                 DataType %in% c('S','R') ~ CatCatchWgt)) %>%
  select(-HaulVal, -DataType, -StdSpecRecCode, -SpecVal, -SubWgt, -SubFactor) %>% 
  mutate(Survey = if_else(Survey=='SCOWCGFS', 'SWC-IBTS', Survey)) %>% 
  mutate(Survey = if_else(Survey=='SCOROC','ROCKALL', Survey)) %>% 
  filter(!(Survey=="NS-IBTS" & BySpecRecCode %in% c(0,2,3,4,5)), # remove hauls where not all species are recorded
         !(Survey=="BITS" & BySpecRecCode==0))
# BySpecRecCode: The code provides general information on whether all bycatch species are reported. 0 (No by-catch species recorded), 
# 1 (Open ended by-catch list - all species, even species complexes), 2 (Closed by-catch list - All species recorded), 
# 3 (Closed by-catch list - Gadoid species recorded), 4 (Closed by-catch list - Flatfish species recorded), 5 (Closed by-catch list - Various species recorded).

# View(subset(survey[,c("Survey","TotalNo")], Survey == "NS-IBTS"))
# View(subset(survey, Survey == "NS-IBTS"))
# unique(survey$HaulDur[which(survey$Survey == "NS-IBTS")])
# View(subset(survey[,c("HaulID", "AphiaID", "HaulDur", "TotalNo")], HaulID == "NS-IBTS 2014 1 FR 35HT GOV S0023 12" & AphiaID == 126446))

# library(ggplot2)
# survey[which(survey$Survey == "NS-IBTS"),] %>%
#   ggplot( aes(x=HaulDur)) +
#   geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

survey$TotalNo <- as.integer(survey$TotalNo)
# 21321.1%%1==0
table(survey$TotalNo%%1==0) # check if decimals (number of FALSE). No decimals

# Delete those observations with TotalNo <=0 (as only species present are recorded, there cannot be 0)
sum(nrow(survey[which(survey$TotalNo < 0),]))
sum(nrow(survey[which(survey$TotalNo <= 0),]))
survey <- subset(survey, TotalNo >= 0)


##########################################################################################!
#### GET THE SWEPT AREA ----
##########################################################################################!
survey <- survey %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread == -9, NA),
         DoorSpread = replace(DoorSpread, DoorSpread == -9, NA),
         Speed = replace(Speed, Speed == -9, NA),
         Distance = replace(Distance, Distance == -9, NA),
         Depth = replace(Depth, Depth == -9, NA),
         Area.swept = Distance*0.001*DoorSpread*0.001,
         Area.swept = if_else(is.na(Area.swept), Speed*1.852*HaulDur/60*DoorSpread*0.001, Area.swept))

# Re-estimate the swept area from a linear model per survey
### EVHOE ###
evhoe <- survey %>%
  filter(Survey=='EVHOE',
         !is.na(Depth)) %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ Depth, data=evhoe)
# plot(Area.swept ~ HaulDur, data=evhoe)
# plot(Area.swept ~ Speed, data=evhoe)
# plot(Area.swept ~ Distance, data=evhoe)

evhoe$Depth <- log(evhoe$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=evhoe)
summary(lm0)

pred0 <- predict.lm (object=lm0, newdata=evhoe, interval='confidence', level=0.95)
evhoe <- cbind(evhoe, pred0)
evhoe[is.na(evhoe$Area.swept),]$Area.swept <- evhoe[is.na(evhoe$Area.swept),]$fit

evhoe <- evhoe %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2 = Area.swept) # a few NA's - not if !is.na(Depth) is filtered when creating evhoe
area2 <- evhoe


### North Sea ###
nsibts <- survey %>%
  filter(Survey=='NS-IBTS',
         #Year>1989,
         !is.na(Depth)) %>%
  select(Year, HaulID, HaulDur, Area.swept, Depth, Ship, Gear, #GearExp, #This variable is not found ¿?
         DoorType, Speed, Distance) %>%
  distinct()
head(nsibts)
# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nsibts, log='xy')
# plot(Area.swept ~ Depth, data=nsibts)
# plot(Area.swept ~ Distance, data=nsibts)
# plot(Area.swept ~ Speed, data=nsibts)

nsibts$Depth <- log(nsibts$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=nsibts)

pred0 <- predict(lm0, newdata=nsibts, interval='confidence', level=0.95)
nsibts <- cbind(nsibts,pred0)
nsibts[is.na(nsibts$Area.swept),]$Area.swept <- nsibts[is.na(nsibts$Area.swept),]$fit

nsibts <- nsibts %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2 = Area.swept)
area2 <- rbind(nsibts, area2)


### SWC-IBTS ###
swc <- survey %>%
  filter(Survey=='SWC-IBTS',
         !is.na(Depth)) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=swc)
# plot(Area.swept ~ Depth, data=swc, log='x')
swc$Depth <- log(swc$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=swc)

pred0 <- predict(lm0, newdata=swc, interval='confidence', level=0.95)
swc <- cbind(swc,pred0)
swc[is.na(swc$Area.swept),]$Area.swept <- swc[is.na(swc$Area.swept),]$fit

swc <- swc %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2 = Area.swept)
area2 <- rbind(area2, swc)


### BITS ###
bits <- survey %>%
  filter(Survey=='BITS',
         !is.na(Depth)) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=bits)
# plot(Area.swept ~ Depth, data=bits, log='x')
lm0 <- lm(Area.swept ~ HaulDur + log(Depth), data=bits)

pred0 <- predict(lm0, newdata=bits, interval='confidence', level=0.95)
bits <- cbind(bits,pred0)
bits[is.na(bits$Area.swept),]$Area.swept <- bits[is.na(bits$Area.swept),]$fit

bits <- bits %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, bits)



### IE-IGFS ###
ie <- survey %>%
  filter(Survey=='IE-IGFS',
         Year>1989,
         !is.na(Depth)) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

# plot(Area.swept ~ HaulDur, data=ie)
# plot(Area.swept ~ Depth, data=ie, log='x')
ie$Depth <- log(ie$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=ie)

pred0 <- predict(lm0, newdata=ie, interval='confidence', level=0.95)
ie <- cbind(ie,pred0)
ie[is.na(ie$Area.swept),]$Area.swept <- ie[is.na(ie$Area.swept),]$fit

ie <- ie %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, ie)



# FR-CGFS very few hauls with swept area data
cgfs <- survey %>%
  filter(Survey=='FR-CGFS',
         !is.na(Depth)) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=cgfs)
# plot(Area.swept ~ Depth, data=cgfs)
# plot(Area.swept ~ Distance, data=cgfs) # Distance always reported

lm0 <- lm(Area.swept ~ HaulDur + Depth + Distance, data=cgfs)

pred0 <- predict(lm0, newdata=cgfs, interval='confidence', level=0.95)
cgfs <- cbind(cgfs,pred0)
cgfs[is.na(cgfs$Area.swept),]$Area.swept <- cgfs[is.na(cgfs$Area.swept),]$fit

cgfs <- cgfs %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, cgfs)


### NIGFS ###
nigfs <- survey %>%
  filter(Survey=='NIGFS',
         !is.na(Depth)) %>%
  mutate(DurQ = ifelse(HaulDur<40,'S','L')) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, DurQ, Speed, Distance) %>%
  distinct()

# par(mfrow=c(1,2))
# plot(Area.swept ~ HaulDur, data=nigfs)
# plot(Area.swept ~ Depth, data=nigfs)

# Model for short hauls
nigfsS <- nigfs %>%
  filter(DurQ == 'S')

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nigfsS)
# plot(Area.swept ~ Depth, data=nigfsS)
# plot(Area.swept ~ Speed, data=nigfsS)
nigfsS$Depth2 <- (nigfsS$Depth-mean(nigfsS$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsS)

pred0 <- predict(lm0, newdata=nigfsS, interval='confidence', level=0.95)
nigfsS <- cbind(nigfsS,pred0)
nigfsS[is.na(nigfsS$Area.swept),]$Area.swept <- nigfsS[is.na(nigfsS$Area.swept),]$fit


# Model for Long hauls
nigfsL <- nigfs %>%
  filter(DurQ == 'L',
         !is.na(Depth))
# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=nigfsL)
# plot(Area.swept ~ Depth, data=nigfsL)
# plot(Area.swept ~ Speed, data=nigfsL)
nigfsL$Depth2 <- (nigfsL$Depth-mean(nigfsL$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsL)

pred0 <- predict(lm0, newdata=nigfsL, interval='confidence', level=0.95)
nigfsL <- cbind(nigfsL,pred0)
nigfsL[is.na(nigfsL$Area.swept),]$Area.swept <- nigfsL[is.na(nigfsL$Area.swept),]$fit

nigfs <- rbind(nigfsL, nigfsS)
nigfs <- nigfs %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2 = Area.swept)
area2 <- rbind(area2, nigfs)



### ROCKALL ###
rock <- survey %>%
  filter(Survey=='ROCKALL',
         !is.na(Depth)) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=rock)
# plot(Area.swept ~ Depth, data=rock)
# plot(Area.swept ~ Speed, data=rock)
# plot(Area.swept ~ Distance, data=rock, log='x')

lm0 <- lm(Area.swept ~ HaulDur + Depth, data=rock)

pred0 <- predict(lm0, newdata=rock, interval='confidence', level=0.95)
rock <- cbind(rock,pred0)
rock[is.na(rock$Area.swept),]$Area.swept <- rock[is.na(rock$Area.swept),]$fit

rock <- rock %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, rock)



### PORTUGAL ###
pt <- survey %>%
  filter(!is.na(HaulDur),
         !is.na(Depth),
         !is.na(Speed))%>%
  select(Survey, Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

# par(mfrow=c(2,2))
# plot(Area.swept ~ HaulDur, data=pt)
# plot(Area.swept ~ Depth, data=pt)
# plot(Area.swept ~ Speed, data=pt)
# plot(Area.swept ~ Distance, data=pt)
lm0 <- lm(Area.swept ~ HaulDur + Depth + Speed, data=pt)

pred0 <- predict(lm0, newdata=pt, interval='confidence', level=0.95)
pt <- cbind(pt,pred0)
pt[is.na(pt$Area.swept),]$Area.swept <- pt[is.na(pt$Area.swept),]$fit

pt <- pt %>%
  filter(Survey=='PT-IBTS') %>%
  select(HaulID, Area.swept) %>%
  dplyr::rename(Area2=Area.swept)
area2 <- rbind(area2, pt)

rm(bits, cgfs, ie, nsibts, pt, nigfsL, nigfsS, nigfs, pred0, lm0, evhoe, swc, rock)


# Paste new estimates to survey data frame
area2 <- area2 %>% distinct()
survey0 <- left_join(survey, area2, by='HaulID')
sum(is.na(survey0$Area.swept))
sum(is.na(survey0$Area2))
survey0 <- survey0 %>%
  mutate(Area.swept = coalesce(Area.swept, Area2)) %>%
  select(-Area2) %>%
  filter(is.na(Area.swept) | Area.swept>0)

mysurvey <- survey0
table(mysurvey$TotalNo%%1==0) # check if decimals (number of FALSE)

myEnv <- mysurvey%>%
  select(Survey, HaulID, Year, Month, Quarter, StatRec, Depth, SBT, SST, ShootLat, ShootLong, AphiaID, Gear, Area.swept, HaulDur, TotalNo)%>%
  group_by(Survey, HaulID, Year, Month, Quarter, StatRec,  Depth, SBT, SST, ShootLat, ShootLong, AphiaID, Gear, Area.swept, HaulDur)%>%
  dplyr::summarise(TotalNo = sum(TotalNo))%>%
  mutate(numcpue = TotalNo/Area.swept, # abundance/km2
         numh = (TotalNo*60)/HaulDur,  # abundance/hour
         Depth = replace(Depth, Depth<0, NA),
         SBT = replace(SBT, SBT<0, NA),
         SST = replace(SST, SST<0, NA))

# save(myEnv, file = "My data/ICES_environment.RData")

##########################################################################################!
#### GET CPUEs AND RIGHT COLUMNS NAMES ----
##########################################################################################!

# Remove data without length composition or negative values
# In my case I don't need length composition
# xx <- subset(mysurvey, HLNoAtLngt<0 | is.na(LngtClass)) 
# HLNoAtLngt: Number of fish at this category in this haul. For CPUE data should be adjusted with time. 
# TotNo=Sum(HLNoAtLngt)*SubFactor for this species.
# 
# LngtClass: Length classes registered for this catch category as specified at the LngthCode. Species-sensitive. 
# Indicates the lower bound of length distribution at this category. F.ex. 65-70cm=65.
# no_length_hauls <- sort(unique(xx$HaulID))


# If I understand correctly, LngthCode reffers to a specific length class and HLNoAtLngt to the number of individuals catchet for that given length class.
# It that's correct, and taking into account that I'm not interested in length classes, I don't need to filter out those observations without this data.
head(mysurvey)

View(subset(mysurvey, AphiaID == 126436 & Year == 2013))

# # Only keep abundances/weight
# survey2 <- mysurvey %>%
#   #filter(!(HaulID %in% no_length_hauls)) %>% # remove hauls without length data # THIS FILTERING STEP IS THE ONE THAT REMOVES LOTS OF HAULS!
#   mutate(numcpue = TotalNo/Area.swept, # abundance/km2
#          wtcpue = CatCatchWgt/(Area.swept*1000), #weight in kg/km2
#          numh = (TotalNo*60)/HaulDur, # abundance/hour
#          wgth = CatCatchWgt*60/(HaulDur*1000), #weight in kg/h
#          num = TotalNo, #raw number of individuals
#          wgt = CatCatchWgt, # raw weight         
#          # numlencpue = HLNoAtLngt/Area.swept, #abundance/km2 per length class # only run if interested in length class 
#          # numlenh = HLNoAtLngt*60/HaulDur, #abundance/h per length class # only run if interested in length class 
#          Season = 'NA',
#          Depth = replace(Depth, Depth<0, NA),
#          SBT = replace(SBT, SBT<0, NA),
#          SST = replace(SST, SST<0, NA),
#          LngtClass = ifelse(LngtCode %in% c('.','0'), LngtClass*0.1, LngtClass)) %>% # fix unit of length class
#   dplyr::rename(Length = LngtClass) %>% 
#   # group_by(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, AphiaID, Length) %>%
#   # summarize_at(.vars=c('numcpue', 'wtcpue', 'numh', 'wgth', 'num', 'wgt', 'numlencpue','numlenh'), .funs=function(x) sum(x, na.rm=T)) %>%
#   select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
#          AphiaID, CatIdentifier, numcpue, wtcpue, numh, wgth, num, wgt#, Length, numlencpue, numlenh
#          )


mysurvey <- mysurvey%>%
  select(Survey, HaulID, Year, Month, Quarter, StatRec, ShootLat, ShootLong, AphiaID, Gear, Area.swept, HaulDur, TotalNo)%>%
  group_by(Survey, HaulID, Year, Month, Quarter, StatRec, ShootLat, ShootLong, AphiaID, Gear, Area.swept, HaulDur)%>%
  dplyr::summarise(TotalNo = sum(TotalNo))%>%
  mutate(numcpue = TotalNo/Area.swept, # abundance/km2
         numh = (TotalNo*60)/HaulDur) # abundance/hour
table(mysurvey$TotalNo%%1==0) # check if decimals (number of FALSE)

table(duplicated(mysurvey$HaulID[which(mysurvey$AphiaID == 126436)]))

head(mysurvey)
mysurvey <- data.frame(mysurvey)
table(is.na(mysurvey$numh)) # == TotalNo. As there aren't any missing total fish data, I guess the adjustment I made is correct and I can use either numcpue or numh for my analysis.
table(is.na(mysurvey$TotalNo))
table(is.na(mysurvey$numcpue))
table(is.na(mysurvey$Area.swept))
table(is.na(mysurvey$Gear))

unique(mysurvey$Quarter)
unique(mysurvey$Survey)

##########################################################################################!
#### Clean species names ----
##########################################################################################!
mysurvey$Species <- NA
dat.ices <- mysurvey
aphia_list <- unique(dat.ices$AphiaID)
aphia_list <- aphia_list[!duplicated(aphia_list)]

# creating taxonomy tables for each species
my_sp_taxo <- wm_record_(id = aphia_list)

# row binds all the results and pass to data frame. 
df_test <- data.frame(do.call(rbind, my_sp_taxo))
df_test$url <- df_test$lsid <- df_test$citation <- NULL
df_test$isExtinct <- df_test$modified <- df_test$valid_authority <- df_test$unacceptreason <- NULL
df_test$authority <- df_test$status <- df_test$taxonRankID <- df_test$isBrackish <- df_test$isFreshwater <- df_test$isTerrestrial <- df_test$match_type <- NULL
#check if it identified everything
dim(subset(df_test, is.na(df_test$phylum))) # ok


# In the class column, we only keep the 5 groups we want. 
df_test <- subset(df_test, class %in% c("Elasmobranchii",
                                        # "Actinopterygii"
                                        "Actinopteri",
                                        "Holocephali",
                                        "Myxini",
                                        "Petromyzonti")) 

keep_sp <- data.frame(df_test) # subsetting
keep_sp <- data.frame(unlist(keep_sp$valid_name)) #unlisting
names(keep_sp) <- 'ScientificName'
keep_ap <- data.frame(df_test) # subsetting
keep_ap <- data.frame(unlist(keep_ap$AphiaID))
names(keep_ap) <- 'AphiaID'
keep_gen <- data.frame(df_test) # subsetting
keep_gen <- data.frame(unlist(keep_gen$genus))
names(keep_gen) <- 'Genus'
keep_fa <- data.frame(df_test) # subsetting
keep_fa <- data.frame(unlist(keep_fa$family))
names(keep_fa) <- 'Family'
keep <- cbind(keep_ap, keep_sp, keep_gen, keep_fa)

dat.ices <- subset(dat.ices, dat.ices$AphiaID %in% keep_ap$AphiaID)
dat.ices <- left_join(dat.ices, keep, by='AphiaID')
dat.ices$Species <- dat.ices$ScientificName
dat.ices$ScientificName <- NULL
myices <- dat.ices

myices$AphiaID <- NULL
length(unique(myices$Species))

### Code to integrate from Anna Rindorf on species bycatch corrections

myices <- data.frame(myices) %>%
  mutate(Species = recode(Species,'Dipturus batis'='Dipturus','Dipturus flossada'='Dipturus',
                          'Dipturus batis-complex'='Dipturus','Dipturus intermedia'='Dipturus',
                          'Dipturus'='Dipturus','Liparis montagui'='Liparis',
                          'Liparis liparis'='Liparis','Liparis liparis liparis'='Liparis',
                          'Chelon aurata'='Chelon','Chelon ramada'='Chelon',
                          'Mustelus mustelus/asterias'='Mustelus','Mustelus'='Mustelus',
                          'Mustelus mustelus'='Mustelus','Mustelus asterias'='Mustelus',
                          'Alosa'='Alosa','Alosa alosa'='Alosa','Alosa fallax'='Alosa',
                          'Argentina'='Argentina','Argentinidae'='Argentina',
                          'Argentina silus'='Argentina','Argentina sphyraena'='Argentina',
                          'Callionymus reticulatus'='Callionymus','Callionymus maculatus'='Callionymus',
                          'Ciliata mustela'='Ciliata','Ciliata septentrionalis'='Ciliata',
                          'Gaidropsarus'='Gaidropsarus','Gaidropsaurus macrophthalmus'='Gaidropsarus',
                          'Gaidropsaurus mediterraneus'='Gaidropsarus','Gaidropsaurus vulgaris'='Gaidropsarus',
                          'Sebastes'='Sebastes','Sebastes norvegicus'='Sebastes','Sebastes mentella'='Sebastes',
                          'Sebastes marinus'='Sebastes','Syngnathus'='Syngnatus',
                          'Syngnathus rostellatus'='Syngnatus','Syngnathus acus'='Syngnatus',
                          'Syngnathus typhle'='Syngnatus','Nerophis ophidion'='Syngnatus',
                          'Pomatoschistus'='Pomatoschistus','Pomatoschistus microps'='Pomatoschistus',
                          'Pomatoschistus minutus'='Pomatoschistus','Pomatoschistus pictus'='Pomatoschistus',
                          'Lesueurigobius'='Gobius','Gobius cobitis'='Gobius','Gobius niger'='Gobius',
                          'Leusueurigobius friesii'='Gobius','Neogobius melanostomus'='Gobius',
                          'Neogobius'='Gobius'))


table(duplicated(myices[,c("HaulID", "Species")]))

View(myices[which(duplicated(myices[,c("HaulID", "Species")])),]) 
# They are all taxa that have been identified until genus (and not species). We can therefore aggregate them
names(myices)
myices <- myices%>%
  group_by(Survey, HaulID, Year, Month, Quarter, StatRec, ShootLat, ShootLong, Gear, Area.swept, HaulDur, # Family, Genus, 
           Species)%>%
  dplyr::summarise(TotalNo = sum(TotalNo),
                   numcpue = sum(numcpue),
                   numh = sum(numh))
table(myices$TotalNo%%1==0) # check if decimals (number of FALSE)

# View(subset(myices, HaulID == "BITS 2011 4 DE 06SL TVS 22056 54" & Species == "Gobius"))
table(duplicated(myices[,c("HaulID", "Species")]))

nrow(unique(myices)) == nrow(myices)

ices.survey <- unique(myices)



##########################################################################################!
#### SAVE DATA ----
##########################################################################################!

# save(ices.survey, file="My data/ICES_survey_clean.RData")
load("My data/ICES_survey_clean.RData") # ices.survey
