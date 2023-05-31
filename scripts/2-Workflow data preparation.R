#########################################!
#### Workflow preparing environmental data (NEMO-MEDUSA), fishing data and fish survey data (ICES)
#### 
#### Marcel Montanyès, DTU Aqua, 17-11-2021
#######################################!



library(ggplot2)
library(dplyr)
library(mapplots)
library(sf)
library(RANN)
library(worrms)



# 1 - Quick exploration of environmental data -----------------------------
test <- readRDS(file = "Data sources/Environment RCP 8.5/Surface_North/NM.01.1980.rds")
names(test)

ggplot() + 
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(test$longitude),ylim=range(test$latitude)) +
  # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
  scale_x_continuous()+
  scale_y_continuous()+
  geom_point(data=test, aes(x=longitude, y=latitude, color=Salinity))

mean(diff(sort(test$longitude)))
sd(diff(sort(test$longitude)))
mean(diff(sort(test$latitude)))
sd(diff(sort(test$latitude)))

rm(test)




# 2 - Define region to extract environmental data for ---------------------
load("My data/ICES_survey_clean.RData") # ices.survey
unique(ices.survey$Survey)
# North Sea
ns <- subset(ices.survey, Survey == "NS-IBTS")

# Set area limit for extraction
xlim <- c(min(ns$ShootLong) - 5, max(ns$ShootLong) + 5)
ylim <- c(min(ns$ShootLat) - 5, max(ns$ShootLat) + 5)


# 3 - Extract North Surface data ------------------------------------------
      # a) Rename Surface North files (Year - Month) ----------------------------
      # Rename files changing order of month year - makes it easier to select data form specific years
      # Need to peviously copy the folder into a "... - renamed" folder
      # origin <- "Data sources/Environment RCP 8.5/Surface_North - renamed/"
      # files <-  list.files(origin, pattern='.rds', full.names=TRUE)
      # for(i in seq_along(files)){
      #   my_file <- files[i]
      # 
      #   month <- substring(my_file, nchar(my_file)-11, nchar(my_file)-9)
      #   year <- substring(my_file, nchar(my_file)-8, nchar(my_file)-4)
      # 
      #   paste0("NM", year, month, ".rds")
      #   file.rename(files[i], paste0(origin,"NM", year, month, ".rds"))
      # }
      
      
      
    
      # b) Extract surface data for North Sea -----------------------------------
      origin <- "Data sources/Environment RCP 8.5/Surface_North - renamed/"
      files <-  list.files(origin, pattern='.rds', full.names=TRUE)
      
      files[50*12] # 50 years * 12 month (data until 2029)
      files_1980_2029 <- files[1:(50*12)]
      
      t0 <- Sys.time()
      NS_Surface_1980_2029 <-  lapply(files_1980_2029,
                                      function(x) {
                                        # Loop over files
                                        rds <- readRDS(x)
                                        rds <- subset(rds,
                                                      longitude > xlim[1] &
                                                        longitude < xlim[2] &
                                                        latitude > ylim[1] &
                                                        latitude < ylim[2])
                                      }
      )
      t1 <- Sys.time()
      t1-t0
      
      # plotTest <- as.data.frame(NS_Surface_1980_2029[[1]])
      # ggplot(plotTest, aes(x = longitude, y = latitude, color = Salinity > 0)) + geom_point(size = 1.5) +
      #   borders(fill="gray44",colour="gray61") + coord_quickmap(xlim = range(plotTest$longitude), ylim = range(plotTest$latitude))
      
      NS_Surface_1980_2029 <- bind_rows(NS_Surface_1980_2029)
      
      head(NS_Surface_1980_2029)
      range(NS_Surface_1980_2029$longitude)
      range(NS_Surface_1980_2029$latitude)
      range(NS_Surface_1980_2029$Year)
      sum(duplicated(NS_Surface_1980_2029[1:5000,]))
      # saveRDS(NS_Surface_1980_2029, file = "NS_Surface_1980_2029.rds")
      
      
      
      
# 4 - Extract North Seafloor data -----------------------------------------
      # a) Rename Seafloor North files (Year - Month) ----------------------------
      # origin <- "Data sources/Environment RCP 8.5/Seafloor_North - renamed/"
      # files <-  list.files(origin, pattern='.rds', full.names=TRUE)
      # for(i in seq_along(files)){
      #   my_file <- files[i]
      #   
      #   month <- substring(my_file, nchar(my_file)-11, nchar(my_file)-9)
      #   year <- substring(my_file, nchar(my_file)-8, nchar(my_file)-4)
      #   
      #   paste0("NM", year, month, ".rds")
      #   file.rename(files[i], paste0(origin,"NM", year, month, ".rds"))
      # }
    
      
      # b) Extract seafloor data for North Sea ----------------------------------
      origin <- "Data sources/Environment RCP 8.5/Seafloor_North - renamed/"
      files <-  list.files(origin, pattern='.rds', full.names=TRUE)
    
      files[50*12] # 50 years * 12 month (data until 2029)
      files_1980_2029 <- files[1:(50*12)]
      
      t0 <- Sys.time()
      NS_Seafloor_1980_2029 <-  lapply(files_1980_2029,
                                       function(x) {
                                         # Loop over files
                                         rds <- readRDS(x)
                                         rds <- subset(rds,
                                                       longitude > xlim[1] &
                                                         longitude < xlim[2] &
                                                         latitude > ylim[1] &
                                                         latitude < ylim[2])
                                       }
      )
      t1 <- Sys.time()
      t1-t0
      
      # plotTest <- as.data.frame(NS_Seafloor_1980_2029[[1]])
      # ggplot(plotTest, aes(x = longitude, y = latitude, color = Salinity > 0)) + geom_point(size = 1.5) +
      #   borders(fill="gray44",colour="gray61") + coord_quickmap(xlim = range(plotTest$longitude), ylim = range(plotTest$latitude))
      
      # Seems that for some point it hasen't been possible to model salinity (for any of the time points)
      
      NS_Seafloor_1980_2029 <- bind_rows(NS_Seafloor_1980_2029)
      
      head(NS_Seafloor_1980_2029)
      range(NS_Seafloor_1980_2029$longitude)
      range(NS_Seafloor_1980_2029$latitude)
      range(NS_Seafloor_1980_2029$Year)
      sum(duplicated(NS_Seafloor_1980_2029[1:5000,]))
      # saveRDS(NS_Seafloor_1980_2029, file = "Data sources/Environment RCP 8.5/NS_Seafloor_1980_2029.rds")
    
    
    
# 5 - Merge Surface and Seafloor data for the North Sea -------------------
# Rename variables
NS_Surface_1980_2029 <- readRDS(file = "Data sources/Environment RCP 8.5/NS_Surface_1980_2029.rds")
names(NS_Surface_1980_2029)
names(NS_Surface_1980_2029) <- c("Salinity_surf", "Temperature_surf", "Ice_conc_suf",
                                 "Zonal_surf", "Meridional_surf",
                                 "DIN_surf", "Detritus_surf", "Phytoplankton_surf",
                                 "x_surf", "y_surf", "longitude", "latitude", "Year", "Month")

NS_Seafloor_1980_2029 <- readRDS(file = "Data sources/Environment RCP 8.5/NS_Seafloor_1980_2029.rds")
names(NS_Seafloor_1980_2029)
names(NS_Seafloor_1980_2029) <- c("Salinity_floor", "Temperature_floor",
                                  "Zonal_floor", "Meridional_floor",
                                  "DIN_floor", "Detritus_floor", "Phytoplankton_floor",
                                  "x_floor", "y_floor", "longitude", "latitude", "Bathymetry", "Year", "Month")
# Merge by time and location
NS_1980_2029_surf_floor <- merge(NS_Surface_1980_2029, NS_Seafloor_1980_2029, by = c("Year", "Month", "longitude", "latitude"),
                                 all.x = FALSE, # Surface has data (0s) from land areas
                                 all.y = TRUE) # Seafloor has only data for marine areas

# Calculate seasonality for temperature and salinity 
Seasonality <- NS_1980_2029_surf_floor%>%
  group_by(Year, longitude, latitude)%>%
  summarise(Tseasonlaity_surf= sd(Temperature_surf),
            Tseasonlaity_floor= sd(Temperature_floor),
            Sseasonlaity_surf= sd(Salinity_surf),
            Sseasonlaity_floor= sd(Salinity_floor))


NS_1980_2029_surf_floor <- merge(NS_1980_2029_surf_floor, Seasonality, by = c("Year", "longitude", "latitude"), all = TRUE)


# ggplot() + 
#   borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(NS_1980_2029_surf_floor$longitude),ylim=range(NS_1980_2029_surf_floor$latitude)) +
#   # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
#   scale_x_continuous()+
#   scale_y_continuous()+
#   geom_point(data=subset(NS_1980_2029_surf_floor, Year == 2000 & Month == 6), aes(x=longitude, y=latitude, color=Temperature_surf), alpha = 0.3)

ggplot() + 
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(NS_1980_2029_surf_floor$longitude),ylim=range(NS_1980_2029_surf_floor$latitude)) +
  # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
  scale_x_continuous()+
  scale_y_continuous()+
  geom_point(data=subset(NS_1980_2029_surf_floor, Year == 2029 & Month == 6), aes(x=longitude, y=latitude), color = "red", shape = 0) #+
  geom_point(data=subset(NS_1980_2029_surf_floor, Year == 2029 & Month == 6), aes(x=longitude, y=latitude), color = "green", alpha = 0.5)

  ggplot() + 
    borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(NS_1980_2029_surf_floor$longitude),ylim=range(NS_1980_2029_surf_floor$latitude)) +
    # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
    scale_x_continuous()+
    scale_y_continuous()+
    geom_point(data=subset(NS_1980_2029_surf_floor, Year == 2029 & Month == 6), aes(x=longitude, y=latitude, color = Meridional_surf))
  


# 6 - Extract sediment type data ------------------------------------------
library(raster)
sediment_raster <- "Data sources/EMODnet_Seabed_substrate_1M/EMODnet_Seabed_Substrate_1M_September2021/Sediment_raster.tif"
sediment_raster <- raster(sediment_raster)
plot(sediment_raster)
sediment_raster@crs

# NS_1980_2029 <- readRDS("Data sources/Environment RCP 8.5/NS_1980_2029.rds")
coords <- unique(NS_1980_2029_surf_floor[,c("longitude", "latitude")])
sum(is.na(coords))

sppoints <- SpatialPoints(coords, proj4string=CRS('+proj=longlat +datum=WGS84')) # turn coordinates into spatial points with same projection as raster
plot(sediment_raster)
plot(sppoints, add = T)

ext <- raster::extract(sediment_raster, sppoints, df=TRUE) # Extract sediment types data; make sure to use extract() from raster package (and not tidyr)
head(ext)

ext[c("longitude", "latitude")] <- coords[,c("longitude", "latitude")]

my_nas <- ext[is.na(ext$Sediment_raster),]
ggplot() +
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(my_nas$longitude),ylim=range(my_nas$latitude)) +
  # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
  scale_x_continuous()+
  scale_y_continuous()+
  geom_point(data=my_nas, aes(x=longitude, y=latitude), alpha = 0.3)

ggplot() +
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(4,7),ylim=c(52,54)) +
  # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
  scale_x_continuous()+
  scale_y_continuous()+
  geom_point(data=my_nas, aes(x=longitude, y=latitude), alpha = 0.3)

# The missing data within the North Sea are manly coastal data points. In total I'd say that we'll only have NAs for ~ 30 points.
# Most of the NAs are outside the study area (Norway, Ireland, France)

ext <- rename(ext, Sediment_ID = Sediment_raster)
ext$Sediment_type <- as.factor(ext$Sediment_ID)
levels(ext$Sediment_type)
levels(ext$Sediment_type) <- c("Mud to muddy sand", # 1
                               "Sand", # 2
                               "Coarse substrate", # 3
                               "Mixed sediment", # 4
                               "Rock and boulders", # 5
                               "No data at this level of folk") # 6
ext$ID <- NULL

NS_1980_2029 <- merge(NS_1980_2029_surf_floor, ext, by = c("longitude", "latitude"), all = T)
head(NS_1980_2029)

# Save
# saveRDS(NS_1980_2029, file = "Data sources/Environment RCP 8.5/NS_1980_2029.rds")
NS_1980_2029 <- readRDS("Data sources/Environment RCP 8.5/NS_1980_2029.rds")

ggplot() +
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(-10,20),ylim=c(45,70)) +
  # borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=c(10,15),ylim=c(55,60)) +
  scale_x_continuous()+
  scale_y_continuous()+
  geom_point(data=subset(NS_1980_2029, Month == 1 & Year == 2000), aes(x=longitude, y=latitude, color = Temperature_surf), alpha = 0.7)


# 7 - Arrange fish survey data --------------------------------------------

      # a) Add 0s for species not observed in each haul -------------------------
      load("My data/ICES_survey_clean.RData") # ices.survey
      
      # Filter for North Sea & Quarter 1
      survey.ns <- ices.survey%>%
        filter(Survey == "NS-IBTS")%>%
        ungroup() %>%
        dplyr::select(-Survey)
      
      head(survey.ns$TotalNo) # abundance
      head(survey.ns$numcpue) # abundance/km2
      head(survey.ns$numh) # abundance/hour
      
      table(survey.ns$TotalNo%%1==0) # check if decimals (represented by the number of FALSEs)
      table(survey.ns$numcpue%%1==0)
      table(survey.ns$numh%%1==0)
      
      
      View(subset(survey.ns, HaulID == "NS-IBTS 2008 3 GB 74E9 GOV 22 43" & Species == "Merlangius merlangus")) # TotalNo it's decimal, meaning that the error comes from before
      
      
      # I think is best to do add the 0s three separate times -one for TotalNo, one for numcpue and one for numh- and then merge
      # TotalNo
      dIn <- survey.ns%>%
        select(HaulID, Year, Month, StatRec, ShootLong, ShootLat, Gear, HaulDur, Area.swept, Species, TotalNo) %>%
        # filter(Species %in% c("Clupea harengus", "Sprattus sprattus", "Gadus morhua")) %>%
        arrange(Species)
      
      # Add missing values as 0 (this can take a while)
      my_zeros <-  FishData::add_missing_zeros(data_frame = as.data.frame(dIn), unique_sample_ID_colname = "HaulID",
                                               sample_colname = "TotalNo", species_colname = "Species",
                                               Method = "Fast", if_multiple_records = "Combine") # "Combine" sums the different observations
      # numcpue
      dIn2 <- survey.ns%>%
        select(HaulID, Year, Month, StatRec, ShootLong, ShootLat, Gear, HaulDur, Area.swept, Species, numcpue) %>%
        # filter(Species %in% c("Clupea harengus", "Sprattus sprattus", "Gadus morhua")) %>%
        arrange(Species)
      my_zeros2 <-  FishData::add_missing_zeros(data_frame = as.data.frame(dIn2), unique_sample_ID_colname = "HaulID",
                                                sample_colname = "numcpue", species_colname = "Species",
                                                Method = "Fast", if_multiple_records = "Combine")
      # numh
      dIn3 <- survey.ns%>%
        select(HaulID, Year, Month, StatRec, ShootLong, ShootLat, Gear, HaulDur, Area.swept, Species, numh) %>%
        # filter(Species %in% c("Clupea harengus", "Sprattus sprattus", "Gadus morhua")) %>%
        arrange(Species)
      my_zeros3 <-  FishData::add_missing_zeros(data_frame = as.data.frame(dIn3), unique_sample_ID_colname = "HaulID",
                                                sample_colname = "numh", species_colname = "Species",
                                                Method = "Fast", if_multiple_records = "Combine")
      nrow(my_zeros) == nrow(my_zeros2)
      nrow(my_zeros) == nrow(my_zeros3)
      
      dZeros <- merge(my_zeros, my_zeros2, 
                      by = c("HaulID", "Year", "Month", "StatRec", "ShootLong", "ShootLat", "Gear", "HaulDur", "Area.swept", "Species"),
                      all = TRUE)
      
      dZeros <- merge(dZeros, my_zeros3, 
                      by = c("HaulID", "Year", "Month", "StatRec", "ShootLong", "ShootLat", "Gear", "HaulDur", "Area.swept", "Species"),
                      all = TRUE)
      
      head(dZeros)
      
      nrow(dZeros[,c("HaulID", "Species")]) == nrow(dZeros)
      sum(is.na(dZeros))
      sum(is.na(dZeros$Area.swept)) == sum(is.na(dZeros))
      
      # save(dZeros, file="My data/data_w_zeros.RData")
      load("My data/data_w_zeros.RData") # dZeros
    
    
      View(dZeros[which(dZeros$numcpue == 0 & dZeros$numh != 0),])
      # Lots of Hauls from 1986 & 1991 for which numcpue == 0, but numh & TotalNo != 0  !!!!!! 
      # This is because those same Hauls, don't have swept area data, and therefore, it is impossible to calculate numcpue...
      # I'll set them as NAs
      dZeros$numcpue[which(is.na(dZeros$Area.swept))] <- NA
      View(dZeros[which(is.na(dZeros$Area.swept)),])
      # dZeros <- dZeros[which(!is.na(dZeros$Area.swept)),]
      sum(is.na(dZeros))-sum(is.na(dZeros$Area.swept))-sum(is.na(dZeros$numcpue)) == 0
    
      # b) Rearrange numh, numcpue & TotalNo:  each species as a column ---------
      # test <- dZeros%>%
      #   filter(!HaulID %in% my_miss)
      # table(my_miss %in% test$HaulID)
    
      
      
      # NUMCPUE (abundance/km2)
      numcpue <- maditr::dcast(data = dZeros%>%
                                 # filter(!HaulID %in% my_miss)%>% # Exclude Hauls without Swept area data
                                 select(-TotalNo, -numh)%>%
                                 mutate(# numcpue = round(numcpue), # HMSC neds integer for lognormal poisson, so i'll round the numcpue (abundance/km2)
                                   Species = as.character(Species))%>% 
                                 arrange(Species), # Sort alphabetically 
                               Year + Month + StatRec + ShootLong + ShootLat + Gear + HaulID + HaulDur + Area.swept ~ Species,
                               value.var = "numcpue")
      head(numcpue)
      sum(is.na(numcpue))
      sum(is.na(numcpue$Area.swept))*length(unique(dZeros$Species)) == sum(is.na(dZeros$Area.swept))
      # save(numcpue, file="My data/matrix_numcpue.RData")
      load("My data/matrix_numcpue.RData") # numcpue
      
      
      
      # NUMH (abundance/hour)
      numh <- maditr::dcast(data = dZeros%>%
                              select(-TotalNo, -numcpue)%>%
                              mutate(# numcpue = round(numcpue), # HMSC neds integer for lognormal poisson, so i'll round the numcpue (abundance/km2)
                                Species = as.character(Species))%>% 
                              arrange(Species), # Sort alphabetically 
                            Year + Month + StatRec + ShootLong + ShootLat + Gear + HaulID + HaulDur + Area.swept ~ Species,
                            value.var = "numh")
      head(numh)
      sum(is.na(numh))
      sum(is.na(numh$Area.swept))
      # save(numh, file="My data/matrix_numh.RData")
      load("My data/matrix_numh.RData") # numh
      
      
      
      # TotalNo (total abundance)
      TotalNo <- maditr::dcast(data = dZeros%>%
                                 select(-numh, -numcpue)%>%
                                 mutate(# numcpue = round(numcpue), # HMSC neds integer for lognormal poisson, so i'll round the numcpue (abundance/km2)
                                   Species = as.character(Species))%>% 
                                 arrange(Species), # Sort alphabetically 
                               Year + Month + StatRec + ShootLong + ShootLat + Gear + HaulID + HaulDur + Area.swept ~ Species,
                               value.var = "TotalNo")
      head(TotalNo)
      sum(is.na(TotalNo))
      sum(is.na(TotalNo$Area.swept))
      # save(TotalNo, file="My data/matrix_TotalNo.RData")
      load("My data/matrix_TotalNo.RData") # TotalNo
      
      
    
# 8 - Match environment and hauls (NUMCPUE) with Nearest Neighbour --------
load("My data/matrix_numcpue.RData") # numcpue
sort(unique(numcpue$Year))
numcpue <- subset(numcpue, Year %in% 1980:2020)

# Environmental data
NS_1980_2029 <- readRDS("Data sources/Environment RCP 8.5/NS_1980_2029.rds")

# Select unique locations with environmental data
coordinates <- unique(NS_1980_2029[,c("longitude", "latitude")])
nrow(NS_1980_2029) == nrow(coordinates)*length(unique(NS_1980_2029$Year))*length(unique(NS_1980_2029$Month))
rownames(coordinates) <- NULL
coordinates$nR <- as.numeric(rownames(coordinates)) # Add row number as variable to match the datasets further on (see below)
# View(NS_1980_2029[1:1300,])

# Check if coordinates limits are wider than haul ones
range(coordinates$longitude)
range(numcpue$ShootLong)

range(coordinates$latitude)
range(numcpue$ShootLat)


# https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/
# fast nearest neighbour search
?nn2()
closest <- nn2(coordinates[,c("longitude", "latitude")], numcpue[,c("ShootLong", "ShootLat")],
               k = 1, searchtype = "radius", radius = 0.5)
closest <- sapply(closest, cbind) %>% as_tibble
head(closest)
# nn.idx refers to the rownumber on coordinates dataframe
# nn.dist refers to the distance to that location

neighbour <- cbind(nn.idx = closest$nn.idx, numcpue)
neighbour <- merge(coordinates, neighbour, by.x = "nR", by.y = "nn.idx", all.x = FALSE, all.y = TRUE)

test <- neighbour%>%mutate(xDist = abs(longitude - ShootLong), yDist = abs(latitude - ShootLat))
max(test[,c("xDist", "yDist")]) # 0.33, GOOD!

neighbour$nn.idx[1] %in% coordinates$nR

names(NS_1980_2029)
names(neighbour)[1:15]
neighbour <- merge(NS_1980_2029, neighbour, by = c("longitude", "latitude", "Year", "Month"), all.x = FALSE, all.y = TRUE)

sum(is.na(neighbour))
sum(is.na(neighbour$Sediment_type)) 
# 37 samplings for which we don't have sediment type data. 
# An option to fix that would be to ommit those points without sediment type from NS_1980_2020. Then This 37 samplings
# would be assigned to the next nearest neighboring point, which wouldn't be likely to change.


# View(neighbour[is.na(neighbour$Sediment_type),])
names(neighbour)[1:33]
sum(is.na(neighbour)) == sum(is.na(neighbour$Area.swept))*(279-31) +  sum(is.na(neighbour$Sediment_type)) + sum(is.na(neighbour$Sediment_ID))
# 279-31 = number of species + Area swept (each of those columns has NAs because of the Swept area NAs)

names(neighbour)[1:2] <- c("NMlongitude", "NMlatitude")

ggplot() + 
  borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(neighbour$ShootLong),ylim=range(neighbour$ShootLat)) +
  geom_point(data=subset(neighbour, Year == 2001 & Month == 1), aes(x=ShootLong, y=ShootLat, color = Temperature_floor), size = 2)




# Save NUMCPUE with NEMO - MEDUSA environmental data
numcpue_nm <- neighbour
numcpue_nm$nR <- NULL

# save(numcpue_nm, file = "My data/numcpue_nemo_medusa.RData")
load("My data/numcpue_nemo_medusa.RData")




# 9 - Add fishing data to NUMCPUE (with NEMO MEDUSA data) -----------------

      # a) Prepare fishing effort data for use ----------------------------------
      # Spatial distribution of total international otter and beam trawling fishing effort, for all ICES rectangles (0.5 degree 
      # latitude by 1 degree longitude) of the North Sea, covering a 31-year time span from 1985 to 2015. The dataset was largely 
      # reconstructed using compiled effort data from 7 fishing effort time-series and completed by estimations of missing data. 
      # Lat and Long coordinates in the data represent the centre of each ICES rectangle.
      # The effort was quantified as number of hours fishing in a year per ICES rectangle. 
      # For each year and ICES rectangle the dataset specifies "Compiled" and "Estimated" effort, and additionally provides a 
      # "Reconstructed" effort which is the sum of the two.
      
      Trawl <- read.csv(file = "Data sources/NorthSea_trawling_effort_1985to2015.csv",sep = ",", header = TRUE) 
      head(Trawl)
      unique(Trawl$ICESrect)
      Trawl$ICESrect <- substr(Trawl$ICESrect, start=2, stop=5)
      Trawl <- Trawl%>%arrange(Year, ICESrect, lon, lat)
      Trawl$StatRec <- as.factor(Trawl$ICESrect)
      
      #Keep only reconstructed fishing effort
      Trawl <- Trawl%>%filter(Datatype == "Reconstructed")
      # Obtain effort for each gear
      names(Trawl)[1] <- "Gear"
      Beams <- Trawl%>%filter(Gear == "Beam")%>%mutate(BeamHours = Hourstrawling)
      Otters <- Trawl%>%filter(Gear == "Otter")%>%mutate(OtterHours = Hourstrawling)
      # Merge them as different columns
      BothGear <- merge(Beams[,c("Year", "StatRec","BeamHours")], Otters[,c("Year", "StatRec","OtterHours")], by=c("Year", "StatRec"), all.x=T, all.y=T)
      
      # If one looks at Couce et al (2020), all the North sea fishing effort is covered and some areas have 0 effort. So set NAs to 0.
      BothGear$BeamHours[which(is.na(BothGear$BeamHours))] <- 0
      BothGear$OtterHours[which(is.na(BothGear$OtterHours))] <- 0
      
      # Rectangle 42E8 for 2009 and 47E7 for 2011 have both 0 effort for Otter and Beam and so they shall be set to 0 manually
      names(BothGear)
      add <- data.frame(Year = c(2009, 2011), StatRec = c("42E8", "47E7"), BeamHours = c(0,0), OtterHours = c(0,0))
      BothGear <-  rbind(BothGear, add)
      
      
      # Calculate total fishing effort
      BothGear$TotalFishingHours <- rowSums(BothGear[,c("OtterHours", "BeamHours")], na.rm=TRUE)
      sum(is.na(BothGear))
      
      FishingEffort <- BothGear
      
      # save(FishingEffort, file="My data/Fishing_effort_data.RData")
      load("My data/Fishing_effort_data.RData") # FishingEffort
      
      
      
      
      
      # b) Add fishing data to numcpue --------------------------------------------
      load("My data/numcpue_nemo_medusa.RData")
      numcpue_nm$StatRec <- as.factor(numcpue_nm$StatRec)
      sum(is.na(numcpue_nm)) ==
        sum(is.na(numcpue_nm$Area.swept))*(279-31) +
        sum(is.na(numcpue_nm$Sediment_type)) +
        sum(is.na(numcpue_nm$Sediment_ID))
      
      # Lag fishing
      library(reshape2)
      my_lag <- FishingEffort[,c("Year", "StatRec", "BeamHours", "OtterHours", "TotalFishingHours")]
      my_lag$Year <- my_lag$Year + 1
      my_lag <- my_lag%>%
        rename(BeamHours_lag = BeamHours,
               OtterHours_lag = OtterHours,
               TotalFishingHours_lag = TotalFishingHours)%>%
        filter(Year %in% c(1980:2019))
      
      fishing_all <- merge(FishingEffort, my_lag, by = c("Year", "StatRec"), all.x = TRUE, all.y = TRUE)
      
      # Merge
      numcpue_nm_f <- merge(fishing_all, numcpue_nm, by = c("Year", "StatRec"), all.x = FALSE, all.y = TRUE)
      sum(is.na(numcpue_nm_f))
      sum(is.na(numcpue_nm_f[numcpue_nm_f$Year %in% c(1985:2015),]))
      sum(is.na(numcpue_nm_f$TotalFishingHours)) == sum(is.na(numcpue_nm_f$TotalFishingHours[numcpue_nm_f$Year %in% c(1985:2015)]))
      
      # Check missing fishing data for years outside 1985-2015
      Miss_fishing <- data.frame(StatRec = unique(numcpue_nm_f$StatRec[is.na(numcpue_nm_f$TotalFishingHours) & numcpue_nm_f$Year %in% c(1985:2015)]))
      Miss_fishing[,c("lon", "lat")]<- ices.rect(Miss_fishing$StatRec)
      
      ggplot() +
        borders(fill="gray44",colour="gray61") + coord_quickmap(xlim=range(Miss_fishing$lon),ylim=range(Miss_fishing$lat)) +
        geom_point(data=Miss_fishing, aes(x=lon, y=lat), size = 2)
      # The rectangles with missing fishing effort for the years within 1985-2015, are actually outside the North Sea.
      
      # I will remove those rectangles:
      numcpue_nm_f <- droplevels(subset(numcpue_nm_f, !(StatRec %in% Miss_fishing$StatRec)))
      sum(is.na(numcpue_nm_f$TotalFishingHours[numcpue_nm_f$Year %in% c(1985:2015)]))
      
      
      # All NAs are accounted for and known:
      sum(is.na(numcpue_nm_f)) == 
        sum(is.na(numcpue_nm_f$TotalFishingHours)) + 
        sum(is.na(numcpue_nm_f$BeamHours)) + 
        sum(is.na(numcpue_nm_f$OtterHours)) +
        sum(is.na(numcpue_nm_f$TotalFishingHours_lag)) + 
        sum(is.na(numcpue_nm_f$BeamHours_lag)) + 
        sum(is.na(numcpue_nm_f$OtterHours_lag)) +
        sum(is.na(numcpue_nm_f$Area.swept))*(279-31) +
        sum(is.na(numcpue_nm_f$Sediment_type)) +
        sum(is.na(numcpue_nm_f$Sediment_ID))
  
      
      # Add season
      sort(unique(numcpue_nm_f$Month))
      numcpue_nm_f$Season[numcpue_nm_f$Month %in% c(1:3)] <- "Winter"
      numcpue_nm_f$Season[numcpue_nm_f$Month %in% c(6:9)] <- "Summer"
      numcpue_nm_f <- numcpue_nm_f[,c(ncol(numcpue_nm_f), 1:(ncol(numcpue_nm_f)-1))]
      head(numcpue_nm_f[,1:10])  
      
      
      # save(numcpue_nm_f, file = "My data/numcpue_nemo_medusa_fishing.RData")
      load("My data/numcpue_nemo_medusa_fishing.RData")
    test <- subset(numcpue_nm_f, `Sprattus sprattus` > 10000000)
      
    
# 10 - Add substrate richness data (NOT INCLUDED, but ready to run) --------
# substrateS <- read.csv(file = "Data sources/Esther covariates/cov_subarea.csv")
# names(substrateS)
# names(substrateS)[1] <- "StatRec"
# length(unique(substrateS$StatRec))
# length(unique(substrateS$StatRec[which(is.na(substrateS$Substrate_R))]))
# 
# load("My data/numcpue_nemo_medusa_fishing.RData")
# 
# numcpue_nm_f_sr <- merge(substrateS[,c("StatRec", "Substrate_R", "Depth")], numcpue_nm_f, by = "StatRec")
# # save(numcpue_nm_f_sr, file="My data/numcpue_nemo_medusa_fishing_substrate_richness.RData")

  
  

# 11 - Create taxonomic data ----------------------------------------------
load('Data sources/DATRAS/HL.28.02.2021.RData')
head(hl)

ns.survey <- subset(hl, Survey == "NS-IBTS")
ns.survey$AphiaID <- ns.survey$Valid_Aphia

myAphia <- na.omit(unique(ns.survey$AphiaID))


# creating taxonomy tables for each species
my_sp_taxo <- wm_record_(id = myAphia)

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

my_names <- df_test[,c("AphiaID", "phylum", "class", "order", "family", "genus", "valid_name")]
names(my_names) <- c("AphiaID", "Phylum", "Class", "Order", "Family", "Genus", "ScientificName")

# Code to integrate from Anna Rindorf on species bycatch corrections
my_names <- my_names %>%
  mutate(Species = recode(ScientificName,'Dipturus batis'='Dipturus','Dipturus flossada'='Dipturus',
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

my_names$ScientificName <- NULL


# Check for missing names
sum(is.na(my_names$Phylum))
sum(is.na(my_names$Class))
sum(is.na(my_names$Order))
sum(is.na(my_names$Family)) # 1 it has only been identified to Order (Rajiformes)
sum(is.na(my_names$Genus))  # 30 have only been identified to Family level.
# sum(is.na(my_names$Species)) # Are the same 30 (including Rajifomres)

my_taxonomy <- my_names
# save(my_taxonomy, file = "My data/NS_taxonomy.RData")
load("My data/NS_taxonomy.RData") # my_taxonomy
  