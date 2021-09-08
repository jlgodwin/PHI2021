# Setup ####
## Set Working Directory ####
setwd('~/Dropbox/PHI2021/Github/PHI-2021/Tutorials/Assessor_data/')

## Clear environment
rm(list = ls())

## Libraries ####
library(dplyr)
library(tidyr)
library(rgdal)

## Parameters ####
todays.date <- "20210908"


# Load data ####
## parcels_to_hra ####
load('parcels_to_hra.rda')

## hra shapefile ####
hra <- readOGR(dsn = '../../Data',
               layer = "HRA_2010Block_Clip")

## read.csv() ####

## Juliette-- double dots mean
## "one folder above my working directory"
## list.files() allows you to see the files
## in a certain "relative path"-- referring
## to a file path relative to fixed directory
## on your machine-- in R this is the working directory

list.files('../../../..')
lookup <- read.csv('../../../../EXTR_LookUp.csv')
apts <- read.csv('../../../../EXTR_AptComplex.csv')
condos <- read.csv('../../../../EXTR_CondoComplex.csv')
resis <- read.csv('../../../../EXTR_ResBldg.csv')
parcels <- read.csv('../../../../EXTR_Parcel.csv')
envir <- read.csv("../../../../EXTR_EnvironmentalRestriction_V.csv")
homeimp <- read.csv("../../../../EXTR_HomeImpApp.csv")
units <- read.csv('../../../../EXTR_UnitBreakdown.csv')

## Exploratory ####
names(lookup)
names(apts)
names(condos)
names(resis)
names(parcels)
names(envir)
names(homeimp)
names(units)

head(apts)
head(condos)
head(resis)
head(parcels)
head(envir)
head(homeimp)

# Parcels ####
table(parcels$Township, useNA = 'ifany')
table(parcels$SubArea, useNA = 'ifany')
table(parcels$DistrictName, useNA = 'ifany')
table(parcels$LevyCode, useNA = 'ifany')
table(parcels$HundredYrFloodPlain, useNA = 'ifany')
table(parcels$PresentUse, useNA = 'ifany')
table(parcels$SeismicHazard, useNA = 'ifany')
table(parcels$LandslideHazard, useNA = 'ifany')
table(parcels$ErosionHazard, useNA = 'ifany')
table(parcels$WaterProblems, useNA = 'ifany')
table(parcels$CurrentZoning, useNA = 'ifany')
table(parcels$Topography, useNA = 'ifany')
table(parcels$TrafficNoise, useNA = 'ifany')

table(parcels$CurrentUseDesignation, useNA = 'ifany')
parcels$CurrentUseDesignation <- factor(parcels$CurrentUseDesignation,
                                        levels = 0:5,
                                        labels = c("Unknown",
                                                   "Agriculture",
                                                   "Forest",
                                                   "Green",
                                                   "DSFRS",
                                                   "CLFRS"))

table(nchar(parcels$Major))
parcels$MAJOR <- parcels$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(parcels$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- parcels$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    parcels$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(parcels$Minor))
parcels$MINOR <- parcels$Minor

for(nchar.idx in 1:4){
  fix.up.idx <- which(nchar(parcels$Minor %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MINOR_tmp <- parcels$Minor[fix.up.idx]
    
    if(nchar.idx < 4){
      zeroes <- paste(rep(0,4 - nchar.idx),
                      collapse = "")
      MINOR_fixed <- paste0(zeroes, MINOR_tmp)
    }else{
      MINOR_fixed <- as.character(MINOR_tmp)
    }
    
    parcels$MINOR[fix.up.idx] <- MINOR_fixed
  }
}

parcels <- parcels %>% 
  mutate(SECTION = ifelse(nchar(Section) == 2,
                          Section, paste0("0",Section)),
         TOWNSHIP= ifelse(nchar(Township) == 2,
                          Township, paste0("0", Township)),
         RANGE = ifelse(nchar(Range) == 2,
                        Range, paste0("0",Range))) %>% 
  mutate(MAJOR2 = paste0(SECTION, TOWNSHIP, RANGE))
# Envir ####
table(envir$Type)
table(envir$Source)
table(envir$PcntAffected)
table(envir$UpdatedBy)

table(nchar(envir$Major))
envir$MAJOR <- envir$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(envir$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- envir$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    envir$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(envir$Minor))
envir$MINOR <- envir$Minor

for(nchar.idx in 1:4){
  fix.up.idx <- which(nchar(envir$Minor %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MINOR_tmp <- envir$Minor[fix.up.idx]
    
    if(nchar.idx < 4){
      zeroes <- paste(rep(0,4 - nchar.idx),
                      collapse = "")
      MINOR_fixed <- paste0(zeroes, MINOR_tmp)
    }else{
      MINOR_fixed <- as.character(MINOR_tmp)
    }
    
    envir$MINOR[fix.up.idx] <- MINOR_fixed
  }
}

# Home Improvement ####
## Juliette--may be something
## for us to look into for case studies?
table(nchar(homeimp$Major))
homeimp$MAJOR <- homeimp$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(homeimp$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- homeimp$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    homeimp$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(homeimp$Minor))
homeimp$MINOR <- homeimp$Minor

for(nchar.idx in 1:4){
  fix.up.idx <- which(nchar(homeimp$Minor %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MINOR_tmp <- homeimp$Minor[fix.up.idx]
    
    if(nchar.idx < 4){
      zeroes <- paste(rep(0,4 - nchar.idx),
                      collapse = "")
      MINOR_fixed <- paste0(zeroes, MINOR_tmp)
    }else{
      MINOR_fixed <- as.character(MINOR_tmp)
    }
    
    homeimp$MINOR[fix.up.idx] <- MINOR_fixed
  }
}

table(homeimp$MaintOrReplace)
table(homeimp$MultipleDwellings)
table(homeimp$PersonalProperty)
summary(homeimp$EstCost)


# Unit breakdown ####
table(units$UnitTypeItemId)


# Households by type ####


### Apartments ####

table(apts$ConstrClass)
apts$ConstrClass <- factor(apts$ConstrClass,
                           levels = 0:5,
                           labels = c("Unknown", "Strutural steel",
                                      "Reinforced concrete",
                                      "Masonry",
                                      "Wood frame",
                                      "Prefab steel"))

#### Join with parcels + other datasets ####
table(nchar(apts$Major))
apts$MAJOR <- apts$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(apts$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- apts$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    apts$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(apts$Minor))
apts$MINOR <- apts$Minor

for(nchar.idx in 1:4){
  fix.up.idx <- which(nchar(apts$Minor %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MINOR_tmp <- apts$Minor[fix.up.idx]
    
    if(nchar.idx < 4){
      zeroes <- paste(rep(0,4 - nchar.idx),
                      collapse = "")
      MINOR_fixed <- paste0(zeroes, MINOR_tmp)
    }else{
      MINOR_fixed <- as.character(MINOR_tmp)
    }
    
    apts$MINOR[fix.up.idx] <- MINOR_fixed
  }
}
apts_in_parcels <- apts %>% 
  group_by(Major, MINOR) %>% 
  left_join(parcels %>%
              group_by(MAJOR, MAJOR2, MINOR) %>%
              ## Select parcel vars of interest
              dplyr::select(Major, PlatName,
                            Section,
                            Township,
                            Range,
                            DistrictName,
                            PropType,
                            LevyCode,
                            CurrentUseDesignation) %>% 
              ungroup(),
            by = c("Major" = "Major",
                   "MINOR" = "MINOR"),
             suffix = c("_apt", "_parcels")) %>%
  # left_join(envir) %>%
  # left_join(homeimp)

apts_in_parcels %>% 
  as.data.frame() %>% 
  head()

apts_in_parcels %>% names()

##### Hazards ####

table(apts_in_parcels$Type, useNA = 'ifany')
table(apts_in_parcels$DistrictName, useNA = 'ifany')
table(apts_in_parcels$Type,
      apts_in_parcels$DistrictName, useNA = 'ifany')

## tidyverse/piping alternative to above
apts_in_parcels %>%
  group_by(DistrictName, Type) %>%
  summarise(N = n())

## OR group by hazard Type first

apts_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n()) 


## Select a specific hazard to look at?

###### Seismic ####
apts_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "SeismicHazard")

###### Hundred Year Flood Plain ####
apts_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "HundredYrFloodPlain")

### Condos ####
table(nchar(condos$Major))
condos$MAJOR <- condos$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(condos$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- condos$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    condos$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(condos$Minor))
condos$MINOR <- "0000"


table(condos$ConstrClass)
condos$ConstrClass <- factor(condos$ConstrClass,
                             levels = 0:5,
                             labels = c("Unknown", "Strutural steel",
                                        "Reinforced concrete",
                                        "Masonry",
                                        "Wood frame",
                                        "Prefab steel"))
table(condos$ConstrClass)


#### Join with parcels and envir ####

condos_in_parcels <- condos %>% 
  group_by(MAJOR, MINOR) %>% 
  left_join(parcels %>%
              group_by(MAJOR, MINOR) %>%
              select(DistrictName)) %>%
  left_join(envir) %>%
  left_join(homeimp)

condos_in_parcels %>% 
  as.data.frame() %>% 
  head()

condos_in_parcels %>% names()
##### Hazards ####

table(condos_in_parcels$Type, useNA = 'ifany')
table(condos_in_parcels$DistrictName, useNA = 'ifany')
table(condos_in_parcels$Type,
      condos_in_parcels$DistrictName, useNA = 'ifany')

###### Seismic ####
condos_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "SeismicHazard")

###### Hundred Year Flood Plain ####
condos_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "HundredYrFloodPlain")


### Residential Buildings ####
table(nchar(resis$Major))
resis$MAJOR <- resis$Major
for(nchar.idx in 1:6){
  fix.up.idx <- which(nchar(resis$Major %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MAJOR_tmp <- resis$Major[fix.up.idx]
    
    if(nchar.idx < 6){
      zeroes <- paste(rep(0, 6 - nchar.idx),
                      collapse = "")
      MAJOR_fixed <- paste0(zeroes, MAJOR_tmp)
    }else{
      MAJOR_fixed <- as.character(MAJOR_tmp)
    }
    
    resis$MAJOR[fix.up.idx] <- MAJOR_fixed
  }
}

table(nchar(resis$Minor))
resis$MINOR <- resis$Minor

for(nchar.idx in 1:4){
  fix.up.idx <- which(nchar(resis$Minor %>% 
                              as.character()) == nchar.idx)
  
  if(length(fix.up.idx) != 0){
    MINOR_tmp <- resis$Minor[fix.up.idx]
    
    if(nchar.idx < 4){
      zeroes <- paste(rep(0,4 - nchar.idx),
                      collapse = "")
      MINOR_fixed <- paste0(zeroes, MINOR_tmp)
    }else{
      MINOR_fixed <- as.character(MINOR_tmp)
    }
    
    resis$MINOR[fix.up.idx] <- MINOR_fixed
  }
}

names(resis)
table(resis$HeatSource)
table(resis$HeatSystem)

resis$HeatSource <- factor(resis$HeatSource,
                           levels = 0:7,
                           labels = c( "Unknown",
                                       "Oil",
                                       "Gas",
                                       "Electricity",
                                       "Oil/Solar",
                                       "Gas/Solar",
                                       "Electricity/Solar",
                                       "Other"))
resis$HeatSystem <- factor(resis$HeatSystem,
                           levels = 0:8,
                           labels = c( "Unknown",
                                       "Floor-Wall",
                                       "Gravity",
                                       "Radiant",
                                       "Elec Bb",
                                       "Forced Air",
                                       "Hot Water",
                                       "Heat Pump",
                                       "Other"))

#### Join with parcels and envir ####

resis_in_parcels <- resis %>% 
  group_by(MAJOR, MINOR) %>% 
  left_join(parcels %>%
              group_by(MAJOR, MINOR) %>%
              select(DistrictName)) %>%
  left_join(envir) %>%
  left_join(homeimp)

resis_in_parcels %>% 
  as.data.frame() %>% 
  head()

resis_in_parcels %>% names()

##### Hazards ####

table(resis_in_parcels$Type, useNA = 'ifany')
table(resis_in_parcels$DistrictName, useNA = 'ifany')
table(resis_in_parcels$Type,
      resis_in_parcels$DistrictName, useNA = 'ifany')


###### Seismic ####
resis_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "SeismicHazard")

###### Hundred Year Flood Plain ####
resis_in_parcels %>%
  group_by(Type, DistrictName) %>%
  summarise(N = n())  %>%
  filter(Type == "HundredYrFloodPlain")


# Apts/Condos + Resi? ####

## Since Apts and Condos
## Don't have heating systems etc
## are they in the residential building file?
## let's see what happens?

apts_in_parcels <- 
  apts %>% 
  left_join(parcels_to_hra) %>% 
  filter(is.na(HRA2010v2_))

apts_in_resis_in_parcels %>% 
  group_by(Major, Minor, HRA2010v2_) %>% 
  summarise(n = n(),
            Bedrooms = sum(Bedrooms, na.rm = TRUE))

## okay this is weird?
## error about building number
## can we merge without BuildingNumber
resis_in_parcels %>%
  mutate(BuildingNumber = as.numeric(BuildingNumber)) %>% 
  left_join(condos_in_parcels) %>%
  nrow()

resis_in_parcels %>%
  left_join(condos_in_parcels %>%
              dplyr::select(-BuildingNumber)) %>%
  nrow()

## Aha! resis include Apts and Condos 
## Can we left join the other way?
apts_in_parcels %>%
  left_join(resis_in_parcels) %>%
  group_by(HeatSystem) %>%
  summarise(N = n())

## Lets aggregate apts, condos, residential buildings to hra ####
sum(!(apts_in_parcels$Major %in%
        parcels_to_hra$MAJOR))

sum(!(apts_in_parcels$Major %in%
        parcels_to_hra$MAJOR))
apts_in_parcels[!(apts_in_parcels$Major %in%
                    parcels_to_hra$MAJOR),] %>% 
  head() %>% 
  as.data.frame()
apts_in_hras <- apts_in_parcels %>% 
  left_join(parcels_to_hra)

all_households <- resis_in_parcels %>%
  left_join(apts_in_parcels) %>%
  left_join(condos_in_parcels %>%
              dplyr::select(-BuildingNumber))

nrow(all_households)

table(all_households$DistrictName)

## HeatSystem and HeatSource ####
## WOW no info for Apts! great to find out
all_households %>%
  group_by(HeatSystem,
           AptIndicator) %>%
  summarise(N = n())


heat_systems <- all_households %>%
  group_by(DistrictName,
           HeatSystem,
           HeatSource) %>%
  summarise(N = n())


## pick a district
heat_systems %>%
  filter(DistrictName == "SEATTLE")

## get rid of heat source
heat_systems %>%
  ungroup() %>%
  group_by(HeatSystem) %>%
  summarise(N = sum(N))

all_households %>%
  group_by(DistrictName,
           HeatSystem) %>%
  summarise(N = n()) %>%
  filter(DistrictName == "KENT")

