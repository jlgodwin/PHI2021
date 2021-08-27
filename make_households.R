# Setup ####
## Set Working Directory ####
setwd('U:/assessor')
list.files()

## Libraries ####
library(dplyr)
library(tidyr)

## Parameters ####
todays.date <- "20210810"


# Load data ####
## read.csv() ####

lookup <- read.csv('EXTR_LookUp.csv')
apts <- read.csv('Apartment Complex/EXTR_AptComplex.csv')
condos <- read.csv('Condo Complex and Units/EXTR_CondoComplex.csv')
resis <- read.csv('Residential Building/EXTR_ResBldg.csv')
parcels <- read.csv('EXTR_Parcel.csv')
envir <- read.csv("Environmental Restriction/EXTR_EnvironmentalRestriction_V.csv")

## Exploratory ####
names(lookup)
names(apts)
names(condos)
names(resis)
names(parcels)
names(envir)

head(apts)
head(condos)
head(resis)
head(parcels)
head(envir)


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


# Envir ####
table(envir$Type)
table(envir$Source)
table(envir$PcntAffected)
table(envir$UpdatedBy)

# Households ####

## by_zipcode ####

### Apartments ####

table(apts$ConstrClass)
apts$ConstrClass <- factor(apts$ConstrClass,
                           levels = 0:5,
                           labels = c("Unknown", "Strutural steel",
                                      "Reinforced concrete",
                                      "Masonry",
                                      "Wood frame",
                                      "Prefab steel"))

by_zipcode <- list()
by_zipcode[["apts"]] <- apts %>%
  select(Major, Minor,
         ComplexDescr,
         ConstrClass,
         NbrBldgs,
         NbrUnits,
         AvgUnitSize,
         Address) %>%
  left_join()

#### Join with parcels and envir ####

apts_in_parcels <- apts %>% 
  group_by(Major, Minor) %>% 
  left_join(parcels %>%
              group_by(Major, Minor) %>%
              select(DistrictName)) %>%
  left_join(envir)

apts_in_parcels %>% 
  as.data.frame() %>% 
  head()

apts_in_parcels %>% names()

##### Hazards ####

table(apts_in_parcels$Type, useNA = 'ifany')
table(apts_in_parcels$DistrictName, useNA = 'ifany')
table(apts_in_parcels$Type,
      apts_in_parcels$DistrictName, useNA = 'ifany')

#### Make ZipCode ####

## Grab the zipcode testing
if(FALSE){
  nchar(apts$Address[1])
  substr(apts$Address[1],
         start = nchar(apts$Address[1]) - 5 + 1,
         stop = nchar(apts$Address[1]))
}
by_zipcode$apts$ZipCode <- substr(by_zipcode$apts$Address,
                                  start = nchar(by_zipcode$apts$Address) - 5 + 1,
                                  stop = nchar(by_zipcode$apts$Address))
by_zipcode$apts$ZipCodeQual <- ifelse(substr(by_zipcode$apts$ZipCode, 1, 1) == 9 &
                                        nchar(by_zipcode$apts$ZipCode) == 5,
                                      1, 0)
table(by_zipcode$apts$ZipCode)
table(by_zipcode$apts$ZipCodeQual)

### Condos ####
table(condos$ConstrClass)
condos$ConstrClass <- factor(condos$ConstrClass,
                           levels = 0:5,
                           labels = c("Unknown", "Strutural steel",
                                      "Reinforced concrete",
                                      "Masonry",
                                      "Wood frame",
                                      "Prefab steel"))
table(condos$ConstrClass)

by_zipcode[["condos"]] <- condos %>%
  select(Major, ComplexType,
         ComplexDescr, NbrBldgs,
         NbrUnits,
         AvgUnitSize,
         ZipCode) 


table(by_zipcode$condos$ZipCode)
by_zipcode$condos$ZipCodeQual <- ifelse(substr(by_zipcode$condos$ZipCode, 1, 1) == 9 &
                                          nchar(by_zipcode$condos$ZipCode) == 5,
                                        1, 0)
table(by_zipcode$condos$ZipCodeQual)

#### Join with parcels and envir ####

condos_in_parcels <- condos %>% 
  group_by(Major) %>% 
  left_join(parcels %>%
              group_by(Major, Minor) %>%
              select(DistrictName)) %>%
  left_join(envir)

condos_in_parcels %>% 
  as.data.frame() %>% 
  head()

condos_in_parcels %>% names()
##### Hazards ####

table(condos_in_parcels$Type, useNA = 'ifany')
table(condos_in_parcels$DistrictName, useNA = 'ifany')
table(condos_in_parcels$Type,
      condos_in_parcels$DistrictName, useNA = 'ifany')



### Residential Buildings ####
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
  group_by(Major, Minor) %>% 
  left_join(parcels %>%
              group_by(Major, Minor) %>%
              select(DistrictName)) %>%
  left_join(envir)

resis_in_parcels %>% 
  as.data.frame() %>% 
  head()

resis_in_parcels %>% names()

##### Hazards ####

table(resis_in_parcels$Type, useNA = 'ifany')
table(resis_in_parcels$DistrictName, useNA = 'ifany')
table(resis_in_parcels$Type,
      resis_in_parcels$DistrictName, useNA = 'ifany')



by_zipcode[["resis"]] <- resis %>%
  select(Major, Minor,
         BldgNbr,
         NbrLivingUnits,
         BuildingNumber,
         Bedrooms,
         SqFtTotLiving,
         BathFullCount,
         ZipCode) %>%
  rename(NbrUnits = NbrLivingUnits)

table(by_zipcode$resis$ZipCode)
by_zipcode$resis$ZipCodeQual <- ifelse(substr(by_zipcode$resis$ZipCode, 1, 1) == 9 &
                                          nchar(by_zipcode$resis$ZipCode) == 5,
                                        1, 0)
table(by_zipcode$resis$ZipCodeQual)

### Aggregate to clean ZipCodes ####
hh.idx <- 0

for(hh.type in names(by_zipcode)){
  hh.idx <- hh.idx + 1
  
  by_zipcode[[hh.idx]] <-
    by_zipcode[[hh.idx]] %>%
    mutate(HouseholdType = hh.type) %>%
    filter(ZipCodeQual == 1) %>%
    group_by(ZipCode, HouseholdType) %>%
    summarise(NbrUnits = sum(NbrUnits))
}
#### save() ####

households <- do.call(rbind.data.frame, by_zipcode)
save(households,
     file = paste0('households_zip_',
                   todays.date,
                   '.rda'))
