setwd('U:/assessor')
list.files()

library(dplyr)
library(tidyr)

lookup <- read.csv('EXTR_LookUp.csv')
apts <- read.csv('Apartment Complex/EXTR_AptComplex.csv')
condos <- read.csv('Condo Complex and Units/EXTR_CondoComplex.csv')
resis <- read.csv('Residential Building/EXTR_ResBldg.csv')


names(lookup)
names(apts)
names(condos)
names(resis)

head(apts)
head(condos)
head(resis)


by_zipcode <- list()


## Grab the zipcode testing
if(FALSE){
  nchar(apts$Address[1])
  substr(apts$Address[1],
         start = nchar(apts$Address[1]) - 5 + 1,
         stop = nchar(apts$Address[1]))
}

by_zipcode[["apts"]] <- apts %>%
  select(Major, Minor,
         ComplexDescr,
         NbrBldgs,
         NbrUnits,
         AvgUnitSize,
         Address) 

by_zipcode$apts$ZipCode <- substr(by_zipcode$apts$Address,
                                  start = nchar(by_zipcode$apts$Address) - 5 + 1,
                                  stop = nchar(by_zipcode$apts$Address))
by_zipcode$apts$ZipCodeQual <- ifelse(substr(by_zipcode$apts$ZipCode, 1, 1) == 9 &
                                        nchar(by_zipcode$apts$ZipCode) == 5,
                                      1, 0)
table(by_zipcode$apts$ZipCode)
table(by_zipcode$apts$ZipCodeQual)

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

households <- do.call(rbind.data.frame, by_zipcode)
save(households,
     file = 'households_20210809.rda')
