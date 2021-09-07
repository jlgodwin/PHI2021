# setwd() ####
setwd('~/Dropbox/PHI2021/Github/PHI-2021/Tutorials/')


# Libraries ####
library(tidycensus)
library(sf)
library(tidyr)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(geosphere)
library(raster)
library(RColorBrewer)
library(classInt)
library(scales) 
library(magrittr)
library(bayesPop)

# Clear environment ####
rm(list = ls())

## tidycensus key ####
source('tidycensus_APIkey.R')
census_api_key(myKey) 

## tidycensus shapefile cache ####
options(tigris_use_cache = TRUE)

## load WA OFM raw? ####
loadOFM <- FALSE

# Load data ####
## WA OFM ####
if(loadOFM){
pop <- read.csv("../Data/sade_all_2010_to_2020.csv") %>% 
  # filter(Year >= 2015) %>% 
  filter(Year == 2020) %>% 
  filter(Sumlev == "Census Tract") %>% 
  filter(grepl("53033", Geoid)) %>% 
  as.data.frame()

save(pop,
     file = '../Data/pop_2020_OFM.rda')
}else{
  load('../Data/pop_2020_OFM.rda')
}
## Census tracts ####

kc_tracts <- get_acs("tract",
                     table = "B01001",
                     geometry = TRUE,
                     year = 2019,
                     survey = "acs5",
                     state = "WA",
                     county = "King",
                     cache_table = TRUE) %>%
  filter(variable == "B01001_001")

names(kc_tracts)

kc_tracts_poly <- kc_tracts %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_geometry() %>%
  as(., "Spatial")

sum(kc_tracts$GEOID %in% 
      pop$Geoid)
which_is_empty <- which(st_is_empty(kc_tracts$geometry))
kc_tracts[which_is_empty, ]
pop[pop$Geoid == kc_tracts$GEOID[which_is_empty], ]

kc_tracts[which_is_empty, c("estimate", "moe")]
sum(pop$Pop[pop$Geoid == kc_tracts$GEOID[which_is_empty]])

## HRA ####
hra <- readOGR(dsn = "../Data",
               layer = "HRA_2010Block_Clip")

hra <- spTransform(hra,
                   kc_tracts_poly@proj4string)

## tracts_to_hra ####
load('../Data/tracts_to_hra.rda')

## Parcels ####

### Shape ####
parcels <- readOGR('../Data',
                   layer = "FLU_dissolve")
parcels <- spTransform(parcels,
                   kc_tracts_poly@proj4string)

juris <- unionSpatialPolygons(SpatialPolygons(parcels@polygons),
                              IDs = parcels@data$Jurisdicti)
juris_data <- parcels@data %>% 
  group_by(Jurisdicti) %>% 
  summarise(Nobs_in_parcels = n(),
            Res_Use = sum(Res_Use == "Y",
                          na.rm = TRUE),
            Mixed_Use = sum(Mixed_Use == "Y",
                            na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Res_Prop = Res_Use/Nobs_in_parcels,
            Mixed_Prop = Mixed_Use/Nobs_in_parcels) %>% 
  filter(!is.na(Jurisdicti)) %>% 
  as.data.frame()
row.names(juris_data) <- juris_data$Jurisdicti
juris <- SpatialPolygonsDataFrame(juris,
                                  data = juris_data)
table(parcels$Jurisdicti)
table(parcels$Zone_adj)

### Data ####

parcels_data <- read.csv('../../../EXTR_Parcel.csv')
parcels_data_dist <- unique(parcels_data$DistrictName)
rm(parcels_data)

# Population pyramids by HRA ####

hra_pop <- pop %>% 
  mutate(GEOID = as.character(Geoid)) %>% 
  left_join(tracts_to_hra$acs5_2019) %>% 
  group_by(FID_HRA_20, Sex_Lbl, Age, Age_Lbl) %>% 
  summarise(HRA = unique(HRA2010v2_),
            Pop = sum(Pop*prop.area)) %>% 
  filter(!is.na(HRA))
pop.cols <- brewer.pal(n = 5,
                       name = 'Blues')

pdf(paste0("../PopPlots/Pyramid_HRA_2020.pdf"),
    height = 5, width = 5)
for(hra.name in unique(hra_pop$HRA)){
  pyr.tmp <- hra_pop %>% 
    filter(HRA == hra.name) %>% 
    arrange(Age, Sex_Lbl) %>% 
    pivot_wider(id_cols = c(Age, Age_Lbl),
                names_from = Sex_Lbl,
                values_from = Pop) %>% 
    ungroup() %>% 
    dplyr::select(Female, Male) %>% 
    as.matrix()
  
  row.names(pyr.tmp) <- hra_pop %>% 
    arrange(Age, Age_Lbl) %>% 
    group_by(Age) %>% 
    summarise(Age_Lbl = unique(Age_Lbl)) %>% 
    ungroup() %>% 
    dplyr::select(Age_Lbl) %>% unlist()
  
  pyr.obj <- get.bPop.pyramid(pyr.tmp,
                              legend = "",
                              LRcolnames = c("Female", "Male"),
                              LRmain = c(hra.name, ""))
  
  par(lend = 1)
  plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                border = pop.cols[3]))
}
dev.off()

# Population maps by HRA####


