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
## Household Size ####

hh_tract <- readRDS('../household_size/hh_by_hh_size_and_tenure_ct.RDS')



## WA OFM ####
if(loadOFM){
  popall <- read.csv("../Data/sade_all_2010_to_2020.csv") %>% 
    # filter(Year >= 2015) %>% 
    # filter(Year == 2020) %>% 
    filter(Sumlev == "Census Tract") %>% 
    filter(grepl("53033", Geoid)) %>% 
    as.data.frame()
  
  for(year in c(2010, 2012, 2015, 2017, 2020)){
    pop <- popall %>% 
      filter(Year == year)
    save(pop,
         file = paste0('../Data/pop_',
                       year, '_OFM.rda'))
  }
  rm(popall)
}else{
  year <- 2020
  load('../Data/pop_2020_OFM.rda')
}

## ACS5 Year ests ####
load('../Tutorials/ACS5_ests_hra_and_tract.rda')
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

## Juris ####

### Shape ####
# jurisdictions <- readOGR('../Data',
#                    layer = "FLU_dissolve")
# jurisdictions <- spTransform(jurisdictions,
#                              kc_tracts_poly@proj4string)
# 
# juris <- unionSpatialPolygons(SpatialPolygons(parcels@polygons),
#                               IDs = parcels@data$Jurisdicti)
# juris_data <- jurisdictions@data %>% 
#   group_by(Jurisdicti) %>% 
#   summarise(Nobs = n(),
#             Res_Use = sum(Res_Use == "Y",
#                           na.rm = TRUE),
#             Mixed_Use = sum(Mixed_Use == "Y",
#                             na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(Res_Prop = Res_Use/Nobs,
#          Mixed_Prop = Mixed_Use/Nobs) %>% 
#   filter(!is.na(Jurisdicti)) %>% 
#   as.data.frame()
# row.names(juris_data) <- juris_data$Jurisdicti
# juris <- SpatialPolygonsDataFrame(juris,
#                                   data = juris_data)
# table(jurisdictions$Jurisdicti)
# table(jurisdictions$Zone_adj)

## Parcels ####

### Shape ####
# parcels <- readOGR(dsn = '../Data/King_County_Parcels___parcel_area/',
#                    layer = 'King_County_Parcels___parcel_area')
# ### Data ####
# 
# parcels_data <- read.csv('../../../EXTR_Parcel.csv')
# parcels_data_dist <- unique(parcels_data$DistrictName)
# rm(parcels_data)
pop.pyrs<-list()
for(year in c(2010, 2012, 2015,
              2017, 2020)){
  
  # Population (OFM) pyramids by HRA ####
  load(paste0('../Data/pop_', 
              year, '_OFM.rda'))  
  hra_pop <- pop %>% 
    mutate(GEOID = as.character(Geoid)) %>% 
    left_join(tracts_to_hra$acs5_2019) %>% 
    group_by(FID_HRA_20, Sex_Lbl, Age, Age_Lbl) %>% 
    summarise(HRA = unique(HRA2010v2_),
              Pop = sum(Pop*prop.area)) %>% 
    filter(!is.na(HRA))
  pop.cols <- brewer.pal(n = 5,
                         name = 'Blues')
  if(!dir.exists(paste0("../PopPlots/",
                        year, "/"))){
    dir.create(paste0("../PopPlots/",
                      year, "/"))
  }
  
  if(!dir.exists(paste0("../PopPlots/",
                        year, "/Pyramid/"))){
    dir.create(paste0("../PopPlots/",
                      year, "/Pyramid/"))
  }
  
  
  pdf(paste0("../PopPlots/",
             year, "/Pyramid/Pyramid_",
             year, ".pdf"),
      height = 5, width = 5)
  pyr.tmp <- hra_pop %>% 
    group_by(Age, Age_Lbl,Sex_Lbl) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
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
  pop.pyrs[[paste0("year_", year)]] <- pyr.tmp
  pyr.obj <- get.bPop.pyramid(pyr.tmp,
                              legend = paste0("OFM, ", year),
                              LRcolnames = c("Female", "Male"),
                              LRmain = c("King County", ""))
  
  
  par(lend = 1)
  plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                border = pop.cols[3]))
  dev.off()
  
  
  
  pdf(paste0("../PopPlots/",
             year, "/Pyramid/Pyramid_HRA_",
             year, ".pdf"),
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
                                show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c(hra.name, ""))
    
    par(lend = 1)
    plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                  border = pop.cols[3]))
  }
  dev.off()
  
  ## With County ####
  pdf(paste0("../PopPlots/", year,
             "/Pyramid/Pyramid_HRAandCounty_",
             year, ".pdf"),
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
    county.pyr <- hra_pop %>% 
      group_by(Age, Age_Lbl, Sex_Lbl) %>% 
      summarise(County_Sum = sum(Pop)) %>% 
      arrange(Age, Sex_Lbl) %>% 
      pivot_wider(id_cols = c(Age, Age_Lbl),
                  names_from = Sex_Lbl,
                  values_from = County_Sum) %>% 
      ungroup() %>% 
      dplyr::select(Female, Male) %>% 
      as.matrix()
    row.names(county.pyr) <-
      row.names(pyr.tmp) <- hra_pop %>% 
      arrange(Age, Age_Lbl) %>% 
      group_by(Age) %>% 
      summarise(Age_Lbl = unique(Age_Lbl)) %>% 
      ungroup() %>% 
      dplyr::select(Age_Lbl) %>% unlist()
    
    pyr.list <- list(county.pyr,
                     pyr.tmp)
    pyr.obj <- get.bPop.pyramid(pyr.list, show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    # county.pyr.obj <- get.bPop.pyramid(county.pyr,
    #                                    LRcolnames = c("Female", "Male"),
    #                                    LRmain = c("Female", "Male"))
    
    par(lend = 1)
    plot(pyr.obj, pyr1.par = list(col = pop.cols[4] , 
                                  border = pop.cols[4]),
         pyr2.par = list(col = pop.cols[2],
                         border = pop.cols[2]),
         main = hra.name)
  }
  dev.off()
  
  
  # Population maps by HRA####
  
  hra_age_pop <- hra_pop %>% 
    group_by(HRA, Age, Age_Lbl) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    arrange(Age)
  
  pop.int.hra <- classIntervals(hra_age_pop$Pop,
                                style = 'jenks',
                                n = 9)
  
  breaks <- pop.int.hra$brks
  breaks <- c(0, 750, 1500,
              2000, 3000, 4000,
              5000, 7500, 10000,
              12500)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  pop.pal <- brewer.pal(n = 9, name = "Blues")
  
  
  for(age in unique(hra_age_pop$Age_Lbl)){
    
    hra_age_tmp <- hra_age_pop %>% 
      filter(Age_Lbl == age)
    hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                     hra@data$HRA2010v2_), ]
    
    pop.int.hra <- classIntervals(hra_age_tmp$Pop,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    
    if(!dir.exists(paste0("../PopPlots/", year, "/"))){
      dir.create(paste0("../PopPlots/",
                        year, "/"))
    }
    if(!dir.exists(paste0("../PopPlots/",
                          year, "/OFM_Ages/"))){
      dir.create(paste0("../PopPlots/",
                        year, "/OFM_Ages/"))
    }
    
    if(!dir.exists(paste0("../PopPlots/", 
                          year, "/OFM_Ages/",
                          age, "/"))){
      dir.create(paste0("../PopPlots/", 
                        year, "/OFM_Ages/",
                        age, "/"))
    }
    pdf(paste0("../PopPlots/", 
               year, "/OFM_Ages/",
               age, "/OFM_",
               year, "_age", age,
               ".pdf"),
        height = 5, width = 5)
    plot(hra,
         col = pop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Population',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.5,
           border = FALSE,
           fill = pop.pal,
           legend = names(attr(pop.col.hra, 'table')))
    dev.off()
  }
  
  
  ### Prevalence of age group in HRA ####
  hra_total_pop <- hra_pop %>% 
    group_by(HRA) %>% 
    summarise(PopTotal = sum(Pop)) %>% 
    ungroup()
  
  hra_age_pop <- hra_pop %>% 
    group_by(HRA, Age, Age_Lbl) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    arrange(Age) %>% 
    left_join(hra_total_pop) %>% 
    group_by(HRA) %>% 
    mutate(AgePrev = Pop/PopTotal)
  
  prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  prop.int.hra <- classIntervals(hra_age_pop$AgePrev,
                                 style = 'jenks',
                                 n = 9)
  
  breaks <- prop.int.hra$brks
  breaks <- c(0, .005, .01,
              .03, .05, .07,
              .1, .125, .15, .16)
  
  for(age in unique(hra_age_pop$Age_Lbl)){
    
    hra_age_tmp <- hra_age_pop %>% 
      filter(Age_Lbl == age)
    hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                     hra@data$HRA2010v2_), ]
    
    prop.int.hra <- classIntervals(hra_age_tmp$AgePrev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    pdf(paste0("../PopPlots/", 
               year, "/OFM_Ages/",
               age, "/OFM_",
               year, "_agePrev_", age,
               ".pdf"),
        height = 5, width = 5)
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Prevalence',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.5,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    dev.off()
  }
  
  
  ### Distribution of age group across HRAs ####
  age_total_pop <- hra_pop %>% 
    group_by(Age, Age_Lbl) %>% 
    summarise(PopTotal = sum(Pop)) %>% 
    ungroup() %>% 
    arrange(Age)
  
  hra_age_pop <- hra_pop %>% 
    group_by(HRA, Age, Age_Lbl) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    arrange(Age) %>% 
    left_join(age_total_pop) %>% 
    group_by(HRA, Age, Age_Lbl) %>% 
    mutate(AgePrev = Pop/PopTotal)
  
  prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  prop.int.hra <- classIntervals(hra_age_pop$AgePrev,
                                 style = 'jenks',
                                 n = 9)
  
  breaks <- prop.int.hra$brks
  breaks <- c(0, .005, .01,
              .02, .03, .04,
              .05, .065, .08, .1)
  
  for(age in unique(hra_age_pop$Age_Lbl)){
    
    hra_age_tmp <- hra_age_pop %>% 
      filter(Age_Lbl == age)
    hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                     hra@data$HRA2010v2_), ]
    
    prop.int.hra <- classIntervals(hra_age_tmp$AgePrev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    pdf(paste0("../PopPlots/", 
               year, "/OFM_Ages/",
               age, "/OFM_",
               year, "_ageDist_", age,
               ".pdf"),
        height = 5, width = 5)
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Distribution',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.5,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    dev.off()
  }
}

pdf(paste0("../PopPlots/",
           "Pyramid_20102020.pdf"),
    height = 5, width = 5)

pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020, pop.pyrs$year_2010),
                            legend = c("OFM, 2020", "OFM, 2010"),
                            LRcolnames = c("Female", "Male"),
                            LRmain = c("King County", ""))

par(lend = 1)
plot(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                              border = pop.cols[5]),
     pyr2.par = list(col = pop.cols[3] , 
                     border = pop.cols[3]))
dev.off()

# Compare OFM & ACS ####

hra_total_pop <- 
  hra_pop <- list()

for(year in c(2010, 2012, 2015,
              2017, 2020)){
  
  load(paste0('../Data/pop_',
              year, '_OFM.rda'))
  hra_pop[[paste0("OFM_", year)]] <- pop %>% 
    mutate(GEOID = as.character(Geoid)) %>% 
    left_join(tracts_to_hra$acs5_2019) %>% 
    group_by(FID_HRA_20, Sex_Lbl, Age, Age_Lbl) %>% 
    summarise(HRA = unique(HRA2010v2_),
              Pop = sum(Pop*prop.area)) %>% 
    filter(!is.na(HRA))
  
  hra_total_pop[[paste0("OFM_", year)]] <- 
    hra_pop[[paste0("OFM_", year)]] %>% 
    group_by(HRA) %>% 
    summarise(Pop = sum(Pop))
}

acs_tmp <- pop_by_agegroup_hra$acs5_2019 %>% 
  group_by(FID_HRA_20,
           HRA2010v2_) %>% 
  summarise(estimate = sum(estimate),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/estimate) %>% 
  ungroup()

pdf('../PopPlots/2017/OFM_ACS5_Compare_2017.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2017$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2017",
     ylab = "ACS 2015-2019")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2017$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2017$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()


pdf('../PopPlots/2020/OFM_ACS5_Compare_2020.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2020$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2020",
     ylab = "ACS 2015-2019")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2020$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2020$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()


pdf('../PopPlots/2015/OFM_ACS5_Compare_2015.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2015$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2015",
     ylab = "ACS 2015-2019")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2015$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2015$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()


acs_tmp <- pop_by_agegroup_hra$acs5_2014 %>% 
  group_by(FID_HRA_20,
           HRA2010v2_) %>% 
  summarise(estimate = sum(estimate),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/estimate) %>% 
  ungroup()

pdf('../PopPlots/2012/OFM_ACS5_Compare_2012.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2012$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2012",
     ylab = "ACS 2010-2014")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2012$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2012$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()

pdf('../PopPlots/2010/OFM_ACS5_Compare_2010.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2010$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2010",
     ylab = "ACS 2010-2014")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2010$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2010$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()

pdf('../PopPlots/2015/OFM_ACS5_Compare2_2015.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2015$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2015",
     ylab = "ACS 2010-2014")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2015$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2015$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()

## By Age ####


pdf('../PopPlots/2020/OFM_ACS5_Compare_2020.pdf',
    height = 5, width = 5)
plot(hra_total_pop$OFM_2020$Pop,
     acs_tmp$estimate,
     pch = 19,
     col = pop.cols[3],
     xlim = c(0, 100000),
     ylim = c(0, 100000),
     xaxt = 'n',
     yaxt = 'n',
     xlab = "OFM, 2020",
     ylab = "ACS 2015-2019")
abline(0,1, lty = 2)
axis(1, at = seq(0, 100000, 25000))
axis(2, at = seq(0, 100000, 25000))
segments(hra_total_pop$OFM_2020$Pop,
         acs_tmp$estimate + 
           qnorm(.95)*acs_tmp$SE,
         hra_total_pop$OFM_2020$Pop,
         acs_tmp$estimate - 
           qnorm(.95)*acs_tmp$SE,
         col = pop.cols[3])
dev.off()


for(race in unique(hra_pop$Race_Lbl)){
  race.clean <- gsub(" and ", "/",
                     race)
  race.clean <- gsub(" or ", "/",
                     race.clean)
  
  
  for(year in c(2010, 2012, 2015,
                2017, 2020)){
    
    # Population Race Sex(OFM) pyramids by HRA ####
    load(paste0('../Data/pop_', 
                year, '_OFM.rda'))  
    hra_pop <- pop %>% 
      mutate(GEOID = as.character(Geoid)) %>% 
      left_join(tracts_to_hra$acs5_2019) %>% 
      group_by(FID_HRA_20, Race, Race_Lbl, Sex_Lbl, Age, Age_Lbl) %>% 
      summarise(HRA = unique(HRA2010v2_),
                Pop = sum(Pop*prop.area)) %>% 
      filter(!is.na(HRA)) %>% 
      arrange(Race,Age)
    pop.cols <- brewer.pal(n = 5,
                           name = 'Blues')
    
    
    
    
    pdf(paste0("../PopPlots/",
               year, "/Pyramid/Pyramid_",
               year, "_", race, ".pdf"),
        height = 5, width = 5)
    pyr.tmp <- hra_pop %>% 
      filter(Race_Lbl == race) %>% 
      group_by(Age, Age_Lbl,Sex_Lbl) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() %>% 
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
    pop.pyrs[[paste0("year_", year)]] <- pyr.tmp
    pyr.obj <- get.bPop.pyramid(pyr.tmp,
                                legend = paste0("OFM, ", year),
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("King County", race.clean))
    
    
    par(lend = 1)
    plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                  border = pop.cols[3]))
    dev.off()
    
  }
  
  pdf(paste0("../PopPlots/",
             "Pyramid_20102020_",
             race, ".pdf"),
      height = 5, width = 5)
  
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020, pop.pyrs$year_2010),
                              legend = c("OFM, 2020", "OFM, 2010"),
                              LRcolnames = c("Female", "Male"),
                              LRmain = c("King County", race.clean))
  
  par(lend = 1)
  plot(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                border = pop.cols[5]),
       pyr2.par = list(col = pop.cols[3] , 
                       border = pop.cols[3]))
  dev.off()
}  




for(year in c(2010, 2012, 2015,
              2017, 2020)){
  for(race in unique(hra_pop$Race_Lbl)){
    race.clean <- gsub(" and ", "/",
                       race)
    race.clean <- gsub(" or ", "/",
                       race.clean)
    
    # Population Race Sex(OFM) pyramids by HRA ####
    load(paste0('../Data/pop_', 
                year, '_OFM.rda'))  
    hra_pop <- pop %>% 
      mutate(GEOID = as.character(Geoid)) %>% 
      left_join(tracts_to_hra$acs5_2019) %>% 
      group_by(FID_HRA_20, Race, Race_Lbl, Sex_Lbl, Age, Age_Lbl) %>% 
      summarise(HRA = unique(HRA2010v2_),
                Pop = sum(Pop*prop.area)) %>% 
      filter(!is.na(HRA)) %>% 
      arrange(Race,Age)
    pop.cols <- brewer.pal(n = 5,
                           name = 'Blues')
    
    
    
    pdf(paste0("../PopPlots/",
               year, "/Pyramid/Pyramid_HRA_",
               year, "_", race, ".pdf"),
        height = 5, width = 5)
    for(hra.name in unique(hra_pop$HRA)){
      pyr.tmp <- hra_pop %>% 
        filter(HRA == hra.name) %>% 
        filter(Race_Lbl == race) %>% 
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
                                  legend = paste0("OFM, ", year),
                                  LRcolnames = c("Female", "Male"),
                                  LRmain = c(hra.name, race.clean))
      
      par(lend = 1)
      plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                    border = pop.cols[3]))
    }
    dev.off()
    
    ## With County ####
    
    
    pdf(paste0("../PopPlots/", year,
               "/Pyramid/Pyramid_HRAandCounty_",
               year, "_", race ,".pdf"),
        height = 5, width = 5)
    for(hra.name in unique(hra_pop$HRA)){
      pyr.tmp <- hra_pop %>%
        filter(HRA == hra.name) %>% 
        filter(Race_Lbl == race) %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = Pop) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      county.pyr <- hra_pop %>% 
        filter(Race_Lbl == race) %>% 
        group_by(Age, Age_Lbl, Sex_Lbl) %>% 
        summarise(County_Sum = sum(Pop)) %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = County_Sum) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      row.names(county.pyr) <-
        row.names(pyr.tmp) <- hra_pop %>% 
        arrange(Age, Age_Lbl) %>% 
        group_by(Age) %>% 
        summarise(Age_Lbl = unique(Age_Lbl)) %>% 
        ungroup() %>% 
        dplyr::select(Age_Lbl) %>% unlist()
      
      pyr.list <- list(county.pyr,
                       pyr.tmp)
      pyr.obj <- get.bPop.pyramid(pyr.list, 
                                  legend = c("King County", "HRA"),
                                  LRcolnames = c("Female", "Male"),
                                  LRmain = c("Female", "Male"))
      # county.pyr.obj <- get.bPop.pyramid(county.pyr,
      #                                    LRcolnames = c("Female", "Male"),
      #                                    LRmain = c("Female", "Male"))
      
      race.clean <- gsub(" and ", "/",
                         race)
      race.clean <- gsub(" or ", "/",
                         race.clean)
      
      par(lend = 1)
      plot(pyr.obj, pyr1.par = list(col = pop.cols[4] , 
                                    border = pop.cols[4]),
           pyr2.par = list(col = pop.cols[2],
                           border = pop.cols[2]),
           main = hra.name,
           sub = race.clean)
    }
    dev.off()
    
    # Population maps by HRA####
    
    hra_age_pop <- hra_pop %>% 
      filter(Race_Lbl == race) %>% 
      group_by(HRA, Age, Age_Lbl) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() %>% 
      arrange(Age)
    
    pop.int.hra <- classIntervals(hra_age_pop$Pop,
                                  style = 'jenks',
                                  n = 9)
    
    breaks <- pop.int.hra$brks
    breaks <- c(0, 250, 500,
                750, 1000, 1500,
                2000, 3000, 4000,
                5750)
    ## Get color based on RColorBrwere palette for 
    ## each area
    
    pop.pal <- brewer.pal(n = 9, name = "Blues")
    
    
    for(age in unique(hra_age_pop$Age_Lbl)){
      
      hra_age_tmp <- hra_age_pop %>% 
        filter(Age_Lbl == age)
      hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                       hra@data$HRA2010v2_), ]
      
      pop.int.hra <- classIntervals(hra_age_tmp$Pop,
                                    style = "fixed",
                                    fixedBreaks = breaks,
                                    n = 9)
      pop.col.hra <- findColours(pop.int.hra, pop.pal)
      
      
      pdf(paste0("../PopPlots/", 
                 year, "/OFM_Ages/",
                 age, "/OFM_",
                 year, "_age", age,
                 "_", race, ".pdf"),
          height = 5, width = 5)
      plot(hra,
           col = pop.col.hra,
           border = 'grey48', lwd = .25,
           main = race.clean,
           adj = 0)
      legend('bottomleft',
             title = 'Population',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.5,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.hra, 'table')))
      dev.off()
    }
    
    
    ### Prevalence of age group in HRA ####
    hra_total_pop <- hra_pop %>% 
      group_by(HRA) %>% 
      summarise(PopTotal = sum(Pop)) %>% 
      ungroup()
    
    hra_age_pop <- hra_pop %>% 
      filter(Race_Lbl == race) %>%
      group_by(HRA, Age, Age_Lbl) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() %>% 
      arrange(Age) %>% 
      left_join(hra_total_pop) %>% 
      group_by(HRA) %>% 
      mutate(AgePrev = Pop/PopTotal)
    
    prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
    
    prop.int.hra <- classIntervals(hra_age_pop$AgePrev,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- prop.int.hra$brks
    breaks <- c(0, .005, .01,
                .02, .03, .04,
                .05, .075, .10, .125)
    
    for(age in unique(hra_age_pop$Age_Lbl)){
      
      hra_age_tmp <- hra_age_pop %>% 
        filter(Age_Lbl == age)
      hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                       hra@data$HRA2010v2_), ]
      
      prop.int.hra <- classIntervals(hra_age_tmp$AgePrev,
                                     style = "fixed",
                                     fixedBreaks = breaks,
                                     n = 9)
      prop.col.hra <- findColours(prop.int.hra, prop.pal)
      
      pdf(paste0("../PopPlots/", 
                 year, "/OFM_Ages/",
                 age, "/OFM_",
                 year, "_agePrev_", age,
                 "_", race, ".pdf"),
          height = 5, width = 5)
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = race.clean,
           adj = 0)
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.5,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      dev.off()
    }
    
    
    ### Distribution of age/race group across HRAs ####
    age_total_pop <- hra_pop %>%
      filter(Race_Lbl == race) %>% 
      group_by(Age, Age_Lbl) %>% 
      summarise(PopTotal = sum(Pop)) %>% 
      ungroup() %>% 
      arrange(Age)
    
    hra_age_pop <- hra_pop %>% 
      filter(Race_Lbl == race) %>% 
      group_by(HRA, Age, Age_Lbl) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() %>% 
      arrange(Age) %>% 
      left_join(age_total_pop) %>% 
      group_by(HRA, Age, Age_Lbl) %>% 
      mutate(AgePrev = Pop/PopTotal)
    
    prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
    
    prop.int.hra <- classIntervals(hra_age_pop$AgePrev,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- prop.int.hra$brks
    breaks <- c(0, .005, .01,
                .02, .03, .04,
                .05, .065, .08, .1)
    
    for(age in unique(hra_age_pop$Age_Lbl)){
      
      hra_age_tmp <- hra_age_pop %>% 
        filter(Age_Lbl == age)
      hra_age_tmp <- hra_age_tmp[match(hra_age_tmp$HRA,
                                       hra@data$HRA2010v2_), ]
      
      prop.int.hra <- classIntervals(hra_age_tmp$AgePrev,
                                     style = "fixed",
                                     fixedBreaks = breaks,
                                     n = 9)
      prop.col.hra <- findColours(prop.int.hra, prop.pal)
      
      pdf(paste0("../PopPlots/", 
                 year, "/OFM_Ages/",
                 age, "/OFM_",
                 year, "_ageDist_", age,
                 "_", race, ".pdf"),
          height = 5, width = 5)
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = race.clean,
           adj = 0)
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.5,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      dev.off()
    }
  }  
}


# Household Size ####

if(!dir.exists("../household_size/Pyramid/")){
  dir.create("../household_size/Pyramid")
}

hh_size_hra <- hh_tract %>% 
  filter(Year >= 2010) %>% 
  left_join(tracts_to_hra$acs5_2019,
            by = c("GEOID" = "GEOID")) %>% 
  group_by(FID_HRA_20, HRA2010v2_, Year, hh_size, tenure) %>% 
  summarise(estimate = sum(estimate*prop.area, na.rm = TRUE),
            moe = sum(moe*prop.area, na.rm = TRUE)) %>% 
  mutate(SE = moe/qnorm(.95),
         CoV = SE/estimate)

pop.pyrs <- list()

for(year in c(2010, 2014, 2019)){
  
  pdf(paste0("../household_size/Pyramid/Pyramid_",
             year, "_hhsize_by_tenure_HRA.pdf"),
      height = 5, width = 5)
  for(hra.name in hra@data$HRA2010v2_){
    hh_size_kc <- hh_size_hra %>% 
      filter(Year == year) %>%
      filter(HRA2010v2_ == hra.name) %>% 
      filter(!is.na(hh_size) &
               hh_size > 0) %>% 
      group_by(hh_size, tenure) %>% 
      summarise(estimate = sum(estimate, na.rm = TRUE),
                SE = sum(SE, na.rm = TRUE)) %>% 
      mutate(CoV = SE/estimate)
    
    pop.pyr <- hh_size_kc %>% 
      filter(hh_size > 0) %>% 
      pivot_wider(id_cols = hh_size,
                  names_from = tenure,
                  values_from = estimate,
                  values_fill = 0) %>% 
      ungroup() %>% 
      dplyr::select(Owner, Renter) %>% 
      as.matrix()
    row.names(pop.pyr) <- c(1:6, "7+")
    
    pyr.obj <- get.bPop.pyramid(pop.pyr,
                                legend = paste0("ACS ", 
                                                year-4, 
                                                "-", year),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    
    par(lend = 1)
    plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                  border = pop.cols[3]),
         main = hra.name,
         adj = 0)
  }
  dev.off()
  
  pdf(paste0("../household_size/Pyramid/Pyramid_",
             year, "_hhsize_by_tenure.pdf"),
      height = 5, width = 5)
  hh_size_kc <- hh_size_hra %>% 
    filter(Year == year) %>%
    filter(!is.na(hh_size) &
             hh_size > 0) %>% 
    group_by(hh_size, tenure) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              SE = sum(SE, na.rm = TRUE)) %>% 
    mutate(CoV = SE/estimate)
  
  pop.pyrs[[paste0("year_",year)]] <- hh_size_kc %>% 
    filter(hh_size > 0) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  row.names(pop.pyrs[[paste0("year_", year)]]) <- c(1:6, "7+")
  
  pyr.obj <- get.bPop.pyramid(pop.pyrs[[paste0("year_", year)]],
                              legend = paste0("ACS ", 
                                              year-4, 
                                              "-", year),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  
  par(lend = 1)
  plot(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                border = pop.cols[3]))
  dev.off()
  if(year == 2019){
    pdf(paste0("../household_size/Pyramid/Pyramid_",
               "20102019_hhsize_by_tenure.pdf"),
        height = 5, width = 5)
    pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019, pop.pyrs$year_2010),
                                legend = c("ACS 2014-2019", "Census 2010"),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    
    par(lend = 1)
    plot(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                  border = pop.cols[5]),
         pyr2.par = list(col = pop.cols[3] , 
                         border = pop.cols[3]))
    dev.off()
    
    pdf(paste0("../household_size/Pyramid/Pyramid_",
               "20102019_hhsize_by_tenure_HRA.pdf"),
        height = 5, width = 5)
    for(hra.name in hra@data$HRA2010v2_){
      hh_size_kc <- hh_size_hra %>% 
        filter(HRA2010v2_ == hra.name) %>% 
        filter(!is.na(hh_size) &
                 hh_size > 0) %>% 
        group_by(Year, hh_size, tenure) %>% 
        summarise(estimate = sum(estimate, na.rm = TRUE),
                  SE = sum(SE, na.rm = TRUE)) %>% 
        mutate(CoV = SE/estimate)
      
      pyr.1 <-  hh_size_kc %>% 
        filter(Year == 2019) %>% 
        filter(hh_size > 0) %>% 
        pivot_wider(id_cols = hh_size,
                    names_from = tenure,
                    values_from = estimate,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Owner, Renter) %>% 
        as.matrix()
      pyr.2 <-  hh_size_kc %>% 
        filter(Year == 2010) %>% 
        filter(hh_size > 0) %>% 
        pivot_wider(id_cols = hh_size,
                    names_from = tenure,
                    values_from = estimate,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Owner, Renter) %>% 
        as.matrix()
      
      row.names(pyr.1) <-
        row.names(pyr.2) <- c(1:6, "7+")
      
      par(lend = 1)
      pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                                  legend = c("ACS 2014-2019", "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      
      par(lend = 1)
      plot(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                    border = pop.cols[5]),
           pyr2.par = list(col = pop.cols[3] , 
                           border = pop.cols[3]),
           main = hra.name)
    }
    dev.off()
  }
}
