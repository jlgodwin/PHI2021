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
library(readxl)

# Clear environment ####
rm(list = ls())

## tidycensus key ####
source('tidycensus_APIkey.R')
census_api_key(myKey) 

## tidycensus shapefile cache ####
options(tigris_use_cache = TRUE)

## load pyramid script ###
source('pyrPlot_JG_20210914.R')
## load WA OFM raw? ####
loadOFM <- FALSE

# Load data ####
## Household Size ####

hh_tract <- readRDS('../household_size/hh_by_hh_size_and_tenure_ct.RDS')

## WA OFM historic ###
# popold <- readxl::read_xlsx("../Data/ofm_april1_intercensal_estimates_county_1960-2010.xlsx",
#                    sheet = "Population",
#                    skip = 3) %>% 
#   group_by(County) %>% 
#   dplyr::select(contains("Census Count")) %>% 
#   pivot_longer(contains("Census Count"),
#                names_to = "Year",
#                names_prefix = "\r\nCensus Count of Total Population ",
#                values_to = "Population") %>% 
#   filter(County == "King") %>% 
#   filter(Year >= 1990)

popold <- readxl::read_xlsx("../Data/ofm_pop_sade_county_2000_to_2010.xlsx",
                            sheet = "Total") %>% 
  # filter(Year >= 2015) %>% 
  # filter(Year == 2020) %>% 
  filter(`Area Name` == "King") %>% 
  filter(`Age Group` != "Total") %>% 
  as.data.frame()


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
jurisdictions <- readOGR('../Data',
                         layer = "FLU_dissolve")
jurisdictions <- spTransform(jurisdictions,
                             kc_tracts_poly@proj4string)

juris <- unionSpatialPolygons(SpatialPolygons(parcels@polygons),
                              IDs = parcels@data$Jurisdicti)
juris_data <- jurisdictions@data %>%
  group_by(Jurisdicti) %>%
  summarise(Nobs = n(),
            Res_Use = sum(Res_Use == "Y",
                          na.rm = TRUE),
            Mixed_Use = sum(Mixed_Use == "Y",
                            na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Res_Prop = Res_Use/Nobs,
         Mixed_Prop = Mixed_Use/Nobs) %>%
  filter(!is.na(Jurisdicti)) %>%
  as.data.frame()
row.names(juris_data) <- juris_data$Jurisdicti
juris <- SpatialPolygonsDataFrame(juris,
                                  data = juris_data)
table(jurisdictions$Jurisdicti)
table(jurisdictions$Zone_adj)

## Parcels ####

### Shape ####
# parcels <- readOGR(dsn = '../Data/King_County_Parcels___parcel_area/',
#                    layer = 'King_County_Parcels___parcel_area')
# ### Data ####
# 
# parcels_data <- read.csv('../../../EXTR_Parcel.csv')
# parcels_data_dist <- unique(parcels_data$DistrictName)
# rm(parcels_data)


# Demographics ####
pop.pyrs<-list()

for(year in c(2010, 2012, 2015,
              2017, 2020)){
  
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
  
  
  ## County Population (OFM) pyramids by Year ####
  
  # pdf(paste0("../PopPlots/",
  #            year, "/Pyramid/Pyramid_",
  #            year, ".pdf"),
  #     height = 5, width = 5)
  
  jpeg(paste0("../PopPlots/",
              year, "/Pyramid/Population/Pyramid_",
              year, ".jpeg"),
       height = 480, width = 480)
  {
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
    x_at <- round(seq(0, max(pyr.tmp), length.out = 5),-4)
    pyr.obj <- get.bPop.pyramid(pyr.tmp,
                                legend = paste0("OFM, ", year),
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))                              
    
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, show.legend = TRUE,
                                 pyr1.par = list(col = pop.cols[3] , 
                                                 border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .76,
                                 cex.sub = .75)
    title(paste0("Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 paste0("King County")),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  
  # County Prevalence (OFM) pyramids by Year ####
  
  jpeg(paste0("../PopPlots/",
              year, "/Pyramid/Prevalence/Pyramid_Prevalence_",
              year, ".jpeg"),
       height = 480, width = 480)
  {
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
    # x_at <- c(-.075, -.05, -.025, -.01, 0, .01, .025, .05, .075)
    x_at <- seq(-.05,.05, .01)
    x_labels <- abs(x_at)
    pyr.obj <- get.bPop.pyramid(pyr.tmp/sum(pyr.tmp),
                                legend = paste0("OFM, ", year),
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))                              
    
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, show.legend = TRUE,
                                 pyr1.par = list(col = pop.cols[3] , 
                                                 border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = x_at,
                                 x_labels = x_labels,
                                 cex.axis = .75,
                                 cex.sub = .75,
                                 x_lims = c(-.065,.065))
    title(paste0("Prevalence of Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 paste0("King County")),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  
  # HRA Population (OFM) pyramids by Year ####
  # pdf(paste0("../PopPlots/",
  #            year, "/Pyramid/Pyramid_HRA_",
  #            year, ".pdf"),
  #     height = 5, width = 5)
  
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
    x_at <- round(seq(0, max(pyr.tmp), length.out = 5),-2)
    pyr.obj <- get.bPop.pyramid(pyr.tmp,
                                show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    hra.name.file <- gsub("\\.","",
                          hra.name)
    hra.name.file <- gsub("/","",
                          hra.name.file)
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Population/Pyramid_HRA_",
                hra.name.file, "_",
                year, ".jpeg"),
         height = 480, width = 480)
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[3] , 
                                                 border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .76,
                                 cex.sub = .75)
    title(paste0("Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
    # HRA Prevalence (OFM) pyramids by Year ####
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Prevalence/Pyramid_Prevalence_HRA_",
                hra.name.file, "_",
                year, ".jpeg"),
         height = 480, width = 480)
    # x_at <- c(-.3, -.2, -.1, -.05, 0, 0.05, .1, .2, .3)
    x_at <- seq(-.05,.05,.01)
    pyr.obj <- get.bPop.pyramid(pyr.tmp/sum(pyr.tmp),
                                show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[3] , 
                                                 border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = x_at,
                                 x_labels = abs(x_at),
                                 cex.axis = .65,
                                 cex.sub = .75,
                                 x_lims = c(-.065,.065))
    title(paste0("Prevalence by Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
  }
  
  ### HRA with County Population Pyramids by Year ####
  
  # pdf(paste0("../PopPlots/", year,
  #            "/Pyramid/Pyramid_HRAandCounty_",
  #            year, ".pdf"),
  #     height = 5, width = 5)
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
    x_at <- round(seq(0, max(county.pyr), length.out = 5),-4)
    pyr.obj <- get.bPop.pyramid(pyr.list, show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    # county.pyr.obj <- get.bPop.pyramid(county.pyr,
    #                                    LRcolnames = c("Female", "Male"),
    #                                    LRmain = c("Female", "Male"))
    hra.name.file <- gsub("\\.","",
                          hra.name)
    hra.name.file <- gsub("/","",
                          hra.name.file)
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Population/Pyramid_HRAandCounty_",
                hra.name.file, "_",
                year, ".jpeg"),
         height = 480, width = 480)
    
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[4] , 
                                                 border = pop.cols[4]),
                                 pyr2.par = list(col = pop.cols[2],
                                                 border = pop.cols[2]),
                                 legend_pos = "topright",
                                 legend_text = c("King County", "HRA"),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .75,
                                 cex.sub = .75)
    title(paste0("Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
    ### HRA with County Prevalence Pyramids by Year ####
    
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Prevalence/Pyramid_Prevalence_HRAandCounty_",
                hra.name.file, "_",
                year, ".jpeg"),
         height = 480, width = 480)  
    pyr.list <- list(county.pyr/sum(county.pyr),
                     pyr.tmp/sum(pyr.tmp))
    pyr.obj <- get.bPop.pyramid(pyr.list, show.legend = FALSE,
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    # x_at <- c(-.3, -.2, -.1, -.05, 0, .05, .1, .2, .3)
    x_at <- seq(-.05,.05,.01)
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[4] , 
                                                 border = pop.cols[4]),
                                 pyr2.par = list(col = pop.cols[2],
                                                 border = pop.cols[2]),
                                 legend_pos = "topright",
                                 legend_text = c("King County", "HRA"),
                                 x_at = x_at,
                                 x_labels = abs(x_at),
                                 cex.axis = .75,
                                 cex.sub = .75,
                                 x_lims = c(-.065,.065))
    title(paste0("Prevalence of Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
  
  
  
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
    
    if(!dir.exists(paste0("../PopPlots/", 
                          year, "/OFM_Ages/",
                          age, "/Population/"))){
      dir.create(paste0("../PopPlots/", 
                        year, "/OFM_Ages/",
                        age, "/Population/"))
      dir.create(paste0("../PopPlots/", 
                        year, "/OFM_Ages/",
                        age, "/Prevalence/"))
      dir.create(paste0("../PopPlots/", 
                        year, "/OFM_Ages/",
                        age, "/Distribution/"))
    }
    ### Population by Age and Year ####
    
    # pdf(paste0("../PopPlots/", 
    #            year, "/OFM_Ages/",
    #            age, "/OFM_",
    #            year, "_age", age,
    #            ".pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../PopPlots/", 
                year, "/OFM_Ages/",
                age, "/Population/OFM_",
                year, "_age", age,
                ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(0,0,1,0))
    plot(hra,
         col = pop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Population',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = pop.pal,
           legend = names(attr(pop.col.hra, 'table')))
    title(paste0("Population Ages ", age, "\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 paste0("King County, (WA OFM, ", year,")")),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
  
  
  ### Population Prevalence by Age and Year####
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
              .025, .05, .075,
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
    
    # pdf(paste0("../PopPlots/", 
    #            year, "/OFM_Ages/",
    #            age, "/OFM_",
    #            year, "_agePrev_", age,
    #            ".pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../PopPlots/", 
                year, "/OFM_Ages/",
                age, "/Prevalence/OFM_",
                year, "_agePrev_", age,
                ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(0,0,1,0))
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Prevalence',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    title(paste0("Prevalence of Population Ages ", age, "\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 paste0("King County, (WA OFM, ", year, ")")),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
  
  
  ### Population Distribution by Age and Year ####
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
    
    # pdf(paste0("../PopPlots/", 
    #            year, "/OFM_Ages/",
    #            age, "/OFM_",
    #            year, "_ageDist_", age,
    #            ".pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../PopPlots/", 
                year, "/OFM_Ages/",
                age, "/Distribution/OFM_",
                year, "_ageDist_", age,
                ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(0,0,1,0))
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Distribution',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    title(paste0("Distribution of Population Ages ", age, "\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 paste0("King County, (WA OFM, ", year, ")")),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
}

## County Population by pyramids 2010, 2020 ####
# pdf(paste0("../PopPlots/",
#            "Pyramid_20102020.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../PopPlots/Population/",
            "Pyramid_20102020.jpeg"),
     height = 480, width = 480)
x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-4)
pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020, pop.pyrs$year_2010),
                            legend = c("OFM, 2020", "OFM, 2010"),
                            LRcolnames = c("Female", "Male"),
                            LRmain = c("Female", "Male"))

pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                                      border = pop.cols[5]),
                             pyr2.par = list(col = pop.cols[3] , 
                                             border = pop.cols[3]),
                             legend_pos = "topright",
                             legend_text = c("OFM, 2020", "OFM, 2010"),
                             x_at = c(rev(-x_at[-1]), x_at),
                             x_labels = c(rev(x_at[-1]), x_at),
                             cex.axis = .8,
                             cex.sub = .75)
title(paste0("Population by Age and Sex\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

## County Population Prevalence by pyramids 2010, 2020 ####
jpeg(paste0("../PopPlots/Prevalence/",
            "Pyramid_Prevalence_20102020.jpeg"),
     height = 480, width = 480)
# x_at <- c(-.1, -.075, -.05, -.025, 0, .025, .05, .075, .1)
x_at <- seq(-.05, .05, .01)
pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020/sum(pop.pyrs$year_2020),
                                 pop.pyrs$year_2010/sum(pop.pyrs$year_2010)),
                            legend = c("OFM, 2020", "OFM, 2010"),
                            LRcolnames = c("Female", "Male"),
                            LRmain = c("Female", "Male"))

pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                                      border = pop.cols[5]),
                             pyr2.par = list(col = pop.cols[3] , 
                                             border = pop.cols[3]),
                             legend_pos = "topright",
                             legend_text = c("OFM, 2020", "OFM, 2010"),
                             x_at = x_at,
                             x_labels = abs(x_at),
                             cex.axis = .75,
                             cex.sub = .75,
                             x_lims = c(-.065, .065))
title(paste0("Prevalence of Population by Age and Sex\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()


## Compare OFM & ACS ####

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
{
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
}
dev.off()


pdf('../PopPlots/2020/OFM_ACS5_Compare_2020.pdf',
    height = 5, width = 5)
{
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
}
dev.off()


pdf('../PopPlots/2015/OFM_ACS5_Compare_2015.pdf',
    height = 5, width = 5)
{
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
}
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
{
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
}
dev.off()

pdf('../PopPlots/2010/OFM_ACS5_Compare_2010.pdf',
    height = 5, width = 5)
{
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
}
dev.off()

pdf('../PopPlots/2015/OFM_ACS5_Compare2_2015.pdf',
    height = 5, width = 5)
{
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
}
dev.off()


#### By Age ####

## NOT DONE YET!! ##
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

# Population Race Sex(OFM) pyramids by HRA ####

for(race in unique(pop$Race_Lbl)){
  race.clean <- gsub(" and ", "/",
                     race)
  race.clean <- gsub(" or ", "/",
                     race.clean)
  
  total.pyr <- list()
  for(year in c(2010, 2012, 2015,
                2017, 2020)){
    
    
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
    
    total.pyr[[paste0("year_", year)]] <- hra_pop %>% 
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
    pop.cols <- brewer.pal(n = 5,
                           name = 'Blues')
    
    
    
    ### County Population pyramid by Age, Race, and Year ####
    # pdf(paste0("../PopPlots/",
    #            year, "/Pyramid/Pyramid_",
    #            year, "_", race, ".pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Population/Pyramid_",
                year, "_", race, ".jpeg"),
         height = 480, width = 480)
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
    x_at <- round(seq(0, max(pyr.tmp), length.out = 5),-3)
    pyr.obj <- get.bPop.pyramid(pyr.tmp,
                                legend = paste0("OFM, ", year),
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .75,
                                 cex.sub = .75)
    title(paste0(race.clean,
                 " Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                "King County"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    ### Maps ####
    
    #### Race, Year only ####
    breaks <- c(0, 250, 500,
                750, 1000, 1500,
                2000, 3000, 4000,
                5750)

    hra_race_tmp <- hra_pop %>% 
      filter(Race_Lbl == race) %>% 
      group_by(HRA) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() 
      
      hra_total_tmp <-  hra_pop %>% 
        group_by(HRA) %>% 
        summarise(Pop = sum(Pop)) %>% 
        ungroup() 
        
    hra_race_tmp <- hra_race_tmp[match(hra_race_tmp$HRA,
                                     hra@data$HRA2010v2_), ]
    hra_total_tmp <- hra_total_tmp[match(hra_total_tmp$HRA,
                                         hra@data$HRA2010v2_), ]
      
    pop.int.hra <- classIntervals(hra_age_tmp$Pop,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    
    ### County Population Prevalence pyramid by Age, Race, and Year ####
    jpeg(paste0("../PopPlots/",
                year, "/Pyramid/Prevalence/Pyramid_Prevalence_",
                year, "_", race, ".jpeg"),
         height = 480, width = 480)
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
    x_at <- seq(-.03, .03, .005)
    pyr.obj <- get.bPop.pyramid(pyr.tmp/sum(total.pyr[[paste0("year_", year)]]),
                                legend = paste0("OFM, ", year),
                                LRcolnames = c("Female", "Male"),
                                LRmain = c("Female", "Male"))
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("OFM, ", year),
                                 x_at = x_at,
                                 x_labels = abs(x_at),
                                 cex.axis = .75,
                                 cex.sub = .75,
                                 x_lims = c(-.035, .035))
    title(paste0("Prevalence of Population by Age and Sex\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County, ", race.clean),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
  }
  
  ### County Population Pyramid by Race, and Year ####
  # pdf(paste0("../PopPlots/",
  #            "Pyramid_20102020_",
  #            race, ".pdf"),
  #     height = 5, width = 5)
  jpeg(paste0("../PopPlots/Population/",
              "Pyramid_20102020_",
              race, ".jpeg"),
       height = 480, width = 480)
  x_at <- round(seq(0, max(pyr.tmp), length.out = 5),-3)
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020,
                                   pop.pyrs$year_2010),
                              legend = c("OFM, 2020", "OFM, 2010"),
                              LRcolnames = c("Female", "Male"),
                              LRmain = c("Female", "Male"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                                        border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[3] , 
                                               border = pop.cols[3]),
                               legend_pos = "topright",
                               legend_text = c("OFM, 2020", "OFM, 2010"),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .75,
                               cex.sub = .75)
  title(paste0("Population by Age and Sex\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               paste0("King County, ", race.clean)),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
  dev.off()
  ### County Population Prevalence Pyramid by Age, Race, and Year ####
  jpeg(paste0("../PopPlots/Prevalence/",
              "Pyramid_Prevalence_20102020_",
              race, ".jpeg"),
       height = 480, width = 480)
  # x_at <- c(-.1, -.075, -.05, -.025, 0, .025, .05, .075, .1)
  x_at <- seq(-.03, .03, .005)
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2020/sum(total.pyr$year_2020),
                                   pop.pyrs$year_2010/sum(total.pyr$year_2010)),
                              legend = c("OFM, 2020", "OFM, 2010"),
                              LRcolnames = c("Female", "Male"),
                              LRmain = c("Female", "Male"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[5] , 
                                                        border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[3] , 
                                               border = pop.cols[3]),
                               legend_pos = "topright",
                               legend_text = c("OFM, 2020",
                                               "OFM, 2010"),
                               x_at = x_at,
                               x_labels = abs(x_at),
                               cex.axis = .75,
                               cex.sub = .75,
                               x_lims = c(-.035,.035))
  title(paste0("Prevalence of Population by Age and Sex\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               "King County, ", race.clean),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
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
    
    
    
    # pdf(paste0("../PopPlots/",
    #            year, "/Pyramid/Pyramid_HRA_",
    #            year, "_", race, ".pdf"),
    #     height = 5, width = 5)
    for(hra.name in unique(hra_pop$HRA)){
      pyr.tmp <- hra_pop %>% 
        filter(HRA == hra.name) %>% 
        filter(Race_Lbl == race) %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = Pop,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      total.tmp <- hra_pop %>% 
        filter(HRA == hra.name) %>% 
        group_by(Age, Age_Lbl, Sex_Lbl) %>% 
        summarise(Pop = sum(Pop, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = Pop,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      
      row.names(pyr.tmp) <- hra_pop %>% 
        arrange(Age, Age_Lbl) %>% 
        group_by(Age) %>% 
        summarise(Age_Lbl = unique(Age_Lbl)) %>% 
        ungroup() %>% 
        dplyr::select(Age_Lbl) %>% unlist()
      
      x_at <- round(seq(0, max(pyr.tmp), length.out = 5),-2)
      pyr.obj <- get.bPop.pyramid(pyr.tmp,
                                  legend = paste0("OFM, ", year),
                                  LRcolnames = c("Female", "Male"),
                                  LRmain = c("Female", "Male"))
      hra.name.file <- gsub("\\.","",
                            hra.name)
      hra.name.file <- gsub("/","",
                            hra.name.file)
      ### HRA Population Pyramid by Age, Race, and Year ####
      jpeg(paste0("../PopPlots/",
                  year, "/Pyramid/Population/Pyramid_HRA_",
                  hra.name.file, "_",
                  year, "_", race, ".jpeg"),
           height = 480, width = 480)
      pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                            border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = paste0("OFM, ", year),
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .75,
                                   cex.sub = .75)
      title(paste0("Population by Age and Sex\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   paste0(hra.name, ", ", race.clean)),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
      
      ### HRA Population Prevalence Pyramid by Age, Race, and Year ####
      jpeg(paste0("../PopPlots/",
                  year, "/Pyramid/Prevalence/Pyramid_Prevalence_HRA_",
                  hra.name.file, "_",
                  year, "_", race, ".jpeg"),
           height = 480, width = 480)
      # x_at <- c(-.1, -.075, -.05, -.025, 0, .025, .05, .075, .1)
      x_at <- seq(-.05, .05, .01)
      pyr.obj <- get.bPop.pyramid(pyr.tmp/sum(total.tmp),
                                  legend = paste0("OFM, ", year),
                                  LRcolnames = c("Female", "Male"),
                                  LRmain = c("Female", "Male"))
      pop.pyramid.bayesPop.pyramid(pyr.obj, 
                                   pyr1.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = paste0("OFM, ", year),
                                   x_at = x_at,
                                   x_labels = abs(x_at),
                                   cex.axis = .75,
                                   cex.sub = .75,
                                   x_lims = c(-.065,.065))
      title(paste0("Prevalence of Population by Age and Sex\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   hra.name, ", ", race.clean),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
    
    
    ## With County ####
    
    
    # pdf(paste0("../PopPlots/", year,
    #            "/Pyramid/Pyramid_HRAandCounty_",
    #            year, "_", race ,".pdf"),
    #     height = 5, width = 5)
    for(hra.name in unique(hra_pop$HRA)){
      pyr.tmp <- hra_pop %>%
        filter(HRA == hra.name) %>% 
        filter(Race_Lbl == race) %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = Pop,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      hra.total.tmp <- hra_pop %>%
        filter(HRA == hra.name) %>% 
        group_by(Age, Age_Lbl, Sex_Lbl) %>% 
        summarise(Pop = sum(Pop, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = Pop,
                    values_fill = 0) %>% 
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
                    values_from = County_Sum,
                    values_fill = 0) %>% 
        ungroup() %>% 
        dplyr::select(Female, Male) %>% 
        as.matrix()
      county.total.pyr <- hra_pop %>% 
        group_by(Age, Age_Lbl, Sex_Lbl) %>% 
        summarise(County_Sum = sum(Pop)) %>% 
        arrange(Age, Sex_Lbl) %>% 
        pivot_wider(id_cols = c(Age, Age_Lbl),
                    names_from = Sex_Lbl,
                    values_from = County_Sum,
                    values_fill = 0) %>% 
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
      x_at <- round(seq(0, max(county.pyr), length.out = 5),-2)
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
      ### HRA + County Population Pyramid by Age, Race, and Year ####
      hra.name.file <- gsub("\\.","",
                            hra.name)
      hra.name.file <- gsub("/","",
                            hra.name.file)
      jpeg(paste0("../PopPlots/", year,
                  "/Pyramid/Population/Pyramid_HRAandCounty_",
                  hra.name.file, "_", year, "_", race ,".jpeg"),
           height = 480, width = 480)
      pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[4] , 
                                                            border = pop.cols[4]),
                                   pyr2.par = list(col = pop.cols[2],
                                                   border = pop.cols[2]),
                                   legend_pos = "topright",
                                   legend_text = c("King County", "HRA"),
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .75,
                                   cex.sub = .75)
      title(paste0(race.clean, 
                   " Population by Age and Sex\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   paste0(hra.name,
                          " and King County, (WA OFM, ",
                          year, ")")),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off() 
      
      ### HRA + County Population Prevalence Pyramid by Age, Race, and Year ####
      hra.name.file <- gsub("\\.","",
                            hra.name)
      hra.name.file <- gsub("/","",
                            hra.name.file)
      jpeg(paste0("../PopPlots/", year,
                  "/Pyramid/Prevalence/Pyramid_HRAandCounty_",
                  hra.name.file, "_", year, "_", race ,".jpeg"),
           height = 480, width = 480)
      pyr.list <- list(county.pyr/sum(county.total.pyr),
                       pyr.tmp/sum(hra.total.tmp))
      # x_at <- c(-.3, -.2, -.1, -.05, 0, .05, .1, .2 , .3)
      x_at <- seq(-0.05, 0.05, .01)
      pyr.obj <- get.bPop.pyramid(pyr.list, 
                                  legend = c("King County", "HRA"),
                                  LRcolnames = c("Female", "Male"),
                                  LRmain = c("Female", "Male"))
      
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[4] , 
                                                   border = pop.cols[4]),
                                   pyr2.par = list(col = pop.cols[2],
                                                   border = pop.cols[2]),
                                   legend_pos = "topright",
                                   legend_text = c("King County", "HRA"),
                                   x_at = x_at,
                                   x_labels = abs(x_at),
                                   cex.axis = .75,
                                   cex.sub = .75,
                                   x_lims = c(-0.065,0.065))
      title(paste0("Prevalence of Population by Age and Sex\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   paste0(hra.name,
                          ", ", race.clean, " (WA OFM, ",
                          year, ")")),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off() 
    }
    
    
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
                2000, 3000,
                5000, 7250)
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
      
      ## Maps ####
      ### HRA Population by Age, Race, and Year ####
      # pdf(paste0("../PopPlots/", 
      #            year, "/OFM_Ages/",
      #            age, "/OFM_",
      #            year, "_age", age,
      #            "_", race, ".pdf"),
      #     height = 5, width = 5)
      jpeg(paste0("../PopPlots/", 
                  year, "/OFM_Ages/",
                  age, "/Population/OFM_",
                  year, "_age", age,
                  "_", race, ".jpeg"),
           height = 480, width = 480)
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = pop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Population',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.hra, 'table')))
      title(paste0("Population Ages ", age, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "King County, ",
                   race.clean, " (WA OFM, ", year, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
    
    
    #### Race, Year only ####
    breaks <- c(0, 250, 500,
                1000, 
                2500, 5000, 7500, 10000,
                25000, 62500)
    hra_tmp <- hra_age_pop %>% 
      group_by(HRA) %>% 
      summarise(Pop = sum(Pop, na.rm = TRUE))
    hra_tmp <- hra_tmp[match(hra_tmp$HRA,
                             hra@data$HRA2010v2_), ]
    
    hra_total_tmp <- hra_pop %>% 
      group_by(HRA) %>% 
      summarise(Pop = sum(Pop, na.rm = TRUE))
    hra_total_tmp <- hra_total_tmp[match(hra_total_tmp$HRA,
                             hra@data$HRA2010v2_), ]
    
    pop.int.hra <- classIntervals(hra_tmp$Pop,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    ## Maps ####
       jpeg(paste0("../PopPlots/", 
                year, "/Population/OFM_",
                year, 
                "_", race, ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(1,1,1,1))
    plot(hra,
         col = pop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Population',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = pop.pal,
           legend = names(attr(pop.col.hra, 'table')))
    title(paste0("Total Population\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County, ",
                 race.clean, " (WA OFM, ", year, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
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
      ### County Population Prevalence by Age, Race, and Year ####
      # pdf(paste0("../PopPlots/", 
      #            year, "/OFM_Ages/",
      #            age, "/OFM_",
      #            year, "_agePrev_", age,
      #            "_", race, ".pdf"),
      #     height = 5, width = 5)
      jpeg(paste0("../PopPlots/", 
                  year, "/OFM_Ages/",
                  age, "/Prevalence/OFM_",
                  year, "_agePrev_", age,
                  "_", race, ".jpeg"),
           height = 480, width = 480)
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "",
           adj = 0)
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Prevalence of Population Ages ", age, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "King County, ", 
                   race.clean, " (WA OFM ", year, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
    
    hra_tmp <- hra_pop %>% 
      filter(Race_Lbl == race) %>%
      group_by(HRA) %>% 
      summarise(Pop = sum(Pop)) %>% 
      left_join(hra_total_pop) %>% 
      group_by(HRA) %>% 
      mutate(RacePrev = Pop/PopTotal) %>% 
      ungroup()
    
    prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
    
    prop.int.hra <- classIntervals(hra_tmp$RacePrev,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- prop.int.hra$brks
    breaks <- c(0, .005, .01,
                .025, .05, .1,
                .25, .5, .75, .85)
    prop.int.hra <- classIntervals(hra_tmp$RacePrev,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    #### Race, Year only ####
    ## Maps ####
    jpeg(paste0("../PopPlots/", 
                year, "/Prevalence/OFM_",
                year, 
                "_", race, ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(1,1,1,1))
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Prevalence',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    title(paste0("Population Prevalence\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County, ",
                 race.clean, " (WA OFM, ", year, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
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
      ### County Population Distribution by Age, Race, and Year ####
      # pdf(paste0("../PopPlots/", 
      #            year, "/OFM_Ages/",
      #            age, "/OFM_",
      #            year, "_ageDist_", age,
      #            "_", race, ".pdf"),
      #     height = 5, width = 5)
      jpeg(paste0("../PopPlots/", 
                  year, "/OFM_Ages/",
                  age, "/Distribution/OFM_",
                  year, "_ageDist_", age,
                  "_", race, ".jpeg"),
           height = 480, width = 480)
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "",
           adj = 0)
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex =  0.75,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Distribution of ", race.clean,
                   " Population Ages ", age, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "King County, (WA OFM, ", year, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
      
    hra_tmp <- hra_pop %>% 
      filter(Race_Lbl == race) %>%
      group_by(HRA) %>% 
      summarise(Pop = sum(Pop)) %>% 
      mutate(Dist = Pop/sum(Pop))
    
    prop.pal <- brewer.pal(n = 9, name = "YlGnBu")
    
    prop.int.hra <- classIntervals(hra_tmp$Dist,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- prop.int.hra$brks
    breaks <- c(0, .005, .01,
                .02, .03, .04,
                .05, .065, .085, .10)
    prop.int.hra <- classIntervals(hra_tmp$Dist,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    ## Maps ####
    jpeg(paste0("../PopPlots/", 
                year, "/Distribution/OFM_",
                year, 
                "_", race, ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(1,1,1,1))
    plot(hra,
         col = prop.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Distribution',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex = 0.75,
           border = FALSE,
           fill = prop.pal,
           legend = names(attr(prop.col.hra, 'table')))
    title(paste0("Population Distribution\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County, ",
                 race.clean, " (WA OFM, ", year, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
  }          
}


# Household Size ####

if(!dir.exists("../household_size/Pyramid/")){
  dir.create("../household_size/Pyramid")
}

if(!dir.exists("../household_size/HRA/")){
  dir.create("../household_size/HRA/")
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

hh_size_hra_2000 <- hh_tract %>% 
  filter(Year < 2010) %>% 
  left_join(tracts_to_hra$acs5_2009,
            by = c("GEOID" = "GEOID")) %>% 
  group_by(FID_HRA_20, HRA2010v2_, Year, hh_size, tenure) %>% 
  summarise(estimate = sum(estimate*prop.area, na.rm = TRUE),
            moe = sum(moe*prop.area, na.rm = TRUE)) %>% 
  mutate(SE = moe/qnorm(.95),
         CoV = SE/estimate)


pop.pyrs <- list()

## Pyramids ####
for(year in c(2010, 2014, 2019)){
  # pdf(paste0("../household_size/Pyramid/Pyramid_",
  #            year, "_hhsize_by_tenure_HRA.pdf"),
  #     height = 5, width = 5)
  
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
    
    
    # x_at <- round(seq(0, max(pop.pyr), length.out = 5),-2)
    x_at <- c(0, 1000, 5000, 10000, 15000)
    pyr.obj <- get.bPop.pyramid(pop.pyr,
                                legend = paste0("ACS ", 
                                                year-4, 
                                                "-", year),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    hra.name.file <- gsub("\\.","",
                          hra.name)
    hra.name.file <- gsub("/","",
                          hra.name.file)
    jpeg(paste0("../household_size/Pyramid/Pyramid_",
                year, "_hhsize_by_tenure_HRA_",
                hra.name.file, ".jpeg"),
         height = 480, width = 480)
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("ACS ", 
                                                      year-4, 
                                                      "-", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .75,
                                 cex.sub = .75,
                                 x_lims = c(-17000,17000))
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
  
  
  
  # pdf(paste0("../household_size/Pyramid/Pyramid_",
  #            year, "_hhsize_by_tenure.pdf"),
  #     height = 5, width = 5)
  jpeg(paste0("../household_size/Pyramid/Pyramid_",
              year, "_hhsize_by_tenure.jpeg"),
       height = 480, width = 480)
  {
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
    x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-2)
    pyr.obj <- get.bPop.pyramid(pop.pyrs[[paste0("year_", year)]],
                                legend = paste0("ACS ", 
                                                year-4, 
                                                "-", year),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("ACS ", 
                                                      year-4, 
                                                      "-", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .75,
                                 cex.sub = .75)
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  ## Compare 2010, 2015-2019 ####
  if(year == 2019){
    # pdf(paste0("../household_size/Pyramid/Pyramid_",
    #            "20102019_hhsize_by_tenure.pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../household_size/Pyramid/Pyramid_",
                "20102019_hhsize_by_tenure.jpeg"),
         height = 480, width = 480)
    {
      x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-4)
      pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019, pop.pyrs$year_2010),
                                  legend = c("ACS 2015-2019", "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[5] ,
                                                   border = pop.cols[5]),
                                   pyr2.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = c("ACS 2015-2019",
                                                   "Census 2010"),
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .65,
                                   cex.sub = .75)
      title(paste0("Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      title(paste0("\n",
                   "King County"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    # pdf(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
    #            "20102019_hhsize_by_tenure.pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
                "20102019_hhsize_by_tenure.jpeg"),
         height = 480, width = 480)
    {
      x_at <- c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2)
      x_labels <- abs(x_at)
      pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019/sum(pop.pyrs$year_2019),
                                       pop.pyrs$year_2010/sum(pop.pyrs$year_2010)),
                                  legend = c("ACS 2015-2019",
                                             "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[5] ,
                                                   border = pop.cols[5]),
                                   pyr2.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = c("ACS 2015-2019",
                                                   "Census 2010"),
                                   x_at = x_at,
                                   x_labels = x_labels,
                                   cex.axis = .65,
                                   cex.sub = .75,
                                   x_lims = c(-.25, .25))
      title(paste0("Prevalence of Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "King County"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      
    }
    dev.off()
    
    # pdf(paste0("../household_size/Pyramid/Pyramid_",
    #            "20102019_hhsize_by_tenure_HRA.pdf"),
    #     height = 5, width = 5)
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
      
      
      # x_at <- round(seq(0, max(pop.pyr), length.out = 5),-2)
      x_at <- c(0, 1000, 2500, 5000, 7500)
      pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                                  legend = c("ACS 2015-2019", "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      jpeg(paste0("../household_size/Pyramid/Pyramid_",
                  "20102019_hhsize_by_tenure_HRA_",
                  hra.name, ".jpeg"),
           height = 480, width = 480)
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[5] ,
                                                   border = pop.cols[5]),
                                   pyr2.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = c("ACS 2015-2019",
                                                   "Census 2010"),
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .65,
                                   cex.sub = .75,
                                   x_lims = c(-8750, 8750))
      title(paste0("Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   hra.name),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
    
    
    # pdf(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
    #            "20102019_hhsize_by_tenure_HRA.pdf"),
    #     height = 5, width = 5)
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
      pyr.1 <- pyr.1/sum(pyr.1)
      pyr.2 <- pyr.2/sum(pyr.2)
      
      row.names(pyr.1) <-
        row.names(pyr.2) <- c(1:6, "7+")
      
      
      x_at <- c(-.3, -.2, -.1, -.05, 0, .05, .1, .2, .3)
      x_labels <- abs(x_at)
      pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                                  legend = c("ACS 2015-2019", "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      hra.name.file <- gsub("\\.","",
                            hra.name)
      hra.name.file <- gsub("/","",
                            hra.name.file)
      jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
                  "20102019_hhsize_by_tenure_HRA_",
                  hra.name.file, ".jpeg"),
           height = 480, width = 480)
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[5] ,
                                                   border = pop.cols[5]),
                                   pyr2.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = c("ACS 2015-2019",
                                                   "Census 2010"),
                                   x_at = x_at,
                                   x_labels = x_labels,
                                   cex.axis = .75,
                                   cex.sub = .75,
                                   x_lims = c(-.3, .3))
      title(paste0("Prevalence of Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   hra.name),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      dev.off()
    }
    
    
    
  }
  
  
}

## Pyramids: pre 2010 ####
for(year in c(2000, 2009)){
  # pdf(paste0("../household_size/Pyramid/Pyramid_",
  #            year, "_hhsize_by_tenure_HRA.pdf"),
  #     height = 5, width = 5)
  for(hra.name in hra@data$HRA2010v2_){
    hh_size_kc <- hh_size_hra_2000 %>% 
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
    
    
    x_at <- round(seq(0, max(pop.pyr), length.out = 5),-2)
    pyr.obj <- get.bPop.pyramid(pop.pyr,
                                legend = paste0("ACS ", 
                                                year-4, 
                                                "-", year),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    hra.name.file <- gsub("\\.","",
                          hra.name)
    hra.name.file <- gsub("/","",
                          hra.name.file)
    jpeg(paste0("../household_size/Pyramid/Pyramid_",
                year, "_hhsize_by_tenure_HRA_",
                hra.name.file, ".jpeg"),
         height = 480, width = 480)
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("ACS ", 
                                                      year-4, 
                                                      "-", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .76,
                                 cex.sub = .75)
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
  }
  
  
  # pdf(paste0("../household_size/Pyramid/Pyramid_",
  #            year, "_hhsize_by_tenure.pdf"),
  #     height = 5, width = 5)
  jpeg(paste0("../household_size/Pyramid/Pyramid_",
              year, "_hhsize_by_tenure.jpeg"),
       height = 480, width = 480)
  hh_size_kc <- hh_size_hra_2000 %>% 
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
  x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-2)
  pyr.obj <- get.bPop.pyramid(pop.pyrs[[paste0("year_", year)]],
                              legend = paste0("ACS ", 
                                              year-4, 
                                              "-", year),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                        border = pop.cols[3]),
                               legend_pos = "topright",
                               legend_text = paste0("ACS ", 
                                                    year-4, 
                                                    "-", year),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .76,
                               cex.sub = .75)
  title(paste0("Households by Size and Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               hra.name),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
  dev.off()
  
}

## Compare 2000, 2015-2019 ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002019_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_",
            "20002019_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
x_at <- round(seq(0, max(unlist(pop.pyrs)),
                  length.out = 5), -4)
pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019,
                                 pop.pyrs$year_2000),
                            legend = c("ACS 2015-2019",
                                       "Census 2000"),
                            LRcolnames = c("Owner", "Renter"),
                            LRmain = c("Owner", "Renter"))

pop.pyramid.bayesPop.pyramid(pyr.obj,
                             pyr1.par = list(col = pop.cols[5] ,
                                             border = pop.cols[5]),
                             pyr2.par = list(col = pop.cols[3] , 
                                             border = pop.cols[3]),
                             legend_pos = "topright",
                             legend_text = c("ACS 2015-2019",
                                             "Census 2000"),
                             x_at = c(rev(-x_at[-1]), x_at),
                             x_labels = c(rev(x_at[-1]), x_at),
                             cex.axis = .65,
                             cex.sub = .75)
title(paste0("Households by Size and Tenure\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002019_hhsize_by_tenure_HRA.pdf"),
#     height = 5, width = 5)
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
  pyr.2 <-  hh_size_hra_2000 %>% 
    filter(HRA2010v2_ == hra.name) %>% 
    filter(!is.na(hh_size) &
             hh_size > 0) %>% 
    group_by(Year, hh_size, tenure) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              SE = sum(SE, na.rm = TRUE)) %>% 
    mutate(CoV = SE/estimate) %>% 
    filter(Year == 2000) %>% 
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
  
  
  x_at <- c(0, 1000, 2500, 5000, 7500)
  pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                              legend = c("ACS 2015-2019",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  hra.name.file <- gsub("\\.","",
                        hra.name)
  hra.name.file <- gsub("/","",
                        hra.name.file)
  jpeg(paste0("../household_size/Pyramid/Pyramid_",
              "20002019_hhsize_by_tenure_HRA_",
              hra.name.file, ".jpeg"),
       height = 480, width = 480)
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[5] ,
                                               border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[3] , 
                                               border = pop.cols[3]),
                               legend_pos = "topright",
                               legend_text = c("ACS 2015-2019",
                                               "Census 2000"),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .6,
                               cex.sub = .75,
                               x_lims = c(-8750,8750))
  title(paste0("Households by Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  title(paste0("\n",
               hra.name),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = .8)
  dev.off()
}


# pdf(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
#            "20002019_hhsize_by_tenure_HRA.pdf"),
#     height = 5, width = 5)
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
    # left_join(hh_size_total,
    #           by = c("Year" = "Year",
    #                  "hh_size" = "hh_size",
    #                  "tenure" = "tenure"),
    #           suffix = c("", "_Total")) %>% 
    # mutate(proportion = estimate/sum(estimate)) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  pyr.2 <-  hh_size_hra_2000 %>% 
    filter(HRA2010v2_ == hra.name) %>% 
    filter(!is.na(hh_size) &
             hh_size > 0) %>% 
    group_by(Year, hh_size, tenure) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              SE = sum(SE, na.rm = TRUE)) %>% 
    mutate(CoV = SE/estimate) %>% 
    filter(Year == 2000) %>% 
    filter(hh_size > 0) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  pyr.1 <- pyr.1/sum(pyr.1)
  pyr.2 <- pyr.2/sum(pyr.2)
  
  row.names(pyr.1) <-
    row.names(pyr.2) <- c(1:6, "7+")
  
  
  x_at <- c(-.3, -.2, -.1, -.05, 0, .05, .1, .2, .3)
  x_labels <- abs(x_at)
  pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                              legend = c("ACS 2015-2019",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  hra.name.file <- gsub("\\.","",
                        hra.name)
  hra.name.file <- gsub("/","",
                        hra.name.file)
  jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
              "20002019_hhsize_by_tenure_HRA_",
              hra.name.file, ".pdf"),
       height = 480, width = 480)
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[5] ,
                                               border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[2] , 
                                               border = pop.cols[2]),
                               legend_pos = "topright",
                               legend_text = c("ACS 2015-2019",
                                               "Census 2000"),
                               x_at = x_at,
                               x_labels = abs(x_at),
                               cex.axis = .65,
                               cex.sub = .75,
                               x_lims = c(-.35,.35))
  title(paste0("Prevalence of Households by Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  title(paste0("\n", hra.name),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = .8)
  dev.off()
}





## Compare 2000, 2010 ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002010_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_",
            "20002010_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
x_at <- round(seq(0, max(unlist(pop.pyrs)),
                  length.out = 5), -4)
pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2010,
                                 pop.pyrs$year_2000),
                            legend = c("Census 2010",
                                       "Census 2000"),
                            LRcolnames = c("Owner", "Renter"),
                            LRmain = c("Owner", "Renter"))

pop.pyramid.bayesPop.pyramid(pyr.obj,
                             pyr1.par = list(col = pop.cols[4] ,
                                             border = pop.cols[4]),
                             pyr2.par = list(col = pop.cols[2] , 
                                             border = pop.cols[2]),
                             legend_pos = "topright",
                             legend_text = c("Census 2010",
                                             "Census 2000"),
                             x_at = c(rev(-x_at[-1]), x_at),
                             x_labels = c(rev(x_at[-1]), x_at),
                             cex.axis = .65,
                             cex.sub = .75)
dev.off()

# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002010_hhsize_by_tenure_HRA.pdf"),
#     height = 5, width = 5)
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
    filter(Year == 2010) %>% 
    filter(hh_size > 0) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  pyr.2 <-  hh_size_hra_2000 %>% 
    filter(HRA2010v2_ == hra.name) %>% 
    filter(!is.na(hh_size) &
             hh_size > 0) %>% 
    group_by(Year, hh_size, tenure) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              SE = sum(SE, na.rm = TRUE)) %>% 
    mutate(CoV = SE/estimate) %>% 
    filter(Year == 2000) %>% 
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
  
  
  x_at <- c(0, 1000, 2500, 5000, 7500)
  pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                              legend = c("Census 2010",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  hra.name.file <- gsub("\\.","",
                        hra.name)
  hra.name.file <- gsub("/","",
                        hra.name.file)
  jpeg(paste0("../household_size/Pyramid/Pyramid_",
              "20002010_hhsize_by_tenure_HRA_",
              hra.name.file, ".pdf"),
       height = 480, width = 480)
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[4] ,
                                               border = pop.cols[4]),
                               pyr2.par = list(col = pop.cols[2] , 
                                               border = pop.cols[2]),
                               legend_pos = "topright",
                               legend_text = c("Census 2010",
                                               "Census 2000"),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .75,
                               cex.sub = .75,
                               x_lims = c(-8750, 8750))
  title(paste0("Households by Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  title(paste0("\n", hra.name),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = .8)
  dev.off()
}


hh_size_total <- hh_size_hra %>% 
  filter(!is.na(hh_size) &
           hh_size > 0) %>% 
  group_by(Year, hh_size, tenure) %>% 
  summarise(estimate = sum(estimate, na.rm = TRUE),
            SE = sum(SE, na.rm = TRUE)) %>% 
  mutate(CoV = SE/estimate)

hh_size_total_2000 <- hh_size_hra_2000 %>% 
  filter(!is.na(hh_size) &
           hh_size > 0) %>% 
  group_by(Year, hh_size, tenure) %>% 
  summarise(estimate = sum(estimate, na.rm = TRUE),
            SE = sum(SE, na.rm = TRUE)) %>% 
  mutate(CoV = SE/estimate)

# pdf(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
#            "20002010_hhsize_by_tenure_HRA.pdf"),
#     height = 5, width = 5)
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
    filter(Year == 2010) %>% 
    filter(hh_size > 0) %>%
    # left_join(hh_size_total,
    #           by = c("Year" = "Year",
    #                  "hh_size" = "hh_size",
    #                  "tenure" = "tenure"),
    #           suffix = c("", "_Total")) %>% 
    # mutate(proportion = estimate/sum(estimate)) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  pyr.2 <-  hh_size_hra_2000 %>% 
    filter(HRA2010v2_ == hra.name) %>% 
    filter(!is.na(hh_size) &
             hh_size > 0) %>% 
    group_by(Year, hh_size, tenure) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              SE = sum(SE, na.rm = TRUE)) %>% 
    mutate(CoV = SE/estimate) %>% 
    filter(Year == 2000) %>% 
    filter(hh_size > 0) %>% 
    pivot_wider(id_cols = hh_size,
                names_from = tenure,
                values_from = estimate,
                values_fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(Owner, Renter) %>% 
    as.matrix()
  pyr.1 <- pyr.1/sum(pyr.1)
  pyr.2 <- pyr.2/sum(pyr.2)
  
  row.names(pyr.1) <-
    row.names(pyr.2) <- c(1:6, "7+")
  
  
  x_at <- c(-.3, -.2, -.1, -.05, 0, .05, .1, .2, .3)
  x_labels <- abs(x_at)
  pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                              legend = c("Census 2010",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  hra.name.file <- gsub("\\.","",
                        hra.name)
  hra.name.file <- gsub("/","",
                        hra.name.file)
  jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence_",
              "20002010_hhsize_by_tenure_HRA_",
              hra.name.file, ".jpeg"),
       height = 480, width = 480)
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[4] ,
                                               border = pop.cols[4]),
                               pyr2.par = list(col = pop.cols[2] , 
                                               border = pop.cols[2]),
                               legend_pos = "topright",
                               legend_text = c("Census 2010",
                                               "Census 2000"),
                               x_at = x_at,
                               x_labels = abs(x_at),
                               cex.axis = .65,
                               cex.sub = .75,
                               x_lims = c(-.35,.35))
  title(paste0("Prevalence of Households by Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  title(paste0("\n", hra.name),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = .8)
  dev.off()
}






## Maps ####
for(year in c(2010, 2014, 2019)){
  pdf(paste0("../household_size/Pyramid/Pyramid_",
             year, "_hhsize_by_tenure_HRA.pdf"),
      height = 5, width = 5)
  for(hra.name in hra@data$HRA2010v2_){
    hh_size_kc <- hh_size_hra_2000 %>% 
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
    
    
    x_at <- round(seq(0, max(pop.pyr), length.out = 5),-2)
    pyr.obj <- get.bPop.pyramid(pop.pyr,
                                legend = paste0("ACS ", 
                                                year-4, 
                                                "-", year),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = paste0("ACS ", 
                                                      year-4, 
                                                      "-", year),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .76,
                                 cex.sub = .75)
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  
  pdf(paste0("../household_size/Pyramid/Pyramid_",
             year, "_hhsize_by_tenure.pdf"),
      height = 5, width = 5)
  hh_size_kc <- hh_size_hra_2000 %>% 
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
  x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-2)
  pyr.obj <- get.bPop.pyramid(pop.pyrs[[paste0("year_", year)]],
                              legend = paste0("ACS ", 
                                              year-4, 
                                              "-", year),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                        border = pop.cols[3]),
                               legend_pos = "topright",
                               legend_text = paste0("ACS ", 
                                                    year-4, 
                                                    "-", year),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .76,
                               cex.sub = .75)
  title(paste0("Households by Size and Tenure,", 
               year, "\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               "King County"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
  dev.off()
  if(year == 2019){
    pdf(paste0("../household_size/Pyramid/Pyramid_",
               "20102019_hhsize_by_tenure.pdf"),
        height = 5, width = 5)
    x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-4)
    pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019, pop.pyrs$year_2010),
                                legend = c("ACS 2015-2019", "Census 2010"),
                                LRcolnames = c("Owner", "Renter"),
                                LRmain = c("Owner", "Renter"))
    
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[5] ,
                                                 border = pop.cols[5]),
                                 pyr2.par = list(col = pop.cols[3] , 
                                                 border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = c("ACS 2015-2019",
                                                 "Census 2010"),
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .65,
                                 cex.sub = .75)
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
    pdf(paste0("../household_size/Pyramid/Pyramid_",
               "20102019_hhsize_by_tenure_HRA.pdf"),
        height = 5, width = 5)
    for(hra.name in hra@data$HRA2010v2_){
      hh_size_kc <- hh_size_hra_2000 %>% 
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
      
      
      x_at <- round(seq(0, max(cbind(pyr.1, pyr.2)), length.out = 5),-2)
      pyr.obj <- get.bPop.pyramid(list(pyr.1, pyr.2),
                                  legend = c("ACS 2015-2019", "Census 2010"),
                                  LRcolnames = c("Owner", "Renter"),
                                  LRmain = c("Owner", "Renter"))
      
      pop.pyramid.bayesPop.pyramid(pyr.obj,
                                   pyr1.par = list(col = pop.cols[5] ,
                                                   border = pop.cols[5]),
                                   pyr2.par = list(col = pop.cols[3] , 
                                                   border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = c("ACS 2015-2019",
                                                   "Census 2010"),
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .76,
                                   cex.sub = .75)
      title(paste0("Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   hra.name),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    
  }
  
  
}

cases.int.hra <- classIntervals(covid_hra_tmp$Confirmed_Cases,
                                style = 'jenks',
                                n = 9)

breaks <- cases.int.hra$brks
breaks <- c(0, 250, 500,
            1500, 2500,
            3500, 4500, 5000, 6000,
            6600)
## Get color based on RColorBrwere palette for 
## each area

cases.pal <- brewer.pal(n = 9, name = "Blues")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

cases.int.hra <- classIntervals(covid_hra_tmp$Confirmed_Cases,
                                style = "fixed",
                                fixedBreaks = breaks,
                                n = 9)
cases.col.hra <- findColours(cases.int.hra, cases.pal)


pdf(paste0("../COVIDPlots/Map_HRA_Cases.pdf"),
    height = 5, width = 5)

plot(hra,
     col = cases.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Cases',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = cases.pal,
       legend = names(attr(cases.col.hra, 'table')))
title(paste0("Cumulative COVID-19 Cases\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()
# COVID ####

# covid_WA <- readxl::read_xlsx('../Data/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx',
#                               sheet = "Deaths")
# 
# cases_WA <- readxl::read_xlsx('../Data/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx',
#                               sheet = "Cases")
# covid_WA <- covid_WA %>%
#   filter(County == "King County") %>%
#   group_by(County) %>%
#   dplyr::select(-WeekStartDate) %>%
#   summarise(across(where(is.numeric), ~sum(.x))) %>%
#   pivot_longer(cols = where(is.numeric),
#                names_to = "metric",
#                values_to = "metric_val") %>% 
#   mutate(metric = gsub("Age ", "",
#                        metric))
# cases_WA <- cases_WA %>%
#   filter(County == "King County") %>%
#   group_by(County) %>%
#   dplyr::select(-WeekStartDate) %>%
#   summarise(across(where(is.numeric), ~sum(.x))) %>%
#   pivot_longer(cols = where(is.numeric),
#                names_to = "metric",
#                values_to = "metric_val") %>%
#   filter()


covid_KC_overall <- readxl::read_xlsx('../Data/overall_geo-aug-30.xlsx',
                                      sheet = "HRA")
covid_KC <- readxl::read_xlsx('../Data/overall_city_demo-aug-30.xlsx')
covid_KC_race <- readxl::read_xlsx('../Data/overall_city_demo-aug-30.xlsx',
                                   sheet = 'Race_Ethnicity')

convert_ages <- expand.grid(AgeStartCovid = seq(0,80,10),
                            AgeStartOFM = seq(0,85,5))
convert_ages <- convert_ages[(convert_ages$AgeStartCovid -
                                convert_ages$AgeStartOFM) %in% c(0,-5),]
convert_ages$AgeCovid <- paste(convert_ages$AgeStartCovid,
                               convert_ages$AgeStartCovid + 9,
                               sep = "-")
convert_ages$AgeOFM <- paste(convert_ages$AgeStartOFM,
                             convert_ages$AgeStartOFM + 4,
                             sep = "-")
convert_ages <- convert_ages %>% 
  mutate(AgeCovid = ifelse(AgeStartCovid == 80,
                           "80+", AgeCovid),
         AgeOFM = ifelse(AgeStartOFM == 85,
                         "85+", AgeOFM))

cases.tmp <- covid_KC %>% 
  filter(Age_Group != "Unknown") %>% 
  filter(City == "All King County") %>% 
  dplyr::select(Confirmed_Cases) %>% 
  mutate(Population2 = Confirmed_Cases) %>% 
  as.matrix()


cov.tmp <- covid_KC %>% 
  filter(Age_Group != "Unknown") %>% 
  filter(City == "All King County") %>% 
  dplyr::select(Hospitalizations, Deaths) %>% 
  as.matrix()

row.names(cov.tmp) <-
  row.names(cases.tmp) <- unique(convert_ages$AgeCovid)
colnames(cov.tmp) <-
  colnames(cases.tmp) <- c("Hospitalizations", "Deaths")

pyr.obj <- get.bPop.pyramid(list(cases.tmp, cov.tmp),
                            legend = c("Cases", "Hosp/Death"),
                            LRcolnames = c("Hospitalizations", "Deaths"),
                            LRmain = c("Hospitalizations", "Deaths"))

if(!dir.exists("../COVIDPlots/")){
  dir.create("../COVIDPlots/")
}
pdf("../COVIDPlots/Pyramid_CasesHospDeath.pdf",
    height = 5, width = 5)
par(lend = 1)
pop.pyramid.bayesPop.pyramid(pyr.obj,
                             pyr1.par = list(col = pop.cols[4], 
                                             border = pop.cols[4]),
                             
                             pyr2.par = list(col = pop.cols[2], 
                                             border = pop.cols[2]),
                             legend_pos = "topright",
                             legend_text = c("Cases", "Hosp/Death"))
title(paste0("Cumulative COVID-19 Outcomes\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)

dev.off()

cities <- unique(covid_KC$City)[-1]
pdf("../COVIDPlots/Pyramid_City_CasesHospDeath.pdf",
    height = 5, width = 5)
for(city in cities){
  cases.tmp <- covid_KC %>% 
    filter(Age_Group != "Unknown") %>% 
    filter(City == city) %>% 
    dplyr::select(Confirmed_Cases) %>% 
    mutate(Population2 = Confirmed_Cases) %>% 
    as.matrix()
  
  
  cov.tmp <- covid_KC %>% 
    filter(Age_Group != "Unknown") %>% 
    filter(City == city) %>% 
    dplyr::select(Hospitalizations, Deaths) %>% 
    as.matrix()
  
  row.names(cov.tmp) <-
    row.names(cases.tmp) <- unique(convert_ages$AgeCovid)
  colnames(cov.tmp) <-
    colnames(cases.tmp) <- c("Hospitalizations", "Deaths")
  
  pyr.obj <- get.bPop.pyramid(list(cases.tmp, cov.tmp),
                              legend = c("Cases", "Hosp/Death"),
                              LRcolnames = c("Hospitalizations", "Deaths"),
                              LRmain = c("Hospitalizations", "Deaths"))
  if(sum(cases.tmp[,1]) != 0){
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[4], 
                                                 border = pop.cols[4]),
                                 
                                 pyr2.par = list(col = pop.cols[2], 
                                                 border = pop.cols[2]),
                                 legend_pos = "topright",
                                 legend_text = c("Cases", "Hosp/Death"))
    
    title(paste0("Cumulative COVID-19 Outcomes by Age\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n", hra.name,
                 " (as of Aug. 30, 2021)"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    
  }
  
}
dev.off()



## HRA maps ####

covid_hra_tmp <- covid_KC_overall %>% 
  filter(Location_Name != "All King County" &
           Location_Name != "Unknown") %>% 
  mutate(Location_Name = ifelse(Location_Name == "Fed Way-Dash Pt",
                                "Fed Way-Dash Point/Woodmont",
                                Location_Name)) %>% 
  dplyr::select(Location_Name,
                Population, Confirmed_Cases,
                Hospitalizations, Deaths) 

### Cases ####
range(covid_hra_tmp$Confirmed_Cases)
covid_hra_tmp$CasesPrev <- covid_hra_tmp$Confirmed_Cases/
  covid_hra_tmp$Population
covid_hra_tmp$CasesDist <- covid_hra_tmp$Confirmed_Cases/
  sum(covid_hra_tmp$Confirmed_Cases)

cases.int.hra <- classIntervals(covid_hra_tmp$Confirmed_Cases,
                                style = 'jenks',
                                n = 9)

breaks <- cases.int.hra$brks
breaks <- c(0, 250, 500,
            1500, 2500,
            3500, 4500, 5000, 6000,
            6600)
## Get color based on RColorBrwere palette for 
## each area

cases.pal <- brewer.pal(n = 9, name = "Blues")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

cases.int.hra <- classIntervals(covid_hra_tmp$Confirmed_Cases,
                                style = "fixed",
                                fixedBreaks = breaks,
                                n = 9)
cases.col.hra <- findColours(cases.int.hra, cases.pal)


pdf(paste0("../COVIDPlots/Map_HRA_Cases.pdf"),
    height = 5, width = 5)

plot(hra,
     col = cases.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Cases',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex= 0.75,
       border = FALSE,
       fill = cases.pal,
       legend = names(attr(cases.col.hra, 'table')))
title(paste0("Cumulative COVID-19 Cases\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

#### Prevalence ####
prev.int.hra <- classIntervals(covid_hra_tmp$CasesPrev,
                               style = 'jenks',
                               n = 9)

breaks <- prev.int.hra$brks
breaks <- c(0, .015, .03,
            .045, .06, .075,
            .085, .095, .105, .12)
prev.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

prev.int.hra <- classIntervals(covid_hra_tmp$CasesPrev,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
prev.col.hra <- findColours(prev.int.hra, prev.pal)


pdf(paste0("../COVIDPlots/Map_HRA_CasesPrev.pdf"),
    height = 5, width = 5)
plot(hra,
     col = prev.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Prevalence',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = prev.pal,
       legend = names(attr(prev.col.hra, 'table')))
title(paste0("Prevalence of Cumulative COVID-19 Cases\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

#### Distribution ####
dist.int.hra <- classIntervals(covid_hra_tmp$CasesDist,
                               style = 'jenks',
                               n = 9)

breaks <- dist.int.hra$brks
breaks <- c(0, .01, .015, .02,
            .025, .03, .035,
            .04, .045, .055)

dist.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

dist.int.hra <- classIntervals(covid_hra_tmp$CasesDist,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
dist.col.hra <- findColours(dist.int.hra, dist.pal)


pdf(paste0("../COVIDPlots/Map_HRA_CasesDist.pdf"),
    height = 5, width = 5)
plot(hra,
     col = dist.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Distribution',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = dist.pal,
       legend = names(attr(dist.col.hra, 'table')))
title(paste0("Distribution of Cumulative COVID-19 Cases\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()


### Hospitalizations ####

range(covid_hra_tmp$Hospitalizations)
covid_hra_tmp$HospPrev <- covid_hra_tmp$Hospitalizations/
  covid_hra_tmp$Population
covid_hra_tmp$HospDist <- covid_hra_tmp$Hospitalizations/
  sum(covid_hra_tmp$Hospitalizations)

hosp.int.hra <- classIntervals(covid_hra_tmp$Hospitalizations,
                               style = 'jenks',
                               n = 9)

breaks <- hosp.int.hra$brks
breaks <- c(0, 25, 50,
            75, 100,
            150, 200, 300, 400,
            450)
## Get color based on RColorBrwere palette for 
## each area

hosp.pal <- brewer.pal(n = 9, name = "Blues")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

hosp.int.hra <- classIntervals(covid_hra_tmp$Hospitalizations,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
hosp.col.hra <- findColours(hosp.int.hra, hosp.pal)


pdf(paste0("../COVIDPlots/Map_HRA_Hospitalizations.pdf"),
    height = 5, width = 5)
plot(hra,
     col = hosp.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Hospitalizations',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = hosp.pal,
       legend = names(attr(hosp.col.hra, 'table')))
title(paste0("Cumulative COVID-19 Hospitalizations\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

#### Prevalence ####
prev.int.hra <- classIntervals(covid_hra_tmp$HospPrev,
                               style = 'jenks',
                               n = 9)

breaks <- prev.int.hra$brks
breaks <- c(0, .0005, .001,
            .002, .003, .004,
            .005, .006, .007, .0085)
prev.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

prev.int.hra <- classIntervals(covid_hra_tmp$HospPrev,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
prev.col.hra <- findColours(prev.int.hra, prev.pal)


pdf(paste0("../COVIDPlots/Map_HRA_HospPrev.pdf"),
    height = 5, width = 5)
plot(hra,
     col = prev.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Prevalence',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = prev.pal,
       legend = names(attr(prev.col.hra, 'table')))
title(paste0("Prevalence of Cumulative COVID-19 Hospitalizations\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)

dev.off()

#### Distribution ####
dist.int.hra <- classIntervals(covid_hra_tmp$HospDist,
                               style = 'jenks',
                               n = 9)

breaks <- dist.int.hra$brks
breaks <- c(0, .01, .015, .02,
            .025, .03, .035,
            .04, .045, .055)

dist.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

dist.int.hra <- classIntervals(covid_hra_tmp$CasesDist,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
dist.col.hra <- findColours(dist.int.hra, dist.pal)


pdf(paste0("../COVIDPlots/Map_HRA_HospDist.pdf"),
    height = 5, width = 5)
plot(hra,
     col = dist.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Distribution',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = dist.pal,
       legend = names(attr(dist.col.hra, 'table')))
title(paste0("Distribution of Cumulative COVID-19 Hospitalizations\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)

dev.off()


### Deaths ####
range(covid_hra_tmp$Deaths)
covid_hra_tmp$DeathPrev <- covid_hra_tmp$Deaths/
  covid_hra_tmp$Population
covid_hra_tmp$DeathDist <- covid_hra_tmp$Deaths/
  sum(covid_hra_tmp$Deaths)
death.int.hra <- classIntervals(covid_hra_tmp$Deaths,
                                style = 'jenks',
                                n = 9)

breaks <- death.int.hra$brks
breaks <- c(0, 5, 15,
            30,
            45, 60, 75, 90,
            100)
## Get color based on RColorBrwere palette for 
## each area

death.pal <- brewer.pal(n = 9, name = "Blues")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

death.int.hra <- classIntervals(covid_hra_tmp$Deaths,
                                style = "fixed",
                                fixedBreaks = breaks,
                                n = 9)
death.col.hra <- findColours(death.int.hra, death.pal)


pdf(paste0("../COVIDPlots/Map_HRA_Deaths.pdf"),
    height = 5, width = 5)
plot(hra,
     col = death.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Deaths',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = death.pal,
       legend = names(attr(death.col.hra, 'table')))
title(paste0("Cumulative COVID-19 Deaths\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)

dev.off()



#### Prevalence ####
prev.int.hra <- classIntervals(covid_hra_tmp$DeathPrev,
                               style = 'jenks',
                               n = 9)

breaks <- prev.int.hra$brks
breaks <- c(0, .00025, .0005,
            .0007, .0008, .0009, .001,
            .00125, .0015, .002, .0085)
prev.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

prev.int.hra <- classIntervals(covid_hra_tmp$DeathPrev,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
prev.col.hra <- findColours(prev.int.hra, prev.pal)


pdf(paste0("../COVIDPlots/Map_HRA_DeathPrev.pdf"),
    height = 5, width = 5)
plot(hra,
     col = prev.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Prevalence',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = prev.pal,
       legend = names(attr(prev.col.hra, 'table')))
title(paste0("Prevalence of Cumulative COVID-19 Deaths\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()

#### Distribution ####
dist.int.hra <- classIntervals(covid_hra_tmp$DeathDist,
                               style = 'jenks',
                               n = 9)

breaks <- dist.int.hra$brks
breaks <- c(0, .0025, .005, .0075,
            .01, .015, .02, .025,
            .035, .045, .06)

dist.pal <- brewer.pal(n = 9,  name = "YlGnBu")

covid_hra_tmp <- covid_hra_tmp[match(covid_hra_tmp$Location_Name,
                                     hra@data$HRA2010v2_), ]

dist.int.hra <- classIntervals(covid_hra_tmp$DeathDist,
                               style = "fixed",
                               fixedBreaks = breaks,
                               n = 9)
dist.col.hra <- findColours(dist.int.hra, dist.pal)


pdf(paste0("../COVIDPlots/Map_HRA_DeathDist.pdf"),
    height = 5, width = 5)
plot(hra,
     col = dist.col.hra,
     border = 'grey48', lwd = .25,
     main = "")
legend('bottomleft',
       title = 'Distribution',
       title.adj = 0,
       ncol = 2,
       bty = 'n',
       cex 0.75,
       border = FALSE,
       fill = dist.pal,
       legend = names(attr(dist.col.hra, 'table')))
title(paste0("Distribution of Cumulative COVID-19 Deaths\n",
             ""),
      font.main = 2, outer = FALSE,
      adj = 0, cex.main = 1)

title(paste0("\n",
             "King County (as of Aug. 30, 2021)"),
      font.main = 1, outer = FALSE,
      adj = 0, cex.main = 1)
dev.off()



for(race in covid_KC_race$Race_Ethnicity){
  cases.tmp <- covid_KC_race %>% 
    filter(Age_Group != "Unknown") %>% 
    filter(City == "All King County") %>% 
    filter(Race_Ethnicity)
  dplyr::select(Confirmed_Cases) %>% 
    mutate(Population2 = Confirmed_Cases) %>% 
    as.matrix()
  
  
  cov.tmp <- covid_KC %>% 
    filter(Age_Group != "Unknown") %>% 
    filter(City == "All King County") %>% 
    dplyr::select(Hospitalizations, Deaths) %>% 
    as.matrix()
  
  row.names(cov.tmp) <-
    row.names(cases.tmp) <- unique(convert_ages$AgeCovid)
  colnames(cov.tmp) <-
    colnames(cases.tmp) <- c("Hospitalizations", "Deaths")
  
  pyr.obj <- get.bPop.pyramid(list(cases.tmp, cov.tmp),
                              legend = c("Cases", "Hosp/Death"),
                              LRcolnames = c("Hospitalizations", "Deaths"),
                              LRmain = c("Hospitalizations", "Deaths"))
  
  pdf("../COVIDPlots/Pyramid_",
      race, "_CasesHospDeath.pdf",
      height = 5, width = 5)
  par(lend = 1)
  plot(pyr.obj,
       pyr1.par = list(col = pop.cols[4], 
                       border = pop.cols[4]),
       
       pyr2.par = list(col = pop.cols[2], 
                       border = pop.cols[2]))
  title(paste0("Cumulative COVID-19 Outcomes by Age\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               "King County, ",
               race, "(as of Aug. 30, 2021)"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
  
  dev.off()
}
