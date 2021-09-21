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

# Load data ####
## Household Size ####

### Direct ####

hh_tract <- readRDS(paste0('../household_size/',
                           'hh_by_hh_size_and_tenure_ct.RDS'))


### Smoothed ####

# hh_hra_smoothed <- readRDS(paste0('../household_size/',
#                                   'Report_estimates/',
#                                   'average_hh_size_renters_hra.RDS'))
# hh_kc_preds <- readRDS(paste0('../household_size/',
#                               'Report_estimates/',
#                               'housing_indicators_kc_preds.rds'))

## Census tracts ####

kc_tracts <- get_acs("tract",
                     table = "B01001",
                     geometry = TRUE,
                     year = 2019,
                     survey = "acs5",
                     state = "WA",
                     county = "King",
                     cache_table = TRUE) %>%
  filter(variable == "B01001_001") %>% 
  filter(!st_is_empty(geometry))

kc_tracts_2000 <- get_acs("tract",
                          table = "B01001",
                          geometry = TRUE,
                          year = 2009,
                          survey = "acs5",
                          state = "WA",
                          county = "King",
                          cache_table = TRUE) %>%
  filter(variable == "B01001_001") %>% 
  filter(!st_is_empty(geometry))

names(kc_tracts)

### Convert to sp ####
kc_tracts_poly <- kc_tracts %>% 
  st_geometry() %>%
  as(., "Spatial")


kc_tracts_2000_poly <- kc_tracts_2000 %>% 
  st_geometry() %>%
  as(., "Spatial")
kc_tracts_2000_poly <- spTransform(kc_tracts_2000_poly,
                                   kc_tracts_poly@proj4string)


## HRA ####
hra <- readOGR(dsn = "../Data",
               layer = "HRA_2010Block_Clip")

hra <- spTransform(hra,
                   kc_tracts_poly@proj4string)

## tracts_to_hra ####
load('../Data/tracts_to_hra.rda')

# Household Size: Direct Estimates ####

if(!dir.exists("../household_size/Pyramid/")){
  dir.create("../household_size/Pyramid")
  dir.create("../household_size/Pyramid/HRA/")
}

if(!dir.exists("../household_size/HRA/")){
  dir.create("../household_size/HRA/")
  dir.create("../household_size/HRA/Population/")
  dir.create("../household_size/HRA/Prevalence/")
  dir.create("../household_size/HRA/Distribution/")
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

pop.cols <- brewer.pal(n = 5,
                       name = 'Blues')

pop.pyrs <- list()

## Pyramids ####
for(year in c(2010, 2014, 2019)){
  
  #### Households by HRA by year ####
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
    jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_",
                year, "_hhsize_by_tenure_HRA_",
                hra.name.file, ".jpeg"),
         height = 480, width = 480)
    {
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
                   hra.name, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ## Households by year ####
  
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
                                 cex.axis = .65,
                                 cex.sub = .75,
                                 x_lims = c(-185000,185000))
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "King County (Estimated from ", legend_string, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  ## Compare 2010, 2015-2019 ####
  if(year == 2019){
    ### Households ####
    # pdf(paste0("../household_size/Pyramid/Pyramid_",
    #            "20102019_hhsize_by_tenure.pdf"),
    #     height = 5, width = 5)
    jpeg(paste0("../household_size/Pyramid/Pyramid_",
                "20102019_hhsize_by_tenure.jpeg"),
         height = 480, width = 480)
    {
      x_at <- round(seq(0, max(unlist(pop.pyrs)), length.out = 5),-2)
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
                                   cex.sub = .75,
                                   x_lims = c(-185000,185000))
      title(paste0("Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      title(paste0("\n",
                   "King County (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    ### Prevalence ####
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
                   "King County, ",
                   year, "(Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
      
    }
    dev.off()
    
    ### Households by HRA ####
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
      hra.name.file <- gsub("\\.","",
                            hra.name)
      hra.name.file <- gsub("/","",
                            hra.name.file)
      jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_",
                  "20102019_hhsize_by_tenure_HRA_",
                  hra.name.file, ".jpeg"),
           height = 480, width = 480)
      {
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
                     hra.name, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
    
    
    ### Prevalence by HRA ####
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
      jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_Prevalence_",
                  "20102019_hhsize_by_tenure_HRA_",
                  hra.name.file, ".jpeg"),
           height = 480, width = 480)
      {
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
                     hra.name, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
}

## Pyramids: pre 2010 ####
for(year in c(2000, 2009)){
  
  ### Households by HRA by year ####
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
    jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_",
                year, "_hhsize_by_tenure_HRA_",
                hra.name.file, ".jpeg"),
         height = 480, width = 480)
    {
      
      pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                            border = pop.cols[3]),
                                   legend_pos = "topright",
                                   legend_text = legend_string,
                                   x_at = c(rev(-x_at[-1]), x_at),
                                   x_labels = c(rev(x_at[-1]), x_at),
                                   cex.axis = .76,
                                   cex.sub = .75,
                                   x_lims = c(-17000,17000))
      title(paste0("Households by Size and Tenure\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   hra.name, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ## Households by year ####
  
  # pdf(paste0("../household_size/Pyramid/Pyramid_",
  #            year, "_hhsize_by_tenure.pdf"),
  #     height = 5, width = 5)
  jpeg(paste0("../household_size/Pyramid/Pyramid_",
              year, "_hhsize_by_tenure.jpeg"),
       height = 480, width = 480)
  {
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
    if(year != 2000){
      legend_string <- paste0("ACS ", 
                              year - 4, 
                              "-", year)
    }else{
      legend_string <- paste0("Census ", year)
    }
    pop.pyramid.bayesPop.pyramid(pyr.obj, pyr1.par = list(col = pop.cols[3] , 
                                                          border = pop.cols[3]),
                                 legend_pos = "topright",
                                 legend_text = legend_string,
                                 x_at = c(rev(-x_at[-1]), x_at),
                                 x_labels = c(rev(x_at[-1]), x_at),
                                 cex.axis = .65,
                                 cex.sub = .75,
                                 x_lims = c(-185000,185000))
    title(paste0("Households by Size and Tenure\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 hra.name, " (Estimated from ", legend_string, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
  }
  dev.off()
  
}

## Compare 2000, 2015-2019 ####
### Households ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002019_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_",
            "20002019_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
{
  x_at <- round(seq(0, max(unlist(pop.pyrs)),
                    length.out = 5), -2)
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019,
                                   pop.pyrs$year_2000),
                              legend = c("ACS 2015-2019",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[5] ,
                                               border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[2] , 
                                               border = pop.cols[2]),
                               legend_pos = "topright",
                               legend_text = c("ACS 2015-2019",
                                               "Census 2000"),
                               x_at = c(rev(-x_at[-1]), x_at),
                               x_labels = c(rev(x_at[-1]), x_at),
                               cex.axis = .65,
                               cex.sub = .75,
                               x_lims = c(-185000,185000))
  title(paste0("Households by Size and Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               "King County (Estimated from ", legend_string, ")"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
}
dev.off()

### Prevalence ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002019_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence",
            "20002019_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
{
  x_at <- c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2)
  x_labels <- abs(x_at)
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2019/
                                     sum(pop.pyrs$year_2019),
                                   pop.pyrs$year_2000/
                                     sum(pop.pyrs$year_2000)),
                              legend = c("ACS 2015-2019",
                                         "Census 2000"),
                              LRcolnames = c("Owner", "Renter"),
                              LRmain = c("Owner", "Renter"))
  
  pop.pyramid.bayesPop.pyramid(pyr.obj,
                               pyr1.par = list(col = pop.cols[5] ,
                                               border = pop.cols[5]),
                               pyr2.par = list(col = pop.cols[2] , 
                                               border = pop.cols[2]),
                               legend_pos = "topright",
                               legend_text = c("ACS 2015-2019",
                                               "Census 2000"),
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
               "King County (Estimated from ", legend_string, ")"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
}
dev.off()


### Households by HRA ####
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
  jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_",
              "20002019_hhsize_by_tenure_HRA_",
              hra.name.file, ".jpeg"),
       height = 480, width = 480)
  {
    pop.pyramid.bayesPop.pyramid(pyr.obj,
                                 pyr1.par = list(col = pop.cols[5] ,
                                                 border = pop.cols[5]),
                                 pyr2.par = list(col = pop.cols[2] , 
                                                 border = pop.cols[2]),
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
                 hra.name, " (Estimated from ", legend_string, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = .8)
  }
  dev.off()
}

### Prevalence by HRA ####

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
  jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_Prevalence_",
              "20002019_hhsize_by_tenure_HRA_",
              hra.name.file, ".pdf"),
       height = 480, width = 480)
  {
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
    title(paste0("\n", hra.name, " (Estimated from ", legend_string, ")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = .8)
  }
  dev.off()
}


## Compare 2000, 2010 ####
### Households ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002010_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_",
            "20002010_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
{
  x_at <- round(seq(0, max(unlist(pop.pyrs)),
                    length.out = 5), -2)
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
                               cex.sub = .75,
                               x_lims = c(-185000,185000))
  title(paste0("Households by Size and Tenure\n",
               ""),
        font.main = 2, outer = FALSE,
        adj = 0, cex.main = 1)
  
  title(paste0("\n",
               "King County (Estimated from ", legend_string, ")"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
}
dev.off()

### Prevalence ####
# pdf(paste0("../household_size/Pyramid/Pyramid_",
#            "20002019_hhsize_by_tenure.pdf"),
#     height = 5, width = 5)
jpeg(paste0("../household_size/Pyramid/Pyramid_Prevalence",
            "20002010_hhsize_by_tenure.jpeg"),
     height = 480, width = 480)
{
  x_at <- c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2)
  x_labels <- abs(x_at)
  pyr.obj <- get.bPop.pyramid(list(pop.pyrs$year_2010/
                                     sum(pop.pyrs$year_2010),
                                   pop.pyrs$year_2000/
                                     sum(pop.pyrs$year_2000)),
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
               "King County (Estimated from Census)"),
        font.main = 1, outer = FALSE,
        adj = 0, cex.main = 1)
}
dev.off()


### Households by HRA ####
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
  jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_",
              "20002010_hhsize_by_tenure_HRA_",
              hra.name.file, ".pdf"),
       height = 480, width = 480)
  {
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
    title(paste0("\n", hra.name, " (Estimated from Census)"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = .8)
  }
  dev.off()
}


### Prevalence by HRA ####
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
  jpeg(paste0("../household_size/Pyramid/HRA/Pyramid_Prevalence_",
              "20002010_hhsize_by_tenure_HRA_",
              hra.name.file, ".jpeg"),
       height = 480, width = 480)
  {
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
    title(paste0("\n", hra.name, " (Estimated from Census)"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = .8)
  }
  dev.off()
}


## HRA Maps: Direct Estimates ####
### Household Size ####
for(year in c(2000, 2009, 2010, 2014, 2019)){
  
  if(year >= 2010){
    hh_year_tmp <- hh_size_hra %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(HRA2010v2_)) %>% 
      group_by(FID_HRA_20, HRA2010v2_, hh_size) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(FID_HRA_20, HRA2010v2_) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20) %>% 
      mutate(CoV = SE/estimate)
  }else{
    hh_year_tmp <- hh_size_hra_2000 %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(HRA2010v2_)) %>% 
      group_by(FID_HRA_20, HRA2010v2_, hh_size) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(FID_HRA_20, HRA2010v2_) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20) %>% 
      mutate(CoV = SE/estimate)
  }
  
  if(year != 2010 &
     year != 2000){
    title_string <- paste0(year - 4, 
                           "-", year)
    
  }else{
    title_string <- year
  }
  
  ### Households ####
  hh.int.hra <- classIntervals(hh_year_tmp$estimate,
                               style = 'jenks',
                               n = 9)
  
  breaks <- hh.int.hra$brks
  breaks <- c(0, 250, 500,
              1000, 
              2500, 5000, 7500,
              10000, 15000, 25000)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  hh.pal <- brewer.pal(n = 9, name = "Blues")
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size)
    hra@data$Density <- 
      hra@data$Est <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Est[match(hra@data$FID_HRA_20,
                         hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$estimate
      hra@data$Density[match(hra@data$FID_HRA_20,
                             hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$Density
    }
    
    
    hh.int.hra <- classIntervals(hra@data$Est,
                                 style = "fixed",
                                 fixedBreaks = breaks,
                                 n = 9)
    hh.col.hra <- findColours(hh.int.hra, hh.pal)
    
    
    jpeg(paste0("../household_size/HRA/Population/", 
                "Households_Size",
                size, "_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = hh.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = hh.pal,
             legend = names(attr(hh.col.hra, 'table')))
      title(paste0("Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    jpeg(paste0("../household_size/HRA/Population/", 
                "Households_Size",
                size, "_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = hh.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      
      hatch.idx <- which(hra@data$Density > 0)
      for(poly in hatch.idx){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = hra@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = hh.pal,
             legend = names(attr(hh.col.hra, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ### Prevalence ####
  prev.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  hh_year_tmp <- hh_year_tmp %>% 
    left_join(hh_year_total,
              by = c("FID_HRA_20" = "FID_HRA_20",
                     "HRA2010v2_" = "HRA2010v2_"),
              suffix = c("", "_Total")) %>% 
    filter(!is.na(HRA2010v2_)) %>% 
    mutate(Prev = estimate/estimate_Total)
  prev.int.hra <- classIntervals(hh_year_tmp$Prev,
                                 style = 'jenks',
                                 n = 9)
  
  breaks <- prev.int.hra$brks
  breaks <- c(0, .05, .1,
              .15, .2, .25,
              .4, .5, .6, .75)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size)
    hra@data$Density <- 
      hra@data$Prev <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Prev[match(hra@data$FID_HRA_20,
                          hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$Prev
      hra@data$Density[match(hra@data$FID_HRA_20,
                             hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$Density
    }
    
    
    prev.int.hra <- classIntervals(hra@data$Prev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prev.col.hra <- findColours(prev.int.hra, prev.pal)
    
    
    jpeg(paste0("../household_size/HRA/Prevalence/", 
                "Households_Size",
                size, "_Prevalence_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prev.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.hra, 'table')))
      title(paste0("Prevalence of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    
    jpeg(paste0("../household_size/HRA/Prevalence/", 
                "Households_Size",
                size, "_Prevalence_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prev.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      
      hatch.idx <- which(hra@data$Density > 0)
      for(poly in hatch.idx){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = hra@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.hra, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Prevalence of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ### Distribution ####
  
  breaks <- c(0, .005, .01,
              .02, .025, .03,
              .045, .06, .075, .1)
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size) %>% 
      mutate(Dist = estimate/sum(estimate))
    hh_size_tmp$Dist %>% summary() %>% print()
    hra@data$Density <-
      hra@data$Dist <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Dist[match(hra@data$FID_HRA_20,
                          hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$Dist
      hra@data$Density[match(hra@data$FID_HRA_20,
                             hh_size_tmp$FID_HRA_20)] <- hh_size_tmp$Density
    }
    prev.int.hra <- classIntervals(hra@data$Dist,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prev.col.hra <- findColours(prev.int.hra, prev.pal)
    
    
    
    jpeg(paste0("../household_size/HRA/Distribution/", 
                "Households_Size",
                size, "_Distribution_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prev.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.hra, 'table')))
      title(paste0("Distribution of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    jpeg(paste0("../household_size/HRA/Distribution/", 
                "Households_Size",
                size, "_Distribution_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(hra,
           col = prev.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      hatch.idx <- which(hra@data$Density > 0)
      for(poly in hatch.idx){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = hra@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.hra, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Distribution of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    # End size dist loop
  }
  # End year loop
}

### Household Size by Tenure ####
for(year in c(2000, 2009, 2010, 2014, 2019)){
  
  
  if(year >= 2010){
    
    hh_year_tmp <- hh_size_hra %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(HRA2010v2_)) %>% 
      group_by(FID_HRA_20, HRA2010v2_, hh_size, tenure) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(FID_HRA_20, HRA2010v2_) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20) %>% 
      mutate(CoV = SE/estimate)
  }else{
    
    hh_year_tmp <- hh_size_hra_2000 %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(HRA2010v2_)) %>% 
      group_by(FID_HRA_20, HRA2010v2_, hh_size, tenure) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(FID_HRA_20, HRA2010v2_) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(FID_HRA_20) %>% 
      mutate(CoV = SE/estimate)
  }
  
  if(year != 2010 &
     year != 2000){
    title_string <- paste0(year - 4, 
                           "-", year)
    
  }else{
    title_string <- year
  }
  
  
  ### Households ####
  hh.int.hra <- classIntervals(hh_year_tmp$estimate,
                               style = 'jenks',
                               n = 9)
  
  breaks <- hh.int.hra$brks
  breaks <- c(0, 250, 500,
              1000, 
              2500, 5000, 7500,
              10000, 15000, 25000)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  hh.pal <- brewer.pal(n = 9, name = "Blues")
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type)
      hra@data$Density <- 
        hra@data$Est <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Est[match(hra@data$FID_HRA_20,
                           hh_size_tmp$FID_HRA_20)] <-
          hh_size_tmp$estimate
        
        hra@data$Density[match(hra@data$FID_HRA_20,
                              hh_size_tmp$FID_HRA_20)] <- 
          hh_size_tmp$Density
        
        hh.int.hra <- classIntervals(hra@data$Est,
                                     style = "fixed",
                                     fixedBreaks = breaks,
                                     n = 9)
        hh.col.hra <- findColours(hh.int.hra, hh.pal)
      }else{
        hh.int.hra$var <- rep(0, nrow(hra@data))
        hh.col.hra <- findColours(hh.int.hra, hh.pal)
      }
      
      jpeg(paste0("../household_size/HRA/Population/", 
                  "Households_Size",
                  size, "_", tenure.type, 
                  "_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = hh.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        legend('bottomleft',
               title = 'Households',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = hh.pal,
               legend = names(attr(hh.col.hra, 'table')))
        title(paste0(tenure.type,
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      jpeg(paste0("../household_size/HRA/Population/", 
                  "Households_Size",
                  size, "_CoV_", tenure.type, 
                  "_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = hh.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        
        hatch.idx <- which(hra@data$Density > 0)
        for(poly in hatch.idx){
          points <- hra@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = hra@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Households',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = hh.pal,
               legend = names(attr(hh.col.hra, 'table')))
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        title(paste0(tenure.type,
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  
  
  ### Prevalence ####
  prev.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  hh_year_tmp <- hh_year_tmp %>% 
    left_join(hh_year_total,
              by = c("FID_HRA_20" = "FID_HRA_20",
                     "HRA2010v2_" = "HRA2010v2_"),
              suffix = c("", "_Total")) %>% 
    filter(!is.na(HRA2010v2_)) %>% 
    mutate(Prev = estimate/estimate_Total)
  prev.int.hra <- classIntervals(hh_year_tmp$Prev,
                                 style = 'jenks',
                                 n = 9)
  
  breaks <- prev.int.hra$brks
  breaks <- c(0, .05, .1,
              .15, .2, .25,
              .4, .5, .6, .75)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type)
      hra@data$Density <- 
        hra@data$Prev <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Prev[match(hra@data$FID_HRA_20,
                            hh_size_tmp$FID_HRA_20)] <-
          hh_size_tmp$Prev
        
        hra@data$Density[match(hra@data$FID_HRA_20,
                               hh_size_tmp$FID_HRA_20)] <- 
          hh_size_tmp$Density
        prev.int.hra <- classIntervals(hra@data$Prev,
                                       style = "fixed",
                                       fixedBreaks = breaks,
                                       n = 9)
        prev.col.hra <- findColours(prev.int.hra, prev.pal)
      }else{
        prev.int.hra$var <- rep(0, nrow(hra@data))
        prev.col.hra <- findColours(prev.int.hra, prev.pal)
      }
      
      
      jpeg(paste0("../household_size/HRA/Prevalence/", 
                  "Households_Size",
                  size, "_", tenure.type,
                  "_Prevalence_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = prev.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        legend('bottomleft',
               title = 'Prevalence',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.hra, 'table')))
        title(paste0("Prevalence of ",
                     tenure.type, " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      jpeg(paste0("../household_size/HRA/Prevalence/", 
                  "Households_Size",
                  size, "_", tenure.type,
                  "_Prevalence_CoV_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = prev.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        
        hatch.idx <- which(hra@data$Density > 0)
        for(poly in hatch.idx){
          points <- hra@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = hra@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Prevalence',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.hra, 'table')))
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        title(paste0("Prevalence of ",
                     tenure.type,
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  
  ### Distribution ####
  breaks <- c(0, .005, .01,
              .02, .03,
              .04, .05, .075, .1, .15)
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type) %>% 
        mutate(Dist = estimate/sum(estimate))
      hh_size_tmp$Dist %>% summary() %>% print()
      hra@data$Density <-
        hra@data$Dist <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Dist[match(hra@data$FID_HRA_20,
                            hh_size_tmp$FID_HRA_20)] <- 
          hh_size_tmp$Dist
        hra@data$Density[match(hra@data$FID_HRA_20,
                               hh_size_tmp$FID_HRA_20)] <-
          hh_size_tmp$Density
        
        prev.int.hra <- classIntervals(hra@data$Dist,
                                       style = "fixed",
                                       fixedBreaks = breaks,
                                       n = 9)
        prev.col.hra <- findColours(prev.int.hra, prev.pal)
      }else{
        prev.int.hra$var <- rep(0, nrow(hra@data))
        prev.col.hra <- findColours(prev.int.hra, prev.pal)
      }
      
      
      jpeg(paste0("../household_size/HRA/Distribution/", 
                  "Households_Size",
                  size,"_", tenure.type,
                  "_Distribution_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = prev.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        legend('bottomleft',
               title = 'Distribution',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.hra, 'table')))
        title(paste0("Distribution of ",
                     tenure.type, " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      jpeg(paste0("../household_size/HRA/Distribution/", 
                  "Households_Size",
                  size,"_", tenure.type,
                  "_Distribution_CoV_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(hra,
             col = prev.col.hra,
             border = 'grey48', lwd = .25,
             main = "")
        
        hatch.idx <- which(hra@data$Density > 0)
        for(poly in hatch.idx){
          points <- hra@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = hra@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Distribution',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.hra, 'table')))
        
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        
        title(paste0("Distribution of ",
                     tenure.type, " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  # End year loop
}


## Tract Maps: Direct Estimates ####
if(!dir.exists('../household_size/Tract/')){
  dir.create('../household_size/Tract/')
  dir.create('../household_size/Tract/Population/')
  dir.create('../household_size/Tract/Prevalence/')
  dir.create('../household_size/Tract/Distribution/')
}
### Household Size ####
hh_size_tract <- hh_tract %>% 
  filter(Year >= 2010) %>% 
  mutate(SE = moe/qnorm(.95),
         CoV = SE/estimate)

hh_size_tract_2000 <- hh_tract %>% 
  filter(Year < 2010) %>% 
  mutate(SE = moe/qnorm(.95),
         CoV = SE/estimate)


for(year in c(2000, 2009, 2010, 2014, 2019)){
  
  ## Get appropriate census tract SpatialPolygonsDataFrame
  if(year < 2010){
    spatialdf <- data.frame(GEOID = kc_tracts_2000$GEOID)
    row.names(spatialdf) <- names(kc_tracts_2000_poly)
    tract_spatialdf <- SpatialPolygonsDataFrame(kc_tracts_2000_poly,
                                                data = spatialdf)
  }else{
    spatialdf <- data.frame(GEOID = kc_tracts$GEOID)
    row.names(spatialdf) <- names(kc_tracts_poly)
    tract_spatialdf <- SpatialPolygonsDataFrame(kc_tracts_poly,
                                                data = spatialdf)
  }
  
  ## Get appropriate estimates
  ## Based on Census tract definition
  
  if(year >= 2010){
    hh_year_tmp <- hh_size_tract %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(GEOID)) %>% 
      filter(GEOID %in% tract_spatialdf@data$GEOID) %>% 
      group_by(GEOID, hh_size) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(GEOID, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # No Info
    hh_year_tmp$CoVOver1 <- ifelse(hh_year_tmp$CoV >= 1,
                                   1, 0)
    
    # less than 80% significance
    
    
    hh_year_tmp$Density[hh_year_tmp$CoV < 1 & 
                          hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(GEOID) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      mutate(CoV = SE/estimate)
  }else{
    hh_year_tmp <- hh_size_tract_2000 %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(GEOID)) %>% 
      filter(GEOID %in% tract_spatialdf@data$GEOID) %>% 
      group_by(GEOID, hh_size) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(GEOID, hh_size) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # No Info
    hh_year_tmp$CoVOver1 <- ifelse(hh_year_tmp$CoV >= 1,
                                   1, 0)
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV < 1 &
                          hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(GEOID) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>%  
      mutate(CoV = SE/estimate)
  }
  
  if(year != 2010 &
     year != 2000){
    title_string <- paste0(year - 4, 
                           "-", year)
    legend_string <- "ACS"
  }else{
    title_string <- year
    legend_string <- "Census"
  }
  
  ### Households ####
  if(FALSE){
    ## Test to figure it out
    ## Takes time to run on Tracts
    hh.int.tract <- classIntervals(hh_year_tmp$estimate,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- hh.int.tract$brks
    breaks
  }
  breaks <- c(0, 50, 100,
              250, 500, 750,
              1000, 2000, 3000, 4500)
  
  hh.pal <- brewer.pal(n = 9, name = "Blues")
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size) 
    
    tract_spatialdf@data$Est <-
      tract_spatialdf@data$Density <- 0
    
    if(nrow(hh_size_tmp) != 0){
      poly_order.idx <- match(tract_spatialdf@data$GEOID,
                              hh_size_tmp$GEOID)
      tract_spatialdf@data$Est[poly_order.idx] <-
        hh_size_tmp$estimate
      tract_spatialdf@data$Density[poly_order.idx] <-
        hh_size_tmp$Density
    }
    hh.int.tract <- classIntervals(tract_spatialdf@data$Est,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    hh.col.tract <- findColours(hh.int.tract, hh.pal)
    
    
    
    
    jpeg(paste0("../household_size/Tract/Population/", 
                "Households_Size",
                size, "_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = hh.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = hh.pal,
             legend = names(attr(hh.col.tract, 'table')))
      title(paste0("Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    jpeg(paste0("../household_size/Tract/Population/", 
                "Households_Size",
                size, "_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = hh.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      
      hatch.idx <- which(tract_spatialdf@data$Density > 0)
      for(poly in hatch.idx){
        points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = tract_spatialdf@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = hh.pal,
             legend = names(attr(hh.col.tract, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ### Prevalence ####
  prev.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  hh_year_tmp <- hh_year_tmp %>% 
    left_join(hh_year_total,
              by = c("GEOID" = "GEOID"),
              suffix = c("", "_Total")) %>% 
    mutate(Prev = estimate/estimate_Total)
  
  ## Test to find good breaks
  if(FALSE){
    prev.int.tract <- classIntervals(hh_year_tmp$Prev,
                                     style = 'jenks',
                                     n = 9)
    
    breaks <- prev.int.tract$brks
    breaks
  }
  breaks <- c(0, .005, .01,
              .05, .1, .2,
              .25, .5, .75, .9)
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size)
    tract_spatialdf@data$Density <- 
      tract_spatialdf@data$Prev <- 0
    
    if(nrow(hh_size_tmp) != 0){
      tract_spatialdf@data$Prev[match(tract_spatialdf@data$GEOID,
                                      hh_size_tmp$GEOID)] <- 
        hh_size_tmp$Prev
      tract_spatialdf@data$Density[match(tract_spatialdf@data$GEOID,
                                         hh_size_tmp$GEOID)] <-
        hh_size_tmp$Density
    }
    
    
    prev.int.tract <- classIntervals(tract_spatialdf@data$Prev,
                                     style = "fixed",
                                     fixedBreaks = breaks,
                                     n = 9)
    prev.col.tract <- findColours(prev.int.tract, prev.pal)
    
    
    jpeg(paste0("../household_size/Tract/Prevalence/", 
                "Households_Size",
                size, "_Prevalence_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = prev.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.tract, 'table')))
      title(paste0("Prevalence of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    
    jpeg(paste0("../household_size/Tract/Prevalence/", 
                "Households_Size",
                size, "_Prevalence_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = prev.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      
      hatch.idx <- which(tract_spatialdf@data$Density > 0)
      for(poly in hatch.idx){
        points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = tract_spatialdf@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.tract, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Prevalence of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  ### Distribution ####
  
  breaks <- c(0, .0005, .001,
              .0025, .005, .0075,
              .01, .0125, .015, .2)
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size) %>% 
      mutate(Dist = estimate/sum(estimate))
    hh_size_tmp$Dist %>% summary() %>% print()
    tract_spatialdf@data$Density <-
      tract_spatialdf@data$Dist <- 0
    
    if(nrow(hh_size_tmp) != 0){
      tract_spatialdf@data$Dist[match(tract_spatialdf@data$GEOID,
                                      hh_size_tmp$GEOID)] <-
        hh_size_tmp$Dist
      tract_spatialdf@data$Density[match(tract_spatialdf@data$GEOID,
                                         hh_size_tmp$GEOID)] <-
        hh_size_tmp$Density
    }
    prev.int.tract <- classIntervals(tract_spatialdf@data$Dist,
                                     style = "fixed",
                                     fixedBreaks = breaks,
                                     n = 9)
    prev.col.tract <- findColours(prev.int.tract, prev.pal)
    
    
    
    jpeg(paste0("../household_size/Tract/Distribution/", 
                "Households_Size",
                size, "_Distribution_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = prev.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.tract, 'table')))
      title(paste0("Distribution of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    ### Add CoV ####
    jpeg(paste0("../household_size/Tract/Distribution/", 
                "Households_Size",
                size, "_Distribution_CoV_", year, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(1,1,1,1))
      plot(tract_spatialdf,
           col = prev.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      hatch.idx <- which(tract_spatialdf@data$Density > 0)
      for(poly in hatch.idx){
        points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = tract_spatialdf@data$Density[poly])
      }
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex = 0.75,
             border = FALSE,
             fill = prev.pal,
             legend = names(attr(prev.col.tract, 'table')))
      legend('bottomright',
             title = 'Significance',
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 0.75,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Distribution of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   title_string, " (Estimated from ", legend_string, ")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    # End size dist loop
  }
  # End year loop
}

### Household Size by Tenure ####

for(year in c(2000, 2009, 2010, 2014, 2019)){
  
  ## Get appropriate census tract SpatialPolygonsDataFrame
  if(year < 2010){
    spatialdf <- data.frame(GEOID = kc_tracts_2000$GEOID)
    row.names(spatialdf) <- names(kc_tracts_2000_poly)
    tract_spatialdf <- SpatialPolygonsDataFrame(kc_tracts_2000_poly,
                                                data = spatialdf)
  }else{
    spatialdf <- data.frame(GEOID = kc_tracts$GEOID)
    row.names(spatialdf) <- names(kc_tracts_poly)
    tract_spatialdf <- SpatialPolygonsDataFrame(kc_tracts_poly,
                                                data = spatialdf)
  }
  
  ## Get appropriate estimates
  ## Based on Census tract definition
  
  if(year >= 2010){
    hh_year_tmp <- hh_size_tract %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(GEOID)) %>% 
      filter(GEOID %in% tract_spatialdf@data$GEOID) %>% 
      group_by(GEOID, hh_size, tenure) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(GEOID, hh_size, tenure) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # No Info
    hh_year_tmp$CoVOver1 <- ifelse(hh_year_tmp$CoV >= 1,
                                   1, 0)
    
    # less than 80% significance
    
    
    hh_year_tmp$Density[hh_year_tmp$CoV < 1 & 
                          hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(GEOID) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      mutate(CoV = SE/estimate)
  }else{
    hh_year_tmp <- hh_size_tract_2000 %>% 
      filter(Year == year) %>% 
      filter(hh_size > 0) %>% 
      filter(!is.na(GEOID)) %>% 
      filter(GEOID %in% tract_spatialdf@data$GEOID) %>% 
      group_by(GEOID, hh_size, tenure) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>% 
      arrange(GEOID, hh_size, tenure) %>% 
      mutate(CoV = SE/estimate) %>% 
      mutate(Density = 0) 
    
    # No Info
    hh_year_tmp$CoVOver1 <- ifelse(hh_year_tmp$CoV >= 1,
                                   1, 0)
    # less than 80% significance
    hh_year_tmp$Density[hh_year_tmp$CoV < 1 &
                          hh_year_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    hh_year_tmp$Density[hh_year_tmp$CoV < 1/qnorm(.9) &
                          hh_year_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    hh_year_total <- hh_year_tmp %>% 
      group_by(GEOID) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      ungroup() %>%  
      mutate(CoV = SE/estimate)
  }
  
  if(year != 2010 &
     year != 2000){
    title_string <- paste0(year - 4, 
                           "-", year)
    legend_string <- "ACS"
  }else{
    title_string <- year
    legend_string <- "Census"
  }
  
  ### Households ####
  if(FALSE){
    ## Test to figure it out
    ## Takes time to run on Tracts
    hh.int.tract <- classIntervals(hh_year_tmp$estimate,
                                   style = 'jenks',
                                   n = 9)
    
    breaks <- hh.int.tract$brks
    breaks
  }
  breaks <- c(0, 50, 100,
              250, 500, 750,
              1000, 2000, 3000, 4500)
  
  hh.pal <- brewer.pal(n = 9, name = "Blues")
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type) 
      
      tract_spatialdf@data$Est <-
        tract_spatialdf@data$Density <- 0
      
      if(nrow(hh_size_tmp) != 0){
        poly_order.idx <- match(tract_spatialdf@data$GEOID,
                                hh_size_tmp$GEOID)
        tract_spatialdf@data$Est[poly_order.idx] <-
          hh_size_tmp$estimate
        tract_spatialdf@data$Density[poly_order.idx] <-
          hh_size_tmp$Density
        hh.int.tract <- classIntervals(tract_spatialdf@data$Est,
                                       style = "fixed",
                                       fixedBreaks = breaks,
                                       n = 9)
        hh.col.tract <- findColours(hh.int.tract, hh.pal)
      }else{
        hh.int.tract$var <- rep(0, nrow(tract_spatialdf@data))
        hh.col.tract <- findColours(hh.int.tract, hh.pal)
      }
      
      
      
      jpeg(paste0("../household_size/Tract/Population/", 
                  "Households_Size",
                  size, "_",
                  tenure.type, "_", 
                  year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = hh.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        legend('bottomleft',
               title = 'Households',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = hh.pal,
               legend = names(attr(hh.col.tract, 'table')))
        title(paste0(tenure.type,
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string,
                     " (Estimated from ", 
                     legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      jpeg(paste0("../household_size/Tract/Population/", 
                  "Households_Size",
                  size, "_",
                  tenure.type,
                  "_CoV_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = hh.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        
        hatch.idx <- which(tract_spatialdf@data$Density > 0)
        for(poly in hatch.idx){
          points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = tract_spatialdf@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Households',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = hh.pal,
               legend = names(attr(hh.col.tract, 'table')))
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        title(paste0(tenure.type,
                     "Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, 
                     " (Estimated from ", 
                     legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      # End tenure loop
    }
    # End size loop
  }
  
  ### Prevalence ####
  prev.pal <- brewer.pal(n = 9, name = "YlGnBu")
  
  hh_year_tmp <- hh_year_tmp %>% 
    left_join(hh_year_total,
              by = c("GEOID" = "GEOID"),
              suffix = c("", "_Total")) %>% 
    mutate(Prev = estimate/estimate_Total)
  
  ## Test to find good breaks
  if(FALSE){
    prev.int.tract <- classIntervals(hh_year_tmp$Prev,
                                     style = 'jenks',
                                     n = 9)
    
    breaks <- prev.int.tract$brks
    breaks
  }
  breaks <- c(0, .005, .01,
              .05, .1, .2,
              .25, .5, .75, .9)
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type) 
      tract_spatialdf@data$Density <- 
        tract_spatialdf@data$Prev <- 0
      
      if(nrow(hh_size_tmp) != 0){
        poly_order.idx <- match(tract_spatialdf@data$GEOID,
                                hh_size_tmp$GEOID)
        tract_spatialdf@data$Prev[poly_order.idx] <-
          hh_size_tmp$Prev
        tract_spatialdf@data$Density[poly_order.idx] <-
          hh_size_tmp$Density
        prev.int.tract <- classIntervals(tract_spatialdf@data$Prev,
                                         style = "fixed",
                                         fixedBreaks = breaks,
                                         n = 9)
        prev.col.tract <- findColours(prev.int.tract, prev.pal)
      }else{
        prev.int.tract$var <- rep(0, nrow(tract_spatialdf@data))
        prev.col.tract <- findColours(prev.int.tract, prev.pal)
      }
      
      
      
      jpeg(paste0("../household_size/Tract/Prevalence/", 
                  "Households_Size",
                  size, "_",
                  tenure.type, "_Prevalence_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = prev.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        legend('bottomleft',
               title = 'Prevalence',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.tract, 'table')))
        title(paste0("Prevalence of ",
                     tenure.type,
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string,
                     " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      
      jpeg(paste0("../household_size/Tract/Prevalence/", 
                  "Households_Size",
                  size, "_",
                  tenure.type, 
                  "_Prevalence_CoV_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = prev.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        
        hatch.idx <- which(tract_spatialdf@data$Density > 0)
        for(poly in hatch.idx){
          points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = tract_spatialdf@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Prevalence',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.tract, 'table')))
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        title(paste0("Prevalence of ",
                     tenure.type, 
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string,
                     " (Estimated from ",
                     legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      # End tenure loop
    }
    # End size loop
  }
  
  ### Distribution ####
  
  breaks <- c(0, .0005, .001,
              .0025, .005, .0075,
              .01, .0125, .015, .2)
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type) %>% 
        mutate(Dist = estimate/sum(estimate))
      hh_size_tmp$Dist %>% summary() %>% print()
      tract_spatialdf@data$Density <-
        tract_spatialdf@data$Dist <- 0
      
      
      if(nrow(hh_size_tmp) != 0){
        poly_order.idx <- match(tract_spatialdf@data$GEOID,
                                hh_size_tmp$GEOID)
        tract_spatialdf@data$Dist[poly_order.idx] <-
          hh_size_tmp$Dist
        tract_spatialdf@data$Density[poly_order.idx] <-
          hh_size_tmp$Density
        prev.int.tract <- classIntervals(tract_spatialdf@data$Dist,
                                         style = "fixed",
                                         fixedBreaks = breaks,
                                         n = 9)
        prev.col.tract <- findColours(prev.int.tract, prev.pal)
      }else{
        prev.int.tract$var <- rep(0, nrow(tract_spatialdf@data))
        prev.col.tract <- findColours(prev.int.tract, prev.pal)
      }
      
      jpeg(paste0("../household_size/Tract/Distribution/", 
                  "Households_Size",
                  size, "_",
                  tenure.type, "_Distribution_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = prev.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        
        legend('bottomleft',
               title = 'Distribution',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.tract, 'table')))
        title(paste0("Distribution of ",
                     tenure.type, 
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string, " (Estimated from ", legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      
      ### Add CoV ####
      jpeg(paste0("../household_size/Tract/Distribution/", 
                  "Households_Size",
                  size, "_",
                  tenure.type,"_Distribution_CoV_", year, ".jpeg"),
           height = 480, width = 480)
      {
        par(lend = 1,
            mar = c(0,0,2,0),
            oma = c(1,1,1,1))
        plot(tract_spatialdf,
             col = prev.col.tract,
             border = 'grey48', lwd = .25,
             main = "")
        hatch.idx <- which(tract_spatialdf@data$Density > 0)
        for(poly in hatch.idx){
          points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
          polygon(points[,1], points[,2],
                  border = FALSE,
                  density = tract_spatialdf@data$Density[poly])
        }
        legend('bottomleft',
               title = 'Distribution',
               title.adj = 0,
               ncol = 2,
               bty = 'n',
               cex = 0.75,
               border = FALSE,
               fill = prev.pal,
               legend = names(attr(prev.col.tract, 'table')))
        legend('bottomright',
               title = 'Significance',
               title.adj = 0,
               ncol = 1,
               bty = 'n',
               cex= 0.75,
               border = 'black',
               fill = 'black',
               density = c(0,25,50),
               legend = c(">= 95%",
                          "80% to 90%",
                          "< 80%"))
        title(paste0("Distribution of ",
                     tenure.type, 
                     " Households of Size ", size, "\n",
                     ""),
              font.main = 2, outer = FALSE,
              adj = 0, cex.main = 1)
        
        title(paste0("\n",
                     title_string,
                     " (Estimated from ",
                     legend_string, ")"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
      # End tenure loop
    }
    # End size dist loop
  }
  # End year loop 
}
