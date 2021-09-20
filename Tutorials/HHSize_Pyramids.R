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


## HRA ####
hra <- readOGR(dsn = "../Data",
               layer = "HRA_2010Block_Clip")

hra <- spTransform(hra,
                   kc_tracts_poly@proj4string)

## tracts_to_hra ####
load('../Data/tracts_to_hra.rda')

# Household Size ####

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
                   hra.name, " (Estimated from ACS)"),
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
                 "King County (Estimated from ACS)"),
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
                   "King County (Estimated from Census, ACS)"),
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
                   "King County (Estimated from Census, ACS)"),
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
                     hra.name, " (Estimated from Census, ACS)"),
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
                     hra.name, " (Estimated from Census, ACS)"),
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
      if(year != 2000){
        legend_string <- paste0("ACS ", 
                                year - 4, 
                                "-", year)
        title_string <- "ACS"
      }else{
        legend_string <- paste0("Census ", year)
        title_string <- "Census"
      }
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
                   hra.name, " (Estimated from ",
                   title_string, ")"),
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
      title_string <- "ACS"
    }else{
      legend_string <- paste0("Census ", year)
      title_string <- "Census"
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
                 hra.name, " (Estimated from ",
                 title_string, ")"),
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
               "King County (Estimated from Census, ACS)"),
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
               "King County (Estimated from Census, ACS)"),
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
                 hra.name, " (Estimated from Census, ACS)"),
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
    title(paste0("\n", hra.name, " (Estimated from Census, ACS)"),
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
               "King County (Estimated from Census, ACS)"),
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


## Maps ####
### Household Size ####
for(year in c(2010, 2014, 2019)){
  
  hh_year_tmp <- hh_size_hra %>% 
    filter(Year == year) %>% 
    filter(hh_size > 0) %>% 
    filter(!is.na(HRA2010v2_)) %>% 
    group_by(FID_HRA_20, HRA2010v2_, hh_size) %>% 
    summarise(estimate = sum(estimate),
              SE = sum(SE)) %>% 
    ungroup() %>% 
    arrange(FID_HRA_20, hh_size) %>% 
    mutate(CoV = SE/estimate)
  
  hh_year_total <- hh_year_tmp %>% 
    group_by(FID_HRA_20, HRA2010v2_) %>% 
    summarise(estimate = sum(estimate),
              SE = sum(SE)) %>% 
    ungroup() %>% 
    arrange(FID_HRA_20) %>% 
    mutate(CoV = SE/estimate)
  
  #### Households ####
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
    hra@data$Est <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Est[match(hh_size_tmp$FID_HRA_20,
                         hra@data$FID_HRA_20)] <- hh_size_tmp$estimate
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
                   "Estimated from Census and ACS"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  
  #### Prevalence ####
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
              .4, .5, .6, .7)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size)
    
    hra@data$Prev <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Prev[match(hh_size_tmp$FID_HRA_20,
                          hra@data$FID_HRA_20)] <- hh_size_tmp$Prev
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
             fill = hh.pal,
             legend = names(attr(hh.col.hra, 'table')))
      title(paste0("Prevalence of Households of Size ", size, "\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "Estimated from Census and ACS"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
  #### Distribution ####
  breaks <- c(0, .005, .01,
              .02, .025, .03,
              .045, .06, .075, .1)
  
  for(size in unique(hh_year_tmp$hh_size)){
    
    hh_size_tmp <- hh_year_tmp %>% 
      filter(hh_size == size) %>% 
      mutate(Dist = estimate/sum(estimate))
    hh_size_tmp$Dist %>% summary() %>% print()
    
    hra@data$Dist <- 0
    
    if(nrow(hh_size_tmp) != 0){
      hra@data$Dist[match(hh_size_tmp$FID_HRA_20,
                          hra@data$FID_HRA_20)] <- hh_size_tmp$Dist
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
                   "Estimated from Census and ACS"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
  
}

### Household Size by Tenure ####
for(year in c(2010, 2014, 2019)){
  
  hh_year_tmp <- hh_size_hra %>% 
    filter(Year == year) %>% 
    filter(hh_size > 0) %>% 
    filter(!is.na(HRA2010v2_)) %>% 
    group_by(FID_HRA_20, HRA2010v2_, hh_size, tenure) %>% 
    summarise(estimate = sum(estimate),
              SE = sum(SE)) %>% 
    ungroup() %>% 
    arrange(FID_HRA_20, hh_size) %>% 
    mutate(CoV = SE/estimate)
  
  hh_year_total <- hh_year_tmp %>% 
    group_by(FID_HRA_20, HRA2010v2_) %>% 
    summarise(estimate = sum(estimate),
              SE = sum(SE)) %>% 
    ungroup() %>% 
    arrange(FID_HRA_20) %>% 
    mutate(CoV = SE/estimate)
  
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
      hra@data$Est <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Est[match(hh_size_tmp$FID_HRA_20,
                           hra@data$FID_HRA_20)] <- hh_size_tmp$estimate
        
        
        
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
                     "Estimated from Census and ACS"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  
  
  #### Prevalence ####
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
              .4, .5, .6, .7)
  ## Get color based on RColorBrwere palette for 
  ## each area
  
  
  for(size in unique(hh_year_tmp$hh_size)){
    for(tenure.type in c("Renter", "Owner")){
      hh_size_tmp <- hh_year_tmp %>% 
        filter(hh_size == size &
                 tenure == tenure.type)
      
      hra@data$Prev <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Prev[match(hh_size_tmp$FID_HRA_20,
                            hra@data$FID_HRA_20)] <- hh_size_tmp$Prev
        
        
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
                     "Estimated from Census and ACS"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  
  #### Distribution ####
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
      
      hra@data$Dist <- 0
      
      if(nrow(hh_size_tmp) != 0){
        hra@data$Dist[match(hh_size_tmp$FID_HRA_20,
                            hra@data$FID_HRA_20)] <- hh_size_tmp$Dist
        
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
                     "Estimated from Census and ACS"),
              font.main = 1, outer = FALSE,
              adj = 0, cex.main = 1)
      }
      dev.off()
    }
  }
  
}
