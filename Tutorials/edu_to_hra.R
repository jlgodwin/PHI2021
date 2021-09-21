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
library(readxl)

# Clear environment ####
rm(list = ls())

## tidycensus key ####
source('tidycensus_APIkey.R')
census_api_key(myKey) 

## tidycensus shapefile cache ####
options(tigris_use_cache = TRUE)

# Load data ####
## Educational Attainment ####

edu_data <- read.csv("../Data/edu_attainment_data.csv")

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

# Aggregate to HRA ####


## Census 2010 and later ####
names(edu_data)
edu_data_hra_2010 <- edu_data %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(Year >= 2010) %>% 
  filter(GEOID %in% kc_tracts$GEOID) %>% 
  mutate(SE = moe/qnorm(.9)) %>% 
  pivot_longer(cols = c("less_than_hs",
                        "hs_grad",
                        "some_college",
                        "college_grad"),
               names_to = "Education",
               values_to = "Educ_Val") %>% 
  filter(Educ_Val == 1) %>% 
  left_join(tracts_to_hra$acs5_2019,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20,
          variable, Education) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_,
           variable, Education) %>% 
  summarise(estimate = sum(prop.area*estimate),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/estimate)

## Census 2000 and later ####
names(edu_data)
edu_data_hra_2000 <- edu_data %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(Year < 2010) %>% 
  filter(GEOID %in% kc_tracts$GEOID) %>% 
  mutate(SE = moe/qnorm(.9)) %>% 
  pivot_longer(cols = c("less_than_hs",
                        "hs_grad",
                        "some_college",
                        "college_grad"),
               names_to = "Education",
               values_to = "Educ_Val") %>% 
  filter(Educ_Val == 1) %>%  
  left_join(tracts_to_hra$acs5_2009,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20,
          variable, Education) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_,
           variable, Education) %>% 
  summarise(estimate = sum(prop.area*estimate),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/estimate)

## Combine both ####
edu_data_hra <- bind_rows(edu_data_hra_2010,
                          edu_data_hra_2000) %>% 
  arrange(Year, FID_HRA_20, Education) %>% 
  filter(!is.na(estimate))

# Maps ####
if(!dir.exists('../Race and Education Data and Plots/HRA/')){
  dir.create('../Race and Education Data and Plots/HRA/')
  dir.create('../Race and Education Data and Plots/Tract/')
}


## White ####

edu_data_hra_tmp <- edu_data_hra %>% 
  filter(grepl("C15002A", variable)) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_,
           Education) %>% 
  summarise(estimate = sum(estimate),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/estimate)


# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns

range(edu_data_hra_tmp$estimate)
summary(edu_data_hra_tmp$estimate)
quantile(edu_data_hra_tmp$estimate,
         probs = seq(0,1,.1))

if(FALSE){
  pop.int.hra <- classIntervals(edu_data_hra_tmp$estimate,
                                style = 'jenks',
                                n = 9)
  
  breaks <- pop.int.hra$brks
  breaks
}


for(yr in c(2019, 2014, 2009)){
  
  
  for(edu_lvl in unique(edu_data_hra$Education)){
    ## Make sure your df is in right order for mapping,
    ## i.e. edu_data_hra_tmp is ordered like hra@data
    
    if(edu_lvl == "less_than_hs"){
      title_string <- "Less than HS"
    }else if(edu_lvl == "hs_grad"){
      title_string <- "High school"
    }else if(edu_lvl == "some_college"){
      title_string <- "Some college"
    }else if(edu_lvl == "college_grad"){
      title_string <- "College"
    }
    
    edu_lvl_tmp <- edu_data_hra_tmp %>% 
      filter(Year == yr) %>% 
      filter(Education == edu_lvl) 
    
    edu_lvl_tmp <- edu_lvl_tmp[match(hra@data$HRA2010v2_,
                                     edu_lvl_tmp$HRA2010v2_), ]
    
    ### Population ####
    
    
    # Grab palette
    pop.pal <- brewer.pal(n = 9, name = "Blues")
    
    ## Define breaks
    breaks <- c(0, 50, 100, 
                500, 1000, 2500,
                5000, 10000, 20000,
                32500)
    
    pop.int.hra <- classIntervals(edu_lvl_tmp$estimate,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    jpeg(paste0("../Race and Education Data and Plots/HRA/",
                edu_lvl, "_White_",
                yr, ".jpeg"),
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
           cex= 0.75,
           border = FALSE,
           fill = pop.pal,
           legend = names(attr(pop.col.hra, 'table')))
    title(paste0("Population with ",
                 title_string, " Education\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "White, (ACS ",
                 yr - 5, "-", yr,")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
    #### Tract ####
    edu_data_tmp <- edu_data  %>% 
      mutate(GEOID = as.character(GEOID)) %>% 
      filter(GEOID %in% kc_tracts$GEOID) %>% 
      mutate(SE = moe/qnorm(.9)) %>% 
      pivot_longer(cols = c("less_than_hs",
                            "hs_grad",
                            "some_college",
                            "college_grad"),
                   names_to = "Education",
                   values_to = "Educ_Val") %>% 
      filter(Educ_Val == 1) %>% 
      filter(grepl("C15002A", variable)) %>% 
      filter(Year == yr) %>% 
      filter(Education == edu_lvl) %>% 
      group_by(GEOID, Education) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      mutate(CoV = SE/estimate,
             Density = 0)
    
    # less than 80% significance
    edu_data_tmp$Density[edu_data_tmp$CoV < 1 &
                           edu_data_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    edu_data_tmp$Density[edu_data_tmp$CoV < 1/qnorm(.9) &
                           edu_data_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    if(yr < 2010){
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
    edu_data_tmp <- edu_data_tmp[match(edu_data_tmp$GEOID,
                                       tract_spatialdf$GEOID),]
    ## Bin with fixed colors
    ## Define breaks
    
    breaks <- c(0, 50, 100, 
                500, 1000, 2500,
                5000, 7500, 10000,
                14000)
    pop.int.tract <- classIntervals(edu_data_tmp$estimate,
                                    style = "fixed",
                                    fixedBreaks = breaks,
                                    n = 9)
    pop.col.tract <- findColours(pop.int.tract, pop.pal)
    
    jpeg(paste0("../Race and Education Data and Plots/Tract/",
                edu_lvl, "_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(0,0,1,0))
      plot(tract_spatialdf,
           col = pop.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      hatch.idx <- which(edu_data_tmp$Density > 0)
      for(poly in hatch.idx){
        points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = edu_data_tmp$Density[poly])
      }
      legend('bottomleft',
             title = 'Population',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 0.75,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.tract, 'table')))
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
      title(paste0("Population with ",
                   title_string, " Education\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "White, (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    # End edu loop
    
    ### Prevalence ####
    hra_total <- edu_data_hra %>% 
      group_by(Year, FID_HRA_20, HRA2010v2_) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      mutate(CoV = SE/estimate)
    
    edu_prev_tmp <- edu_lvl_tmp %>% 
      left_join(hra_total,
                by = c("Year" = "Year",
                       "FID_HRA_20" = "FID_HRA_20",
                       "HRA2010v2_" = "HRA2010v2_"),
                suffix = c("", "_Total")) %>% 
      mutate(Prev = estimate/estimate_Total)
    
    # Grab palette
    prev.pal <- brewer.pal(n = 9, name = "YlGnBu")
    
    ## Define breaks
    breaks <- c(0, .01, .025, .05, 
                .075, .1, .15,
                .25, .5, .75)
    
    prev.int.hra <- classIntervals(edu_prev_tmp$Prev,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    prev.col.hra <- findColours(prev.int.hra, prev.pal)
    
    jpeg(paste0("../Race and Education Data and Plots/HRA/",
                edu_lvl, "_Prevalence_White_",
                yr, ".jpeg"),
         height = 480, width = 480)
    par(lend = 1,
        mar = c(0,0,2,0),
        oma = c(0,0,1,0))
    plot(hra,
         col = prev.col.hra,
         border = 'grey48', lwd = .25,
         main = "")
    legend('bottomleft',
           title = 'Prevalence',
           title.adj = 0,
           ncol = 2,
           bty = 'n',
           cex= 0.75,
           border = FALSE,
           fill = pop.pal,
           legend = names(attr(pop.col.hra, 'table')))
    title(paste0("Prevalence of Population with ",
                 title_string, " Education\n",
                 ""),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 1)
    
    title(paste0("\n",
                 "White, (ACS ",
                 yr - 5, "-", yr,")"),
          font.main = 1, outer = FALSE,
          adj = 0, cex.main = 1)
    dev.off()
    
    #### Tract ####
    
    tract_total <- edu_data %>% 
      mutate(SE = moe/qnorm(.95)) %>% 
      group_by(Year, GEOID) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      mutate(CoV = SE/estimate)
    
    edu_prev_tmp <- edu_data  %>% 
      mutate(GEOID = as.character(GEOID)) %>% 
      filter(GEOID %in% kc_tracts$GEOID) %>% 
      mutate(SE = moe/qnorm(.9)) %>% 
      pivot_longer(cols = c("less_than_hs",
                            "hs_grad",
                            "some_college",
                            "college_grad"),
                   names_to = "Education",
                   values_to = "Educ_Val") %>% 
      filter(Educ_Val == 1) %>% 
      filter(grepl("C15002A", variable)) %>% 
      filter(Year == yr) %>% 
      filter(Education == edu_lvl) %>% 
      group_by(Year, GEOID, Education) %>% 
      summarise(estimate = sum(estimate),
                SE = sum(SE)) %>% 
      mutate(CoV = SE/estimate,
             Density = 0,
             GEOID = as.numeric(GEOID))%>% 
      left_join(tract_total,
                by = c("Year" = "Year",
                       "GEOID" = "GEOID"),
                suffix = c("", "_Total")) %>% 
      mutate(Prev = estimate/estimate_Total)
    
    # less than 80% significance
    edu_prev_tmp$Density[edu_prev_tmp$CoV < 1 &
                           edu_prev_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    edu_prev_tmp$Density[edu_prev_tmp$CoV < 1/qnorm(.9) &
                           edu_prev_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    if(yr < 2010){
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
    edu_prev_tmp <- edu_prev_tmp[match(tract_spatialdf$GEOID,
                                       edu_prev_tmp$GEOID),]
    ## Bin with fixed colors
    ## Define breaks
    
    breaks <- c(0, .01, .025, .05, 
                .075, .1, .15,
                .25, .5, .75)
    
    prev.int.tract <- classIntervals(edu_prev_tmp$Prev,
                                    style = "fixed",
                                    fixedBreaks = breaks,
                                    n = 9)
    prev.col.tract <- findColours(prev.int.tract, prev.pal)
    
    jpeg(paste0("../Race and Education Data and Plots/Tract/",
                edu_lvl, "_Prevalence_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,2,0),
          oma = c(0,0,1,0))
      plot(tract_spatialdf,
           col = prev.col.tract,
           border = 'grey48', lwd = .25,
           main = "")
      hatch.idx <- which(edu_prev_tmp$Density > 0)
      for(poly in hatch.idx){
        points <- tract_spatialdf@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = FALSE,
                density = edu_prev_tmp$Density[poly])
      }
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 0.75,
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
      title(paste0("Prevalence of Population with ",
                   title_string, " Education\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
                   "White, (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    # End edu loop
  }
  # End year loop
}



