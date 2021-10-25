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
myKey <- "493b01690c601ceeadbfc1bfc0089bae12b3f476"
census_api_key(myKey) 

## tidycensus shapefile cache ####
options(tigris_use_cache = TRUE)

# Load data ####
## Rent Burden ####

burden <- read.csv('../Data/tract_rent_burden_pop.csv')

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
names(burden)
burden_hra_2010 <- burden %>% 
  dplyr::select(GEOID, Year,
                value, SE,
                variable) %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename("burden" = "variable") %>% 
  filter(Year >= 2010) %>% 
  filter(GEOID %in% kc_tracts$GEOID) %>% 
  left_join(tracts_to_hra$acs5_2019,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20, burden) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_, burden) %>% 
  summarise(value = sum(prop.area*value),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/value)

## Census 2000 and later ####
names(burden)
burden_hra_2000 <- burden %>% 
  dplyr::select(GEOID, Year,
                value, SE,
                variable) %>% 
  rename("burden" = "variable") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(Year < 2010) %>% 
  filter(GEOID %in% kc_tracts_2000$GEOID) %>% 
  left_join(tracts_to_hra$acs5_2009,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20, burden) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_, burden) %>% 
  summarise(value = sum(prop.area*value),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/value)

## Combine both ####
burden_hra <- bind_rows(burden_hra_2010,
                        burden_hra_2000) %>% 
  arrange(Year, FID_HRA_20) %>% 
  filter(!is.na(value))

# Maps ####
impact_areas <- c("Burien", "Kent-SE", "Kent-West",
                  "SeaTac/Tukwila", "Beacon/Gtown/S.Park",
                  "Fed Way-Central/Military Rd", "NW Seattle")

hra@data$ImpactArea <- hra@data$HRA2010v2_ %in% impact_areas

## Population ####

if(!dir.exists('../burdenPlots/')){
  dir.create('../burdenPlots/')
  dir.create('../burdenPlots/HRA/')
  dir.create('../burdenPlots/HRA/Population/')
  dir.create('../burdenPlots/HRA/Prevalence/')
  dir.create('../burdenPlots/HRA/Distribution/')
  dir.create('../burdenPlots/Tract/')
  dir.create('../burdenPlots/Tract/Population/')
  dir.create('../burdenPlots/Tract/Prevalence/')
  dir.create('../burdenPlots/Tract/Distribution/')
}

# Grab palette
pop.pal <- brewer.pal(n = 9, name = "Blues")

# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns
range(burden_hra$value)
summary(burden_hra$value)
quantile(burden_hra$value,
         probs = seq(0,1,.1))
pop.int.hra <- classIntervals(burden_hra$value,
                              style = 'jenks',
                              n = 9)

breaks <- pop.int.hra$brks
breaks

## Define breaks


for(yr in c(2009, 2014, 2019)){
  for(burden.type in c("low_burden",
                       "severely_burdened",
                       "highly_burdened")){
    ## Make sure your df is in right order for mapping,
    ## i.e. burden_hra_tmp is ordered like hra@data
    
    if(burden.type == "low_burden"){
      legend_string <- "Low Rent-burdened"
    }else if(burden.type == "severely_burdened"){
      legend_string <- "Severely Rent-burdened"
    }else if(burden.type == "highly_burdened"){
      legend_string <- "High Rent-burdened"
    }
    burden_hra_tmp <- burden_hra %>% 
      filter(Year == yr) %>% 
      filter(burden == burden.type) 
    
    burden_hra_tmp <- burden_hra_tmp[match(hra@data$HRA2010v2_,
                                           burden_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    breaks <- c(0, 250, 500,
                1000, 2500, 5000,
                7500, 10000, 15000,
                20000)
    pop.int.hra <- classIntervals(burden_hra_tmp$value,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    jpeg(paste0("../burdenPlots/HRA/Population/",
                "Map_HRA_",
                burden.type, "_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(hra,
           col = pop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.hra, 'table')))
      title(paste0("Counts of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    jpeg(paste0("../burdenPlots/HRA/Population/",
                "Map_HRA_ImpactOutline",
                burden.type, "_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(hra,
           col = pop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white', lwd = 2)
      }
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.hra, 'table')))
      title(paste0("Counts of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1.5)
    }
    dev.off()
    
    ### Tract ####
    
    
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
    
    burden_tmp <- burden %>%
      filter(Year == yr) %>%
      mutate(GEOID = as.character(GEOID)) %>%
      filter(GEOID %in% tract_spatialdf$GEOID) %>% 
      rename("burden" = "variable") %>% 
      filter(burden == burden.type) %>%
      mutate(Density = 0)
    
    # less than 80% significance
    burden_tmp$Density[burden_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    burden_tmp$Density[burden_tmp$CoV < 1/qnorm(.9) &
                         burden_tmp$CoV >= 1/qnorm(.95)] <- 25
    
    burden_tmp <- burden_tmp[match(tract_spatialdf$GEOID,
                                   burden_tmp$GEOID),]
    ## Bin with fixed colors
    breaks <- c(0, 500, 1000,
                2000, 3000, 4000,
                5000, 7500, 10000,
                14000)
    pop.int.hra <- classIntervals(burden_tmp$value,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    jpeg(paste0("../burdenPlots/Tract/Population/",
                "Map_Tract_",
                burden.type, "_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(tract_spatialdf,
           col = pop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white',
                lwd = 2)
      }
      legend('bottomleft',
             title = 'Households',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = pop.pal,
             legend = names(attr(pop.col.hra, 'table')))
      legend('bottomright',
             title = "Probability of \nSignificant Uncertainty",
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 1,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Counts of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
}

## Prevalence ####

# Calculate prevalence
head(burden_hra)

## First get total households by HRA
total_hh_hra <- burden_hra %>% 
  group_by(Year, FID_HRA_20,
           HRA2010v2_) %>% 
  summarise(value = sum(value),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/value)
head(total_hh_hra)

## Calc prev
burden_prev_hra <- burden_hra %>% 
  left_join(total_hh_hra,
            by = c("Year" = "Year",
                   "FID_HRA_20" = "FID_HRA_20",
                   "HRA2010v2_" = "HRA2010v2_"),
            suffix = c("", "_TotalHH")) %>% 
  mutate(Prev = value/value_TotalHH)

# Grab palette
prop.pal <- brewer.pal(n = 9, name = "YlGnBu")

# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns

range(burden_prev_hra$Prev)
summary(burden_prev_hra$Prev)
quantile(burden_prev_hra$Prev,
         probs = seq(0,1,.1))

prop.int.hra <- classIntervals(burden_prev_hra$Prev,
                               style = 'jenks',
                               n = 9)

breaks <- prop.int.hra$brks
breaks

## Define breaks
breaks <- c(0, .05, .15, .2,
            .3, .4, .5, 
            .75, .85,
            1)



for(yr in c(2009, 2014, 2019)){
  for(burden.type in c("low_burden",
                       "severely_burdened",
                       "highly_burdened")){
    ## Make sure your df is in right order for mapping,
    ## i.e. burden_hra_tmp is ordered like hra@data
    if(burden.type == "low_burden"){
      legend_string <- "Low Rent-burdened"
    }else if(burden.type == "severely_burdened"){
      legend_string <- "Severely Rent-burdened"
    }else if(burden.type == "highly_burdened"){
      legend_string <- "High Rent-burdened"
    }
    burden_prev_hra_tmp <- burden_prev_hra %>% 
      filter(Year == yr) %>% 
      filter(burden == burden.type)
    
    burden_prev_hra_tmp <- burden_prev_hra_tmp[match(hra@data$HRA2010v2_,
                                                     burden_prev_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    prop.int.hra <- classIntervals(burden_prev_hra_tmp$Prev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../burdenPlots/HRA/Prevalence/",
                "Map_HRA_",
                burden.type, "_Prevalence_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
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
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Prevalence of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    jpeg(paste0("../burdenPlots/HRA/Prevalence/",
                "Map_HRA_",
                burden.type, "_Prevalence_ImpactOutline",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white', lwd = 2)
      }
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Prevalence of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    
    ### Tract ####
    burden_tract_total <- burden %>% 
      filter(Year == yr) %>% 
      rename("burden" = "variable") %>% 
      group_by(GEOID) %>% 
      summarise(value = sum(value),
                SE = sum(SE)) 
    burden_tmp <- burden %>% 
      filter(Year == yr) %>% 
      rename("burden" = "variable") %>% 
      filter(burden == burden.type) %>%
      left_join(burden_tract_total,
                by = c("GEOID" = "GEOID"),
                suffix = c("", "_TotalHH")) %>% 
      mutate(Density = 0,
             Prev = value/value_TotalHH) 
    burden_tmp$Density[burden_tmp$CoV >= 1/qnorm(.9)] <- 50
    burden_tmp$Density[burden_tmp$CoV < 1/qnorm(.9) &
                         burden_tmp$CoV >= 1/qnorm(.95)] <- 25
    
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
    burden_tmp <- burden_tmp %>% 
      filter(GEOID %in% tract_spatialdf$GEOID)
    
    burden_tmp <- burden_tmp[match(tract_spatialdf$GEOID,
                                   burden_tmp$GEOID),]
    ## Bin with fixed colors
    prop.int.hra <- classIntervals(burden_tmp$Prev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../burdenPlots/Tract/Prevalence/",
                "Map_Tract_",
                burden.type, "_Prevalence_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(tract_spatialdf,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white',
                lwd = 2)
      }
      legend('bottomleft',
             title = 'Prevalence',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      legend('bottomright',
             title = "Probability of \nSignificant Uncertainty",
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 1,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Prevalence of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
}

## Distribution ####

# Calculate distribution
head(burden_hra)

## First get total households by HRA
total_hh_burden <- burden_hra %>% 
  group_by(Year, burden) %>% 
  summarise(value = sum(value),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/value)
head(total_hh_burden)

## Calc prev
burden_dist_hra <- burden_hra %>% 
  left_join(total_hh_burden,
            by = c("Year" = "Year",
                   "burden" = "burden"),
            suffix = c("", "_TotalHH")) %>% 
  mutate(Dist = value/value_TotalHH)

## Check work
burden_dist_hra %>% 
  group_by(Year, burden) %>% 
  summarise(SumDist = sum(Dist)) 

# Grab palette
prop.pal <- brewer.pal(n = 9, name = "YlGnBu")

# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns

range(burden_dist_hra$Dist)
summary(burden_dist_hra$Dist)
quantile(burden_dist_hra$Dist,
         probs = seq(0,1,.1))

prop.int.hra <- classIntervals(burden_dist_hra$Dist,
                               style = 'jenks',
                               n = 9)

breaks <- prop.int.hra$brks
breaks



for(yr in c(2009, 2014, 2019)){
  
  for(burden.type in c("low_burden",
                       "severely_burdened",
                       "highly_burdened")){
    ## Make sure your df is in right order for mapping,
    ## i.e. burden_hra_tmp is ordered like hra@data
    if(burden.type == "low_burden"){
      legend_string <- "Low Rent-burdened"
    }else if(burden.type == "severely_burdened"){
      legend_string <- "Severely Rent-burdened"
    }else if(burden.type == "highly_burdened"){
      legend_string <- "High Rent-burdened"
    }
    burden_dist_hra_tmp <- burden_dist_hra %>% 
      filter(Year == yr) %>% 
      filter(burden == burden.type)
    
    burden_dist_hra_tmp <- burden_dist_hra_tmp[match(hra@data$HRA2010v2_,
                                                     burden_dist_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    ## Define breaks
    breaks <- c(0, .005, .0075,
                .01, .02, .03, 
                .04, .05, .075,
                .10)
    # print(max(burden_dist_hra_tmp$Dist))
    prop.int.hra <- classIntervals(burden_dist_hra_tmp$Dist,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../burdenPlots/HRA/Distribution/Map_HRA_",
                burden.type, "_Distribution_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
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
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Distribution of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    jpeg(paste0("../burdenPlots/HRA/Distribution/Map_HRA_",
                burden.type, "_Distribution_ImpactOutline",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,3,0),
          oma = c(0,0,1,0))
      plot(hra,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white',
                lwd = 2)
      }
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      title(paste0("Distribution of Households\n",
                   legend_string),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
    
    
    ### Tract ####
    burden_tract_total <- burden %>% 
      filter(Year == yr) %>% 
      rename("burden" = "variable") %>% 
      group_by(burden) %>% 
      summarise(value = sum(value),
                SE = sum(SE)) 
    burden_tmp <- burden %>% 
      filter(Year == yr) %>% 
      rename("burden" = "variable") %>% 
      filter(burden == burden.type) %>%
      left_join(burden_tract_total,
                by = c("burden" = "burden"),
                suffix = c("", "_TotalHH")) %>% 
      mutate(Density = 0,
             Dist = value/value_TotalHH) 
    burden_tmp$Density[burden_tmp$CoV >= 1/qnorm(.9)] <- 50
    burden_tmp$Density[burden_tmp$CoV < 1/qnorm(.9) &
                         burden_tmp$CoV >= 1/qnorm(.95)] <- 25
    
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
    burden_tmp <- burden_tmp %>% 
      filter(GEOID %in% tract_spatialdf$GEOID)
    burden_tmp <- burden_tmp[match(tract_spatialdf$GEOID,
                                   burden_tmp$GEOID),]
    ## Bin with fixed colors
    breaks <- c(0, .001,
                .0025, .005,
                .0075, .01,
                .0125, 0.015,
                0.0175, .02)
    # print(max(burden_tmp$Dist))
    prop.int.hra <- classIntervals(burden_tmp$Dist,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../burdenPlots/Tract/Distribution/",
                "Map_Tract_",
                burden.type, "_Distribution_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
    {
      par(lend = 1,
          mar = c(0,0,1,0),
          oma = c(0,0,1,0))
      plot(tract_spatialdf,
           col = prop.col.hra,
           border = 'grey48', lwd = .25,
           main = "")
      for(poly in which(hra@data$ImpactArea)){
        points <- hra@polygons[[poly]]@Polygons[[1]]@coords
        polygon(points[,1], points[,2],
                border = 'white',
                lwd = 2)
      }
      legend('bottomleft',
             title = 'Distribution',
             title.adj = 0,
             ncol = 2,
             bty = 'n',
             cex= 1,
             border = FALSE,
             fill = prop.pal,
             legend = names(attr(prop.col.hra, 'table')))
      legend('bottomright',
             title = "Probability of \nSignificant Uncertainty",
             title.adj = 0,
             ncol = 1,
             bty = 'n',
             cex= 1,
             border = 'black',
             fill = 'black',
             density = c(0,25,50),
             legend = c(">= 95%",
                        "80% to 90%",
                        "< 80%"))
      title(paste0("Distribution of ", legend_string, " Households"),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1.5)
      
      title(paste0("\n\n\n",
                   "King County (ACS ",
                   yr - 5, "-", yr,")"),
            font.main = 1, outer = FALSE,
            adj = 0, cex.main = 1)
    }
    dev.off()
  }
}


