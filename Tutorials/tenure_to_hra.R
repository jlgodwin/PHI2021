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
## Household Tenure ####

tenure <- read.csv('../Data/tract_RO_population.csv')

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
names(tenure)
tenure_hra_2010 <- tenure %>% 
  dplyr::select(GEOID, Year,
                value, SE,
                short_label) %>% 
  mutate(GEOID = as.character(GEOID),
         tenure = gsub(" occupied", "",
                       short_label)) %>% 
  filter(Year >= 2010) %>% 
  filter(GEOID %in% kc_tracts$GEOID) %>% 
  left_join(tracts_to_hra$acs5_2019,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20, tenure) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_, tenure) %>% 
  summarise(value = sum(prop.area*value),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/value)

## Census 2000 and later ####
names(tenure)
tenure_hra_2000 <- tenure %>% 
  dplyr::select(GEOID, Year,
                value, SE,
                short_label) %>% 
  mutate(GEOID = as.character(GEOID),
         tenure = gsub(" occupied", "",
                       short_label)) %>% 
  filter(Year < 2010) %>% 
  filter(GEOID %in% kc_tracts_2000$GEOID) %>% 
  left_join(tracts_to_hra$acs5_2009,
            by = c("GEOID" = "GEOID"),
            suffix = c("", "_toHRA")) %>% 
  ungroup() %>% 
  arrange(Year, FID_HRA_20, tenure) %>% 
  group_by(Year, FID_HRA_20, HRA2010v2_, tenure) %>% 
  summarise(value = sum(prop.area*value),
            SE = sum(prop.area*SE)) %>% 
  mutate(CoV = SE/value)

## Combine both ####
tenure_hra <- bind_rows(tenure_hra_2010,
                        tenure_hra_2000) %>% 
  arrange(Year, FID_HRA_20) %>% 
  filter(!is.na(value))

# Maps ####
impact_areas <- c("Burien", "Kent-SE", "Kent-West",
                  "SeaTac/Tukwila", "Beacon/Gtown/S.Park",
                  "Fed Way-Central/Military Rd", "NW Seattle")

hra@data$ImpactArea <- hra@data$HRA2010v2_ %in% impact_areas

## Population ####

if(!dir.exists('../TenurePlots/')){
  dir.create('../TenurePlots/')
}

# Grab palette
pop.pal <- brewer.pal(n = 9, name = "Blues")

# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns
range(tenure_hra$value)
summary(tenure_hra$value)
quantile(tenure_hra$value,
         probs = seq(0,1,.1))
pop.int.hra <- classIntervals(tenure_hra$value,
                              style = 'jenks',
                              n = 9)

breaks <- pop.int.hra$brks
breaks

## Define breaks


for(yr in c(2009, 2014, 2018)){
  for(tenure.type in c("Renter", "Owner")){
    ## Make sure your df is in right order for mapping,
    ## i.e. tenure_hra_tmp is ordered like hra@data
    
    tenure_hra_tmp <- tenure_hra %>% 
      filter(Year == yr) %>% 
      filter(tenure == tenure.type) 
    
    tenure_hra_tmp <- tenure_hra_tmp[match(hra@data$HRA2010v2_,
                                           tenure_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    breaks <- c(0, 2500, 5000,
                7500, 10000, 20000,
                30000, 40000, 50000,
                75000)
    pop.int.hra <- classIntervals(tenure_hra_tmp$value,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    jpeg(paste0("../TenurePlots/Map_HRA_",
                tenure.type, "_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0(tenure.type, "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    jpeg(paste0("../TenurePlots/Map_HRA_ImpactOutline",
                tenure.type, "_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0(tenure.type, "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    ### Tract ####
    tenure_tmp <- tenure %>% 
      filter(Year == yr) %>% 
      mutate(tenure = gsub(" occupied", "",
                           short_label)) %>% 
      filter(tenure == tenure.type) %>% 
      mutate(Density = 0) 
    
    # less than 80% significance
    tenure_tmp$Density[tenure_tmp$CoV >= 1/qnorm(.9)] <- 50
    
    # between 80 and 90%
    tenure_tmp$Density[tenure_tmp$CoV < 1/qnorm(.9) &
                         tenure_tmp$CoV >= 1/qnorm(.95)] <- 25
    
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
    tenure_tmp <- tenure_tmp %>% 
      filter(GEOID %in% tract_spatialdf$GEOID)
    tenure_tmp <- tenure_tmp[match(tract_spatialdf$GEOID,
                                   tenure_tmp$GEOID),]
    ## Bin with fixed colors
    breaks <- c(0, 500, 1000,
                2000, 3000, 4000,
                5000, 7500, 10000,
                14000)
    pop.int.hra <- classIntervals(tenure_tmp$value,
                                  style = "fixed",
                                  fixedBreaks = breaks,
                                  n = 9)
    pop.col.hra <- findColours(pop.int.hra, pop.pal)
    
    jpeg(paste0("../TenurePlots/Map_Tract_",
                tenure.type, "_CoV_",
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
             title = 'Significance',
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
      title(paste0(tenure.type, "-occupied Households\n",
                   ""),
            font.main = 2, outer = FALSE,
            adj = 0, cex.main = 1)
      
      title(paste0("\n",
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
head(tenure_hra)

## First get total households by HRA
total_hh_hra <- tenure_hra %>% 
  group_by(Year, FID_HRA_20,
           HRA2010v2_) %>% 
  summarise(value = sum(value),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/value)
head(total_hh_hra)

## Calc prev
tenure_prev_hra <- tenure_hra %>% 
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

range(tenure_prev_hra$Prev)
summary(tenure_prev_hra$Prev)
quantile(tenure_prev_hra$Prev,
         probs = seq(0,1,.1))

prop.int.hra <- classIntervals(tenure_prev_hra$Prev,
                               style = 'jenks',
                               n = 9)

breaks <- prop.int.hra$brks
breaks

## Define breaks
breaks <- c(0, .05, .15, .2,
            .3, .4, .5, 
            .75, .85,
            1)



for(yr in c(2009, 2014, 2018)){
  for(tenure.type in c("Renter", "Owner")){
    ## Make sure your df is in right order for mapping,
    ## i.e. tenure_hra_tmp is ordered like hra@data
    
    tenure_prev_hra_tmp <- tenure_prev_hra %>% 
      filter(Year == yr) %>% 
      filter(tenure == tenure.type)
    
    tenure_prev_hra_tmp <- tenure_prev_hra_tmp[match(hra@data$HRA2010v2_,
                                                     tenure_prev_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    prop.int.hra <- classIntervals(tenure_prev_hra_tmp$Prev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../TenurePlots/Map_HRA_",
                tenure.type, "_Prevalence_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0("Prevalence of ",
                 tenure.type,
                 "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    jpeg(paste0("../TenurePlots/Map_HRA_",
                tenure.type, "_Prevalence_ImpactOutline",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0("Prevalence of ",
                 tenure.type,
                 "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    
    ### Tract ####
    tenure_tract_total <- tenure %>% 
      filter(Year == yr) %>% 
      mutate(tenure = gsub(" occupied", "",
                           short_label)) %>% 
      group_by(GEOID) %>% 
      summarise(value = sum(value),
                SE = sum(SE)) 
    tenure_tmp <- tenure %>% 
      filter(Year == yr) %>% 
      mutate(tenure = gsub(" occupied", "",
                           short_label)) %>% 
      filter(tenure == tenure.type) %>%
      left_join(tenure_tract_total,
                by = c("GEOID" = "GEOID"),
                suffix = c("", "_TotalHH")) %>% 
      mutate(Density = 0,
             Prev = value/value_TotalHH) 
    tenure_tmp$Density[tenure_tmp$CoV >= 1/qnorm(.9)] <- 50
    tenure_tmp$Density[tenure_tmp$CoV < 1/qnorm(.9) &
                         tenure_tmp$CoV >= 1/qnorm(.95)] <- 25
    
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
    tenure_tmp <- tenure_tmp %>% 
      filter(GEOID %in% tract_spatialdf$GEOID)
    
    tenure_tmp <- tenure_tmp[match(tract_spatialdf$GEOID,
                                   tenure_tmp$GEOID),]
    ## Bin with fixed colors
    prop.int.hra <- classIntervals(tenure_tmp$Prev,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../TenurePlots/Map_Tract_",
                tenure.type, "_Prevalence_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
           title = 'Significance',
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
    title(paste0("Prevalence of ",
                 tenure.type, "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
  }
}

## Distribution ####

# Calculate distribution
head(tenure_hra)

## First get total households by HRA
total_hh_tenure <- tenure_hra %>% 
  group_by(Year, tenure) %>% 
  summarise(value = sum(value),
            SE = sum(SE)) %>% 
  mutate(CoV = SE/value)
head(total_hh_tenure)

## Calc prev
tenure_dist_hra <- tenure_hra %>% 
  left_join(total_hh_tenure,
            by = c("Year" = "Year",
                   "tenure" = "tenure"),
            suffix = c("", "_TotalHH")) %>% 
  mutate(Dist = value/value_TotalHH)

## Check work
tenure_dist_hra %>% 
  group_by(Year, tenure) %>% 
  summarise(SumDist = sum(Dist)) 

# Grab palette
prop.pal <- brewer.pal(n = 9, name = "YlGnBu")

# Use 'jenks' auto bin to get an idea of bin breaks
# Plus other summary fns

range(tenure_dist_hra$Dist)
summary(tenure_dist_hra$Dist)
quantile(tenure_dist_hra$Dist,
         probs = seq(0,1,.1))

prop.int.hra <- classIntervals(tenure_dist_hra$Dist,
                               style = 'jenks',
                               n = 9)

breaks <- prop.int.hra$brks
breaks



for(yr in c(2009, 2014, 2018)){
  for(tenure.type in c("Renter", "Owner")){
    ## Make sure your df is in right order for mapping,
    ## i.e. tenure_hra_tmp is ordered like hra@data
    
    tenure_dist_hra_tmp <- tenure_dist_hra %>% 
      filter(Year == yr) %>% 
      filter(tenure == tenure.type)
    
    tenure_dist_hra_tmp <- tenure_dist_hra_tmp[match(hra@data$HRA2010v2_,
                                                     tenure_dist_hra_tmp$HRA2010v2_), ]
    
    ## Bin with fixed colors
    ## Define breaks
    breaks <- c(0.002, .005, .0075,
                .01, .02, .025, 
                .03, .04, .05,
                .06)
    prop.int.hra <- classIntervals(tenure_dist_hra_tmp$Dist,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../TenurePlots/Map_HRA_",
                tenure.type, "_Distribution_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0("Distribution of ",
                 tenure.type,
                 "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    jpeg(paste0("../TenurePlots/Map_HRA_",
                tenure.type, "_Distribution_ImpactOutline",
                yr, ".jpeg"),
         height = 480, width = 480)
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
    title(paste0("Distribution of ",
                 tenure.type,
                 "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
    
    
    
    
    ### Tract ####
    tenure_tract_total <- tenure %>% 
      filter(Year == yr) %>% 
      mutate(tenure = gsub(" occupied", "",
                           short_label)) %>% 
      group_by(tenure) %>% 
      summarise(value = sum(value),
                SE = sum(SE)) 
    tenure_tmp <- tenure %>% 
      filter(Year == yr) %>% 
      mutate(tenure = gsub(" occupied", "",
                           short_label)) %>% 
      filter(tenure == tenure.type) %>%
      left_join(tenure_tract_total,
                by = c("tenure" = "tenure"),
                suffix = c("", "_TotalHH")) %>% 
      mutate(Density = 0,
             Dist = value/value_TotalHH) 
    tenure_tmp$Density[tenure_tmp$CoV >= 1/qnorm(.9)] <- 50
    tenure_tmp$Density[tenure_tmp$CoV < 1/qnorm(.9) &
                         tenure_tmp$CoV >= 1/qnorm(.95)] <- 25
    
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
    tenure_tmp <- tenure_tmp %>% 
      filter(GEOID %in% tract_spatialdf$GEOID)
    tenure_tmp <- tenure_tmp[match(tract_spatialdf$GEOID,
                                   tenure_tmp$GEOID),]
    ## Bin with fixed colors
    breaks <- c(0, .001,
                .002, .003,
                .004, .005,
                .0075, .01,
                .0125)
    prop.int.hra <- classIntervals(tenure_tmp$Dist,
                                   style = "fixed",
                                   fixedBreaks = breaks,
                                   n = 9)
    prop.col.hra <- findColours(prop.int.hra, prop.pal)
    
    jpeg(paste0("../TenurePlots/Map_Tract_",
                tenure.type, "_Distribution_CoV_",
                yr, ".jpeg"),
         height = 480, width = 480)
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
           title = 'Significance',
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
    title(paste0("Distribution of ",
                 tenure.type, "-occupied Households"),
          font.main = 2, outer = FALSE,
          adj = 0, cex.main = 2)
    
    # title(paste0("\n",
    #              "King County (ACS ",
    #              yr - 5, "-", yr,")"),
    #       font.main = 1, outer = FALSE,
    #       adj = 0, cex.main = 1)
    dev.off()
  }
}


