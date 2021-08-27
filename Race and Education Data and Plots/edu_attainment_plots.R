library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)
library(data.table)
library(gridExtra)
library(sp)
library(rgdal)
library(rgeos)
library(geosphere)
library(RColorBrewer)
library(classInt)
library(scales) 
library(tidyr)
library(jsonlite)
library(rjson)
library(geojsonsf)

edu_data <- read.csv("edu_attainment_data.csv")
horrid <- fromJSON(file = "kc_tract.json")
##tracts <- geojson_sf(tracts)


feature_test <- list()
featuredf_test <- list()
feature_geo_test <- list()
for(feature.id in 1:length(horrid$features)){
  tmp_geometry <- horrid$features[[feature.id]]$geometry
  tmp_coords <- tmp_geometry$coordinates
  feature_test[[feature.id]] <- lapply(unlist(tmp_coords),
                                       function(coords_elem){ coords_elem[[1]]
                                         
                                       } )
  
  featuredf_test[[feature.id]] <- 
    data.frame(long = unlist(feature_test[[feature.id]][seq(1,
                                                            length(feature_test[[feature.id]]),2)]),
               lat = unlist(feature_test[[feature.id]][seq(2,
                                                           length(feature_test[[feature.id]]),2)]))  
  feature_geo_test[[feature.id]] <- df_geojson(featuredf_test[[feature.id]], 
                                               lon = "long", lat = "lat") %>% 
    geojson_sf()
}

### make graphs without the crazy stuff ^^
get_geometry <-
  get_acs("tract",
          table = "C15002B",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) 

edu_geo <- get_geometry %>% 
  select(GEOID, geometry) %>% 
  mutate(GEOID = as.numeric(GEOID))


edu_data <- edu_data %>% 
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_data <- edu_geo %>% 
  left_join(edu_data, by = "GEOID")

years <- c(2009, 2014, 2019)
less_than_hs_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_estimate[[as.character(year)]] <- edu_data %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = estimate)) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_fermenter() + 
    theme_void()
}