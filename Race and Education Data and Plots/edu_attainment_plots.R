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

## create white less than hs plots
edu_data_less_hs_white <- edu_data %>% 
  filter(str_starts(variable, "C15002A")) %>% 
  filter(less_than_hs == 1) %>%
  group_by(Year, GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_data_less_hs_white <- edu_geo %>% 
  left_join(edu_data_less_hs_white, by = "GEOID")

years <- c(2009, 2014, 2019)
less_than_hs_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_estimate[[as.character(year)]] <- edu_data_less_hs_white %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
    theme_void()
}

less_than_hs_white_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_cov[[as.character(year)]] <- edu_data_less_hs_white %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Coefficient of Variation, White alone, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('less_than_hs_white.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_white_estimate[[as.character(year)]], 
               less_than_hs_white_cov[[as.character(year)]], ncol = 2)
}
dev.off()

## create AIAN less than hs plots
edu_less_hs_AIAN <- edu_data %>% 
  filter(str_starts(variable, "C15002C")) %>% 
  filter(less_than_hs == 1) %>%
  group_by(Year, GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_less_hs_AIAN <- edu_geo %>% 
  left_join(edu_less_hs_AIAN, by = "GEOID")

years <- c(2009, 2014, 2019)
less_than_hs_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_AIAN_estimate[[as.character(year)]] <- edu_less_hs_AIAN %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, American Indian/Alaska Native, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
    theme_void()
}

less_than_hs_AIAN_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_AIAN_cov[[as.character(year)]] <- edu_less_hs_AIAN %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Coefficient of Variation, 
                           American Indian/Alaska Native, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('less_than_hs_AIAN.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_AIAN_estimate[[as.character(year)]], 
               less_than_hs_AIAN_cov[[as.character(year)]], ncol = 2)
}
dev.off()

## create Asian less than hs plots
edu_less_hs_Asian <- edu_data %>% 
  filter(str_starts(variable, "C15002D")) %>% 
  filter(less_than_hs == 1) %>%
  group_by(Year, GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_less_hs_Asian <- edu_geo %>% 
  left_join(edu_less_hs_Asian, by = "GEOID")

years <- c(2009, 2014, 2019)
less_than_hs_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_Asian_estimate[[as.character(year)]] <- edu_less_hs_Asian %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
    theme_void()
}

less_than_hs_Asian_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_Asian_cov[[as.character(year)]] <- edu_less_hs_Asian %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Less than High School",
         subtitle = paste0("Coefficient of Variation, Asian ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('less_than_hs_Asian.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_Asian_estimate[[as.character(year)]], 
               less_than_hs_Asian_cov[[as.character(year)]], ncol = 2)
}
dev.off()

## create white college grad plots
edu_collegegrad_white <- edu_data %>% 
  filter(str_starts(variable, "C15002A")) %>% 
  filter(college_grad == 1) %>%
  group_by(Year, GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_collegegrad_white <- edu_geo %>% 
  left_join(edu_collegegrad_white, by = "GEOID")

years <- c(2009, 2014, 2019)
collegegrad_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_white_estimate[[as.character(year)]] <- edu_collegegrad_white %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_distiller(type = "seq", 
                         limits = c(0, max(edu_collegegrad_white$estimate)),
                         palette = "Blues",
                         direction = 1) + 
    theme_void()
}

collegegrad_white_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_white_cov[[as.character(year)]] <- edu_collegegrad_white %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, White alone, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('college_grad_white.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(collegegrad_white_estimate[[as.character(year)]], 
               collegegrad_white_cov[[as.character(year)]], ncol = 2)
}
dev.off()

## create college grad Asian plots
edu_collegegrad_Asian <- edu_data %>% 
  filter(str_starts(variable, "C15002D")) %>% 
  filter(college_grad == 1) %>%
  group_by(Year, GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_collegegrad_Asian <- edu_geo %>% 
  left_join(edu_collegegrad_Asian, by = "GEOID")

years <- c(2009, 2014, 2019)
collegegrad_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_Asian_estimate[[as.character(year)]] <- edu_collegegrad_Asian %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_distiller(type = "seq", 
                         palette = "Blues",
                         direction = 1) + 
    theme_void()
}

collegegrad_Asian_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_Asian_cov[[as.character(year)]] <- edu_collegegrad_Asian %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, Asian, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('college_grad_Asian.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(collegegrad_Asian_estimate[[as.character(year)]], 
               collegegrad_Asian_cov[[as.character(year)]], ncol = 2)
}
dev.off()

# create AIAN college grad plots
edu_collegegrad_AIAN <- edu_data %>% 
  filter(str_starts(variable, "C15002C")) %>% 
  filter(college_grad == 1) %>%
  group_by(Year, GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()%>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

edu_collegegrad_AIAN <- edu_geo %>% 
  left_join(edu_collegegrad_AIAN, by = "GEOID")

years <- c(2009, 2014, 2019)
collegegrad_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_AIAN_estimate[[as.character(year)]] <- edu_collegegrad_AIAN %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = estimate), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, American Indian/Alaska Native, ", year)) +
    scale_fill_distiller(type = "seq", 
                         palette = "Blues",
                         direction = 1) + 
    theme_void()
}

collegegrad_AIAN_cov <- list()
for(year in years){
  year.idx <- match(year, years)
  collegegrad_AIAN_cov[[as.character(year)]] <- edu_collegegrad_AIAN %>% 
    filter(Year == year) %>% 
    ggplot() +
    geom_sf(aes(fill = CoV), size = .25) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation,
                           American Indian/Alaska Native, ", year)) +
    scale_fill_distiller(type = "seq", palette = "Blues", limits = c(0, .66), 
                         direction = 1) + 
    theme_void()
}

pdf('college_grad_AIAN.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(collegegrad_AIAN_estimate[[as.character(year)]], 
               collegegrad_AIAN_cov[[as.character(year)]], ncol = 2)
}
dev.off()