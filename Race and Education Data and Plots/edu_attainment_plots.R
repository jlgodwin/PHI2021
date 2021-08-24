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

edu_data <- read.csv("edu_attainment_data.csv")

years <- c(2009, 2014, 2019)
less_than_hs_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_estimate[[as.character(year)]] <- edu_data %>% 
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}