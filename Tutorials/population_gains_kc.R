########################################################
###
### Analysis of Census and ACS data at the county level
### Describing gains in population over time
### Updated August 17, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(tidycensus)
library(sf)
library(spdep)
library(raster)
library(mapview)

library(data.table)
library(tidyverse)
library(lme4)
library(INLA)
library(ggplot2)
library(gridExtra)

# constants
main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "/Code/PHI2021/household_size/")

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)


#########################
### King County level ##
#########################

yearsCensus <- c(2000,2010)
yearsACS <- c(2005:2019)

var_df_census2000 <- load_variables(2000, "sf1", cache = TRUE)
var_df_census2010 <- load_variables(2010, "sf1", cache = TRUE)
var_df_acs2005 <- load_variables(2005, "acs1", cache = TRUE)
var_df_acs2019 <- load_variables(2019, "acs1", cache = TRUE)

population_kc <- bind_rows(lapply(yearsCensus, function(x){
  get_decennial(
    "county",
    variables = "P001001",
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE,
    geometry = FALSE
  ) %>%
    mutate(Year = x)})) %>%
  rename(total_population = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(yearsCensus, function(x){
      get_decennial(
        "county",
        variables = "H010001",
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE
      ) %>%
        mutate(Year = x)})) %>%
      rename(hh_population = value) %>%
      dplyr::select(-variable)
  ) %>%    
  left_join(
      bind_rows(
        get_decennial(
        "county",
        variables = "P027024",
        state = "WA",
        county = "King",
        year = 2000,
        cache_table = TRUE,
        geometry = FALSE
      ) %>%
        mutate(Year = 2000) %>%
      rename(gq_population = value) %>%
      dplyr::select(-variable),
      get_decennial(
        "county",
        variables = "P029026",
        state = "WA",
        county = "King",
        year = 2010,
        cache_table = TRUE,
        geometry = FALSE
      ) %>%
        mutate(Year = 2010) %>%
        rename(gq_population = value) %>%
        dplyr::select(-variable)
        )
  ) %>%
  mutate(
    moe.total_population = NA,
    moe.hh_population = NA,
    moe.gq_population = NA,
  )%>% 
  rbind(
  bind_rows(lapply(yearsACS, function(x){
  get_acs(
    "county",
    variables = "B01003_001",
    state = "WA",
    county = "King",
    year = x,
    survey = "acs1",
    moe = 95,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = estimate,
         moe.total_population = moe) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(yearsACS, function(x){
      get_acs(
    "county",
    variables = "B25008_001", #B25008_001
    state = "WA",
    county = "King",
    year = x,
    survey = "acs1",
    moe = 95,
    cache_table = TRUE,
    geometry = FALSE) %>%
      mutate(Year = x)})) %>%
  rename(hh_population = estimate,
         moe.hh_population = moe) %>%
  dplyr::select(-variable)) %>%
  left_join(
    bind_rows(lapply(2006:2019, function(x){
      get_acs(
        "county",
        variables = "B26001_001",
        state = "WA",
        county = "King",
        year = x,
        survey = "acs1",
        moe = 95,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(gq_population = estimate,
             moe.gq_population = moe) %>%
      dplyr::select(-variable)))

saveRDS(population_kc, file = paste0(out_dir,"population_over_time_kc.RDS"))
