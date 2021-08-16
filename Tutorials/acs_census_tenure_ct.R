########################################################
###
### Analysis of Census and ACS data at the census tract level
### Stratified analysis by ownership status
### Updated August 16, 2021

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
yearsACS <- c(2009,2014,2019)

############################
## Average household size ##
############################
var_df_census <- load_variables(2010, "sf1", cache = TRUE)
var_df_acs <- load_variables(2019, "acs1", cache = TRUE)



avgHHsizeDF <- bind_rows(lapply(yearsCensus, function(x){
  get_decennial(
    "tract",
    variables = c("H012001" #hhs
    ),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) %>%
  rename(hhs = value) %>%
  dplyr::select(-variable) %>%
  mutate(type = "Total") %>%
  rbind(
    bind_rows(lapply(yearsCensus, function(x){
      get_decennial(
        "tract",
        variables = c("H012002" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = value) %>%
      dplyr::select(-variable)  %>%
      mutate(type = "Owner") 
  ) %>%
  rbind(
    bind_rows(lapply(yearsCensus, function(x){
      get_decennial(
        "tract",
        variables = c("H012003" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = value) %>%
      dplyr::select(-variable) %>%
      mutate(type = "Renter") 
  ) %>%
  mutate(source = "Census",
         moe = NA) %>%
  rbind(
    bind_rows(lapply(yearsACS, function(x){
      get_acs(
        "tract",
        variables = c("B25010_001" #hhs
        ),
        state = "WA",
        county = "King",
        survey = "acs5",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = estimate) %>%
      dplyr::select(-c(variable)) %>%
      mutate(type = "Total")
    %>%
      rbind(
        bind_rows(lapply(yearsACS, function(x){
          get_acs(
            "tract",
            variables = c("B25010_002" #hhs
            ),
            state = "WA",
            county = "King",
            survey = "acs5",
            moe_level = 95,
            year = x,
            cache_table = TRUE,
            geometry = FALSE) %>%
            mutate(Year = x)})) %>%
          rename(hhs = estimate) %>%
          dplyr::select(-c(variable)) %>%
          mutate(type = "Owner")
      ) %>%
      rbind(
        bind_rows(lapply(yearsACS, function(x){
          get_acs(
            "tract",
            variables = c("B25010_003" #hhs
            ),
            state = "WA",
            county = "King",
            survey = "acs5",
            moe_level = 95,
            year = x,
            cache_table = TRUE,
            geometry = FALSE) %>%
            mutate(Year = x)})) %>%
          rename(hhs = estimate) %>%
          dplyr::select(-c(variable)) %>%
          mutate(type = "Renter")
      ) %>%
      mutate(source = "ACS"))

saveRDS(avgHHsizeDF, file = paste0(out_dir, "average_hh_size_by_ownership_ct.RDS"))

##################################
## Households by household size ##
##################################

hhByhhSizeDF <- bind_rows(
  get_decennial(
    "tract",
    table = c("H015"
    ),
    state = "WA",
    county = "King",
    year = 2000,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = 2000),
  get_decennial(
    "tract",
    table = c("H016"
    ),
    state = "WA",
    county = "King",
    year = 2010,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = 2010)) %>%
  rename(estimate = value) %>%
  mutate(source = "Census",
         moe = NA) %>%
  rbind(
    bind_rows(lapply(yearsACS, function(x){
      get_acs(
        "tract",
        table = c("B25009"),
        state = "WA",
        county = "King",
        survey = "acs5",
        moe_level = 95,
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      mutate(source = "ACS")) %>%
  mutate(hh_size = case_when(
    variable == "H015001" ~ 0, # total
    variable == "H015003" ~ 1,
    variable == "H015004" ~ 2,
    variable == "H015005" ~ 3,
    variable == "H015006" ~ 4,
    variable == "H015007" ~ 5,
    variable == "H015008" ~ 6,
    variable == "H015009" ~ 7,
    variable == "H015011" ~ 1,
    variable == "H015012" ~ 2,
    variable == "H015013" ~ 3,
    variable == "H015014" ~ 4,
    variable == "H015015" ~ 5,
    variable == "H015016" ~ 6,
    variable == "H015017" ~ 7,
    variable == "H016001" ~ 0, # total
    variable == "H016003" ~ 1,
    variable == "H016004" ~ 2,
    variable == "H016005" ~ 3,
    variable == "H016006" ~ 4,
    variable == "H016007" ~ 5,
    variable == "H016008" ~ 6,
    variable == "H016009" ~ 7,
    variable == "H016011" ~ 1,
    variable == "H016012" ~ 2,
    variable == "H016013" ~ 3,
    variable == "H016014" ~ 4,
    variable == "H016015" ~ 5,
    variable == "H016016" ~ 6,
    variable == "H016017" ~ 7,
    variable == "B25009_001" ~ 0, # total
    variable == "B25009_003" ~ 1,
    variable == "B25009_004" ~ 2,
    variable == "B25009_005" ~ 3,
    variable == "B25009_006" ~ 4,
    variable == "B25009_007" ~ 5,
    variable == "B25009_008" ~ 6,
    variable == "B25009_009" ~ 7,
    variable == "B25009_011" ~ 1,
    variable == "B25009_012" ~ 2,
    variable == "B25009_013" ~ 3,
    variable == "B25009_014" ~ 4,
    variable == "B25009_015" ~ 5,
    variable == "B25009_016" ~ 6,
    variable == "B25009_017" ~ 7
  )) %>%
  mutate(
    tenure = ifelse(variable %in% c("B25009_011","B25009_012","B25009_013",
                                    "B25009_014","B25009_015","B25009_016","B25009_017",
                                    "H016011","H016012","H016013","H016014","H016015",
                                    "H015016","H015017",
                                    "H015011","H015012","H015013","H015014","H015015",
                                    "H015016","H015017"), "Renter", "Owner")
  )
hhByhhSizeDF$tenure[hhByhhSizeDF$hh_size == 0] <- "Total" 

saveRDS(hhByhhSizeDF, file = paste0(out_dir, "hh_by_hh_size_and_tenure_ct.RDS"))

