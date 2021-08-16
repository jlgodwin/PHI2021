########################################################
###
### Analysis of Census and ACS data at the county level
### Stratified analysis by household type (family vs non-family)
### Updated August 15, 2021

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
out_dir <- paste0(main_dir, "output/")

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)


#########################
### King County level ##
#########################

yearsCensus <- c(2000,2010)
yearsACS <- c(2005:2019)

##################################
## Households by household size ##
##################################

hhByhhTypeDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "county",
    table = c("P028"
    ),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) %>%
  rename(estimate = value) %>%
  mutate(source = "Census",
         moe = NA) %>%
  rbind(
    bind_rows(lapply(yearsACS, function(x){
      get_acs(
        "county",
        table = c("B25009"),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      mutate(source = "ACS")) %>%
  mutate(hh_size = case_when(
    variable == "P028001" ~ 0, # total
    variable == "P028003" ~ 1,
    variable == "P028004" ~ 2,
    variable == "P028005" ~ 3,
    variable == "P028006" ~ 4,
    variable == "P028007" ~ 5,
    variable == "P028008" ~ 6,
    variable == "P028009" ~ 7,
    variable == "P028011" ~ 1,
    variable == "P028012" ~ 2,
    variable == "P028013" ~ 3,
    variable == "P028014" ~ 4,
    variable == "P028015" ~ 5,
    variable == "P028016" ~ 6,
    variable == "P028017" ~ 7,
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
                                    "P028011","P028012","P028013","P028014","P028015",
                                    "P028016","P028017"), "Non-family", "Family")
  ) %>%
  filter(!is.na(hh_size))
hhByhhTypeDF$tenure[hhByhhTypeDF$hh_size == 0] <- "Total" 

saveRDS(hhByhhTypeDF, file = paste0(out_dir, "hh_by_hh_size_and_type_kc.RDS"))
