########################################################
###
### Analysis of Census and ACS data at the county level
### Stratified analysis by multi-generational households
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
yearsACS <- c(2008:2019)

##################################
## Households by household size ##
##################################

multigenHHDF <- bind_rows(lapply(yearsCensus, function(x){
  get_decennial(
    "county",
    table = c("PCT014"
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
        table = c("B11017"),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      mutate(source = "ACS")) %>%
  mutate(multigenerational = case_when(
    variable == "PCT014001" ~ "Total", # total
    variable == "PCT014002" ~ "Yes",
    variable == "PCT014003" ~ "No",
    variable == "B11017_001" ~ "Total", # total
    variable == "B11017_002" ~ "Yes",
    variable == "B11017_003" ~ "No"
  )) %>%
  filter(!is.na(multigenerational))

saveRDS(multigenHHDF, file = paste0(out_dir, "multi_gen_hh_kc.RDS"))
