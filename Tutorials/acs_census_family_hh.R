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
out_dir <- paste0(main_dir, "/Code/PHI2021/household_size/")

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)


#########################
### King County level ##
#########################

yearsCensus <- c(2000,2010)
yearsACS <- c(2005:2019)
var2000<-load_variables(2000,"sf1")
var2010<-load_variables(2010,"sf1")
var2005 <- load_variables(2005, "acs1")
##################################
## Households by household size ##
##################################

hhByhhTypeDF <- bind_rows(
  get_decennial(
    "county",
    table = c("P026"
    ),
    state = "WA",
    county = "King",
    year = 2000,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = 2000),
  get_decennial(
    "county",
    table = c("P028"
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
        "county",
        table = c("B11016"),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      mutate(source = "ACS")) %>%
  mutate(hh_size = case_when(
    variable == "P026001" ~ 0, # total
    variable == "P026003" ~ 2,
    variable == "P026004" ~ 3,
    variable == "P026005" ~ 4,
    variable == "P026006" ~ 5,
    variable == "P026007" ~ 6,
    variable == "P026008" ~ 7,
    variable == "P026010" ~ 1,
    variable == "P026011" ~ 2,
    variable == "P026012" ~ 3,
    variable == "P026013" ~ 4,
    variable == "P026014" ~ 5,
    variable == "P026015" ~ 6,
    variable == "P026016" ~ 7,
    variable == "P028001" ~ 0, # total
    variable == "P028003" ~ 2,
    variable == "P028004" ~ 3,
    variable == "P028005" ~ 4,
    variable == "P028006" ~ 5,
    variable == "P028007" ~ 6,
    variable == "P028008" ~ 7,
    variable == "P028010" ~ 1,
    variable == "P028011" ~ 2,
    variable == "P028012" ~ 3,
    variable == "P028013" ~ 4,
    variable == "P028014" ~ 5,
    variable == "P028015" ~ 6,
    variable == "P028016" ~ 7,
    variable == "B11016_001" ~ 0, # total
    variable == "B11016_003" ~ 2,
    variable == "B11016_004" ~ 3,
    variable == "B11016_005" ~ 4,
    variable == "B11016_006" ~ 5,
    variable == "B11016_007" ~ 6,
    variable == "B11016_008" ~ 7,
    variable == "B11016_010" ~ 1,
    variable == "B11016_011" ~ 2,
    variable == "B11016_012" ~ 3,
    variable == "B11016_013" ~ 4,
    variable == "B11016_014" ~ 5,
    variable == "B11016_015" ~ 6,
    variable == "B11016_016" ~ 7
  )) %>%
  mutate(
    type = ifelse(variable %in% c("B11016_010","B11016_011","B11016_012","B11016_013",
                                    "B11016_014","B11016_015","B11016_016",
                                    "P028010","P028011","P028012","P028013","P028014","P028015",
                                    "P028016","P028017",
                                    "P026010","P026011","P026012","P026013","P026014","P026015",
                                    "P026016","P026017"), "Non-family", "Family")
  ) %>%
  filter(!is.na(hh_size))
hhByhhTypeDF$type[hhByhhTypeDF$hh_size == 0] <- "Total" 

saveRDS(hhByhhTypeDF, file = paste0(out_dir, "hh_by_hh_size_and_type_kc.RDS"))
