########################################################
###
### Extraction of census and acs data relating to equity
### awareness variables to identify HRAs most at risk

## Wildfire risk - income distribution, race/ethnicity, language needs, mobility/disability
## Extreme heat - income, race/ethnicity, health disparities/health conditions, age (very young <4, very old >65)
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

yearsCensus <- c(2000,2010)
yearsACS <- c(2005:2019)
var2000<-load_variables(2000,"sf1")
var2010<-load_variables(2010,"sf1")
varACS <- load_variables(2019, "acs5")

disability<-get_acs(
  "tract",
  variables = c("B10052_002"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE)

language<-get_acs(
  "tract",
  table = c("C16002"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE) %>%
  filter(variable %in% c("C16002_004","C16002_007","C16002_010","C16002_013")) %>%
  group_by(GEOID) %>%
  summarize(
    lim_eng = sum(estimate),
    moe.lim_eng = moe_sum(moe)
  )

race<-get_acs(
  "tract",
  table = c("B02001"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE) %>%
  filter(!(variable %in% c("B02001_001","B02001_002"))) %>%
  group_by(GEOID) %>%
  summarize(
    non_white = sum(estimate),
    moe.non_white = moe_sum(moe)
  )

ratio_inc_pov_lvl <- get_acs(
  "tract",
  table = c("C17002"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE) %>%
  filter(variable %in% c("C17002_002","C17002_002","C17002_002")) %>%
  group_by(
    GEOID
  ) %>%
    summarize(
      pop_under_1.25_pov_line = sum(estimate),
      moe.pop_under_1.25_pov_line = moe_sum(moe)
    )
age_young <- get_acs(
  "tract",
  table= c("B01001"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE) %>%
  filter(variable %in% c("B01001_027","B01001_003")) %>%
  group_by(GEOID) %>%
  summarize(
    young = sum(estimate),
    moe.young = moe_sum(estimate)
  )

age_old <- get_acs(
  "tract",
  table= c("B01001"
  ),
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  cache_table = TRUE,
  geometry = FALSE) %>%
  filter(variable %in% c("B01001_020","B01001_021",
                         "B01001_022","B01001_023",
                         "B01001_024","B01001_025",
                         "B01001_044","B01001_045",
                         "B01001_046","B01001_047",
                         "B01001_048","B01001_049")) %>%
  group_by(GEOID) %>%
  summarize(
    old = sum(estimate),
    moe.old = moe_sum(estimate)
  )

