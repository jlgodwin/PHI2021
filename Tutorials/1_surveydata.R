########################################################
###
### Analysis of Census and ACS data
### Updated August 9, 2021

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
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)


#########################
### King County level ##
#########################

years <- c(2000,2010)


#############################
## Read in the Census data ##
#############################
censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    geography = "county",
    variables = c("P001001"# tot pop
    ),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H001001" # hs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(housing_units = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H010001" # hhp
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(household_population = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H003001" 
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(total_occ = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H003002"
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(occ = value) %>%
      dplyr::select(-variable)
  )  %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H003003"
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(vac = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H012001" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = value) %>%
      dplyr::select(-variable)
  ) 

hh_by_size_census <- bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013001" # total # hh
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hh = value) %>%
      dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013002" #hh1
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.1 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013003" #hh2
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.2 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013004" #hh3
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.3 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013005" #hh4
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.4 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013006" #hh5
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.5 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013007" #hh6
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.6 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H013008" #hh7+
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.7 = value) %>%
      dplyr::select(-variable)
  ) 

##########################
## Read in the ACS data ##
##########################
var_df <- load_variables(2019, "acs1", cache = TRUE)

years <- c(2005:2019)
acs1DF <- bind_rows(lapply(years, function(x){
  get_acs(
    "county",
    variables = c("B01001_001"),
    state = "WA",
    county = "King",
    survey = "acs1",
    moe_level = 95,
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = estimate,
         total_population.moe = moe) %>%
  dplyr::select(-c(variable))  %>%
left_join(
  bind_rows(lapply(c(2005,2009:2019), function(x){
    get_acs(
      "county",
      variables = c("B25026_001"
      ),
      state = "WA",
      county = "King",
      survey = "acs1",
      moe_level = 95,
      year = x,
      cache_table = TRUE,
      geometry = FALSE) %>%
      mutate(Year = x)})) %>%
    rename(household_population = estimate,
           household_population.moe = moe) %>%
    dplyr::select(-c(variable))
) %>%
  left_join(
    bind_rows(lapply(c(2005,2009:2019), function(x){
      get_acs(
        "county",
        table = c("B25027"
        ),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      filter(variable == "B25027_001"| variable == "B25027_010") %>%
      group_by(GEOID, NAME, Year) %>%
      summarize(
        estimate = sum(estimate),
        moe = moe_sum(moe, estimate)
      ) %>%
      rename(housing_units = estimate,
             housing_units.moe = moe)
  ) %>%
left_join(
  bind_rows(lapply(years, function(x){
    get_acs(
      "county",
      variables = c("B25002_001" 
      ),
      state = "WA",
      county = "King",
      survey = "acs1",
      moe_level = 95,
      year = x,
      cache_table = TRUE,
      geometry = FALSE) %>%
      mutate(Year = x)})) %>%
    rename(total_occ = estimate,
           total_occ.moe = moe) %>%
    dplyr::select(-c(variable))
) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "county",
        variables = c("B25002_002"
        ),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(occ = estimate,
             occ.moe = moe) %>%
      dplyr::select(-c(variable))
  )  %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "county",
        variables = c("B25002_003"
        ),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(vac = estimate,
             vac.moe = moe) %>%
      dplyr::select(-c(variable))
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "county",
        variables = c("B25010_001" #hhs
        ),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = FALSE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = estimate,
             hhs.moe = moe) %>%
      dplyr::select(-c(variable))
  ) 

hh_by_size_acs <- bind_rows(lapply(years, function(x){
  get_acs(
    "county",
    table = c("B11016"
    ),
    # summary_var = "B11016_01",
    state = "WA",
    county = "King",
    survey = "acs1",
    moe_level = 95,
    year = x,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) %>%
  mutate(hh_size = case_when(
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
    variable == "B11016_016" ~ 7,
  ),
  hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>% 
  group_by(GEOID, Year, hh_size) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe,estimate)
  )

library(plyr)

totDF <- acs1DF %>%
      mutate(
        source = "acs"
      )  %>%
      data.frame() %>%
      rbind.fill(
        censusDF %>%
          mutate(
            source = "census"
          ) %>%
          data.frame()
      )
saveRDS(totDF, file = paste0(out_dir, "kc_survey_data.RDS"))
saveRDS(hh_by_size_acs, file = paste0(out_dir, "kc_hh_by_size.RDS"))

#########################
### census-tract level ##
#########################

#############################
## Read in the Census data ##
#############################

years <- c(2000,2010)

censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "tract",
    variables = c("P001001"# tot pop
    ),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H001001" # hs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(housing_units = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H010001" # hhp
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(household_population = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003001" 
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(total_occ = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003002"
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(occ = value) %>%
      dplyr::select(-variable)
  )  %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003003"
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(vac = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H012001" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013001" # total # hh
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hh = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013002" #hh1
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.1 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013003" #hh2
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.2 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013004" #hh3
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.3 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013005" #hh4
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.4 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013006" #hh5
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.5 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013007" #hh6
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.6 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013008" #hh7+
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.7 = value) %>%
      dplyr::select(-variable)
  ) 

##########################
## Read in the ACS data ##
##########################
var_df <- load_variables(2019, "acs5", cache = TRUE)

years <- c(2009,2014,2019)
acs5DF <- bind_rows(lapply(years, function(x){
  get_acs(
    "tract",
    variables = c("B01001_001"),
    state = "WA",
    county = "King",
    survey = "acs5",
    moe_level = 95,
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = estimate,
         total_population.moe = moe) %>%
  dplyr::select(-c(variable))  %>%
  # left_join(
  #   bind_rows(lapply(years, function(x){
  #     get_acs(
  #       "tract",
  #       variables = c("B26101_214" 
  #       ),
  #       state = "WA",
  #       county = "King",
  #       survey = "acs5",
  #       moe_level = 95,
  #       year = x,
  #       cache_table = TRUE,
  #       geometry = TRUE) %>%
  #       mutate(Year = x)})) %>%
  #     rename(household_population = estimate,
  #            household_population.moe = moe) %>%
  #     dplyr::select(-c(variable))
  # ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "tract",
        variables = c("B25002_001" 
        ),
        state = "WA",
        county = "King",
        survey = "acs5",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(total_occ = estimate,
             total_occ.moe = moe) %>%
      dplyr::select(-c(variable))
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "tract",
        variables = c("B25002_002"
        ),
        state = "WA",
        county = "King",
        survey = "acs5",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(occ = estimate,
             occ.moe = moe) %>%
      dplyr::select(-c(variable))
  )  %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "tract",
        variables = c("B25002_003"
        ),
        state = "WA",
        county = "King",
        survey = "acs5",
        moe_level = 95,
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(vac = estimate,
             vac.moe = moe) %>%
      dplyr::select(-c(variable))
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
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
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = estimate,
             hhs.moe = moe) %>%
      dplyr::select(-c(variable))
  ) 

hh_by_size <- bind_rows(lapply(years, function(x){
  get_acs(
    "tract",
    table = c("B11016"
    ),
    # summary_var = "B11016_01",
    state = "WA",
    county = "King",
    survey = "acs5",
    moe_level = 95,
    year = x,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) 
hh_by_size %>%
  mutate(hh_size = case_when(
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
    variable == "B11016_016" ~ 7,
  ),
  hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>% 
  group_by(GEOID, Year, hh_size) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe,estimate)
  )

saveRDS(acs5DF, file = paste0(out_dir, "census_tract_survey_data.RDS"))
saveRDS(hh_by_size, file = paste0(out_dir, "census_tract_hh_by_size.RDS"))
############################
## Linking census and ACS ##
############################

loc <- readRDS(paste0(out_dir,"loc.RDS")) %>%
  mutate(GEOID = as.character(tractfips))

tmp<-censusDF %>%
  left_join(tracts_to_hra[['acs5_2009']])
