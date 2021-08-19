########################################################
###
### Analysis of Census and ACS data at the county level
### Describing gains in population over time by hh size
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

hhByhhSize <- bind_rows(lapply(yearsCensus, function(x){
  get_decennial(
    "county",
    table = "H013",
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  mutate(hh_size = case_when(
    variable == "H013002" ~ 1,
    variable == "H013003" ~ 2,
    variable == "H013004" ~ 3,
    variable == "H013005" ~ 4,
    variable == "H013006" ~ 5,
    variable == "H013007" ~ 6,
    variable == "H013008" ~ 7,
  ),
  hh_size = factor(hh_size),
  source = "Census") %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>%
  mutate(moe = NA) %>%
  rbind(bind_rows(lapply(yearsACS, function(x){
  get_acs(
    "county",
    table = "B11016",
    state = "WA",
    county = "King",
    year = x,
    survey = "acs1",
    moe = 95,
    cache_table = TRUE) %>%
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
  hh_size = factor(hh_size),
  source = "ACS") %>%
  dplyr::select(-variable) %>%
    rename(value = estimate) %>%
  filter(!is.na(hh_size)) %>%
    group_by(GEOID, NAME, Year, hh_size, source) %>%
    summarize(
      value = sum(value),
      moe = moe_sum(moe)
    )) %>% 
  data.table()

familyCensusDF <- bind_rows(lapply(yearsCensus, function(x){
  get_decennial(
    "county",
    variables = "P021002",
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(tot_family = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(yearsCensus, function(x){
      get_decennial(
        "county",
        variables = "P021011",
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)}))%>%
      rename(tot_nonfamily = value) %>%
      dplyr::select(-variable)
  ) %>%
  mutate(source = "Census") 
familyacsDF<- bind_rows(lapply(yearsACS, function(x){
    get_acs(
      "county",
      table = "B11016",
      state = "WA",
      county = "King",
      year = x,
      survey = "acs1",
      moe = 95,
      cache_table = TRUE) %>%
      mutate(Year = x)})) %>%
      filter(variable %in% c("B11016_002","B11016_009")) %>%
      mutate(source = "ACS") %>% 
  data.table()

familyacsDF$estimate[familyacsDF$Year == 2019 & familyacsDF$variable == "B11016_002"]-familyCensusDF$tot_family[familyCensusDF$Year == 2000]
familyacsDF$estimate[familyacsDF$Year == 2019 & familyacsDF$variable == "B11016_009"]-familyCensusDF$tot_nonfamily[familyCensusDF$Year == 2000]
familyCensusDF$tot_family[familyCensusDF$Year == 2000]-familyCensusDF$tot_nonfamily[familyCensusDF$Year == 2000]
familyacsDF$estimate[familyacsDF$Year == 2019 & familyacsDF$variable == "B11016_002"]-familyacsDF$estimate[familyacsDF$Year == 2019 & familyacsDF$variable == "B11016_009"]

##############################
## Plot % hh size over time ##
##############################

p1 <- plotDF %>%
  ggplot(aes(x = Year, y = value, ymin = lo, ymax = hi, 
             group = hh_size, shape = source)) +
  geom_line(data = plotDF %>% filter(source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in King County grouped by household size") +
  guides(fill=FALSE)

ggsave(filename = paste0(out_dir,"population_by_hh_size.png"),
       height = 6, width = 9, plot = p1)

diffDF <- plotDF %>%
  filter(Year == 2000) %>%
  dplyr::select(GEOID, hh_size, value) %>%
  rename(value2000=value) %>%
  left_join(plotDF %>% filter(Year != 2000)) %>%
  mutate(diff2000 = value - value2000,
         diff2000_hi = hi - value2000,
         diff2000_lo = lo - value2000)

p2<-ggplot(data = diffDF, aes(x = Year, y = diff2000 , ymin = diff2000_lo, ymax = diff2000_hi, 
             group = hh_size, shape = source)) +
  geom_line(data = diffDF,aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Gains in households since 2000 in King County") +
  guides(fill=FALSE)

ggsave(filename = paste0(out_dir,"population_gains_by_hh_size.png"),
       height = 6, width = 9, plot = p2)
