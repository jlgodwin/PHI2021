########################################################
###
### First Analysis of ACS data
### Updated July 21st, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(tidycensus)
library(sf)
library(mapview)
library(data.table)
library(tidyverse)

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)

#####################
# -- Census 2010 -- #
#####################

# Download the Variable dictionary
var_df <- load_variables(2010, "sf1", cache = TRUE)

###############################################
# -- reproducing analysis from Jarosz 2021 -- #
##############################################
tractpopDF <- get_decennial("tract",
                          table = c("H01"),
                          summary_var = "H010001",
                          year = 2010,
                          state = "CA",
                          geometry = FALSE)

hhsizeDF <- get_decennial("tract",
                    table = c("H013"),
                    summary_var = "H013001",
                    year = 2010,
                    state = "CA",
                    geometry = FALSE)

hhaveragesizeDF <- get_decennial("tract",
                          table = c("H012"),
                          summary_var = "H012001",
                          year = 2010,
                          state = "CA",
                          geometry = FALSE)

length(unique(tractpopDF$GEOID))
length(unique(hhsizeDF$GEOID))
length(unique(hhaveragesizeDF$GEOID)) # 79 extra tracts compared to Jarosz paper ...

tractpopDF %>%
  filter(variable == "H010001") %>%
  rename(totpop = value) %>%
  dplyr::select(-c(variable,summary_value)) %>%
  left_join(hhaveragesizeDF %>%
              filter(variable == "H012001") %>%
              rename(averagesize = value)) %>%
  summarize(
    average_per_tract = sum(totpop*averagesize)/sum(totpop)
  ) # 3.06

tmp <- hhsizeDF %>%
  mutate(
    hhsize = case_when(
      # variable == "H013001" ~ 0, # total
      variable == "H013002" ~ 1,
      variable == "H013003" ~ 2,
      variable == "H013004" ~ 3,
      variable == "H013005" ~ 4,
      variable == "H013006" ~ 5,
      variable == "H013007" ~ 6,
      variable == "H013008" ~ 7
    ),
    hhsize = factor(hhsize)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hhsize)) %>% 
  data.table()

tmp[, list(value = sum(value)), by = 'GEOID']
tmp[, prop := value/total]

hist(tmp$prop)

glm(family = "poisson",hhaveragesizeDF %>%
  filter(variable == "H012001") %>%
    mutate(value = value - 1))

