library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)

ddi <- read_ipums_ddi("cps_00003.xml")
data <- read_ipums_micro(ddi)
king_county <- data %>% 
  filter(COUNTY == 53033)
#count number of individuals with less than hs education
less_than_hs <- king_county %>% 
  filter(!EDUC %in% c(001, 040, 050, 060, 070, 071, 072, 073, 080, 081, 090, 091, 
                      092, 100, 110, 120, 121, 122, 124, 111, 123, 125, 999)) %>% 
  select(RACE, EDUC)

less_than_hs_count <- less_than_hs %>% 
  count(EDUC, RACE)
  print(less_than_hs_count)
  
