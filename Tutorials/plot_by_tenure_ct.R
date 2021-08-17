########################################################
###
### Maps and trends in KC at the HRA level by tenure status
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
library(crsuggest)
library(tigris)
# constants
main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "Code/PHI2021/Tutorials/")
  
# loading estimates
avgHHsizeDF <- readRDS(file = paste0(out_dir, "average_hh_size_by_ownership_ct.RDS"))
hhByhhSizeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_tenure_ct.RDS"))

############################
## Average household size ##
############################

# need to population-weight our estimates
yearsACS <- c(2009,2014,2019)

totDF <- avgHHsizeDF %>%
  filter(Year %in% yearsACS) %>%
  right_join(bind_rows(lapply(yearsACS, function(x){
  get_acs("tract",
          table =  "B25003",
          state = "WA",
          county = "King",
          year = x,
          cache_table = TRUE,
          geometry = FALSE) %>%
    mutate(Year = x)})) %>%
    dplyr::select(-NAME) %>%
  rename(
    hhp = estimate,
    hhp.moe = moe
  ) %>%
  mutate(
    type = case_when(
      variable == "B25008_001" ~ "Total",
      variable == "B25008_002" ~ "Owner",
      variable == "B25008_003" ~ "Renter"
    )
  ))

load(paste0(out_dir, "tracts_to_hra.rda"))

# unlisting the tract to hra object (1 per acs5)
cw2009 <- tracts_to_hra$acs5_2009 %>%
  mutate(Year = 2009)
cw2014 <- tracts_to_hra$acs5_2014 %>%
  mutate(Year = 2014)
cw2019 <- tracts_to_hra$acs5_2019 %>%
  mutate(Year = 2019)

cw <- rbind(cw2009, cw2014, cw2019)

# population-weighted average hh size estimate

hraDF <- totDF %>%
  left_join(cw) %>%
  group_by(HRA2010v2_, Year, type) %>%
  summarize(
    av.hh.size = sum(hhs*hhp, na.rm = T)/sum(hhp, na.rm = T), # population_weighted,
    av.hh.size.moe = moe_ratio(sum(hhs*hhp),
                               sum(hhp),
                               moe_sum(moe = moe_product(hhs,
                                                         hhp,
                                                         moe,
                                                         hhp.moe)),
                               moe_sum(moe = hhp.moe))
  )

############################
## Looking at it on a map ##
############################
hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))

# getting pretty map settings
source(paste0(code_dir, "map_main_theme.R"))

# average hh size
st_as_sf(hra) %>%
  full_join(hraDF) %>%
  filter(type != "Total") %>%
  ggplot() +
  geom_sf(aes(fill = av.hh.size)) +
  facet_wrap(Year ~ type, nrow = 3, ncol = 2) +
  ggtitle("Average household size by tenure and Health Reporting Areas (ACS estimates)") +
  labs(fill = "Average household size") +
  map_theme_main
