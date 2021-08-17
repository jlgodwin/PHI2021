########################################################
###
### Aggregating average household size to HRA by:
### 1) population-weighting census-tract level average hh size estimates
### 2) aggregating population and housing units total to HRAs

### Updated August 13, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(tidycensus)
library(sf)
library(sp)
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
code_dir <- paste0(main_dir, "Code/PHI2021/Tutorials/")

# getting the census tract level estimates
totDF <- readRDS(file = paste0(out_dir, "census_tract_survey_data.RDS"))
load(paste0(out_dir, "tracts_to_hra.rda"))

# unlisting the tract to hra object (1 per acs5)
cw2009 <- tracts_to_hra$acs5_2009 %>%
  mutate(Year = 2009)
cw2014 <- tracts_to_hra$acs5_2014 %>%
  mutate(Year = 2014)
cw2019 <- tracts_to_hra$acs5_2019 %>%
  mutate(Year = 2019)

cw <- rbind(cw2009, cw2014, cw2019)

# 1) population-weighted average hh size estimate

est_hra_avhhsize1 <- totDF %>%
  subset(source == "acs") %>%
  left_join(cw) %>%
  group_by(HRA2010v2_, Year) %>%
  summarize(
    av.hh.size = sum(hhs*household_population, na.rm = T)/sum(household_population, na.rm = T), # population_weighted,
    av.hh.size.moe = moe_ratio(sum(hhs*household_population),
                               sum(household_population),
                               moe_sum(moe = moe_product(hhs,
                                                         household_population,
                                                         hhs.moe,
                                                         household_population.moe)),
                               moe_sum(moe = household_population.moe))
  )


est_hra_avhhsize2 <- totDF %>%
  subset(source == "acs") %>%
  left_join(cw) %>%
  group_by(factor(HRA2010v2_), Year) %>%
  summarize(
    av.hh.size = sum(household_population, na.rm = T)/sum(housing_units, na.rm = T), # population_weighted,
    av.hh.size.moe = moe_ratio(sum(household_population, na.rm = T),
                               sum(housing_units, na.rm = T),
                               moe_sum(moe = household_population.moe),
                               moe_sum(moe = housing_units.moe))
  )


############################
## Looking at it on a map ##
############################
hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))

# getting pretty map settings
source(paste0(code_dir, "map_main_theme.R"))

# average hh size
st_as_sf(hra) %>%
  full_join(est_hra_avhhsize1) %>%
  ggplot() +
  geom_sf(aes(fill = av.hh.size)) +
  facet_wrap(~Year) +
  ggtitle("Average household size by Health Reporting Areas (ACS estimates)") +
  labs(fill = "Average household size") +
  map_theme_main

# normalized average hh size
st_as_sf(hra) %>%
  full_join(est_hra_avhhsize1 %>%
              mutate(
                av.hh.size.z = (av.hh.size-mean(av.hh.size))/av.hh.size
              )) %>%
  ggplot() +
  geom_sf(aes(fill = av.hh.size.z)) +
  facet_wrap(~Year) +
  ggtitle("Normalized average household size by Health Reporting Areas (ACS estimates)") +
  labs(fill = "Normalized average household size") +
  map_theme_main
# saving the estimates for them to be used by others
saveRDS(est_hra_avhhsize1, file = paste0(code_dir, "average_hh_size_by_HRAs.RDS"))

wideDF <- est_hra_avhhsize1 %>%
  data.table() %>%
  dcast(HRA2010v2_ ~ Year, value.var = c("av.hh.size","av.hh.size.moe"))

wideDF[,change_2019_2009:= (av.hh.size_2019-av.hh.size_2009)]  
wideDF[,change_2014_2009:= (av.hh.size_2014-av.hh.size_2009)]   
wideDF[,change_2019_2014:= (av.hh.size_2019-av.hh.size_2014)] 

wideDF.plot <- st_as_sf(hra) %>%
  full_join(
    wideDF %>%
    melt(
      id.vars = c('HRA2010v2_')
    ) %>%
    mutate(Year = str_sub(variable, 8, 20)) %>%
    filter(Year %in% c('2019_2014', '2014_2009','2019_2009'))
  ) 

ggplot(wideDF.plot) +
  geom_sf(aes(fill = value)) +
  facet_wrap(~Year) +
  ggtitle("Change in average household size by Health Reporting Areas (ACS estimates)") +
  labs(fill = "Change in average household size") +
  map_theme_main
