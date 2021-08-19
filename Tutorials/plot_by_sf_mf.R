########################################################
###
### Plots of trends in KC single vs multi family hh
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

# loading estimates
hhByhhTypeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_type_kc.RDS"))

#################################
## Household by household size ##
#################################

plotDF <- hhByhhTypeDF %>%
  mutate(hi = estimate + moe,
         lo = estimate - moe) %>%
  filter(
    type != "Total" & !is.na(hh_size)
  ) %>%
  mutate(hh_size = factor(hh_size))

p2 <- plotDF %>%  
  ggplot(aes(x = Year, y = estimate, ymin = lo, ymax = hi, 
             group = hh_size, shape = source)) +
  geom_line(data = plotDF %>% filter(source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~type) +
  labs(y="", color = "Household by household size in King County",
       title = "Household by household size and type") +
  guides(fill=FALSE)

ggsave(p2, filename =  paste0(out_dir, "hh_by_hh_size_and_type_kc.png"),
       width = 9, height = 6)

plotDF <- hhByhhTypeDF %>%
  filter(type == "Total") %>%
  dplyr::select(Year, source, estimate,moe) %>%
  rename(total = estimate,
         total.moe = moe) %>%
  left_join(hhByhhTypeDF) %>%
  mutate(prop = estimate/total,
         prop.moe = moe_ratio(
           estimate, total,
           moe, total.moe
         )) %>%
  filter(type != "Total" & !is.na(hh_size)) %>%
  mutate(hi = prop + prop.moe,
         lo = prop - prop.moe,
         hh_size = factor(hh_size))

p3 <- plotDF %>%  
  ggplot(aes(x = Year, y = prop, ymin = lo, ymax = hi, 
             group = hh_size, shape = source)) +
  geom_line(data = plotDF %>% filter(source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~type) +
  labs(y="", color = "Household by household size in King County",
       title = "Household by household size and type") +
  guides(fill=FALSE)

ggsave(p3, filename =  paste0(out_dir, "Prop_hh_by_hh_size_and_type_kc.png"),
       width = 9, height = 6)

