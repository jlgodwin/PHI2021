########################################################
###
### Plots of trends in KC by tenure status
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
avgHHsizeDF <- readRDS(file = paste0(out_dir, "average_hh_size_by_ownership_kc.RDS"))
hhByhhSizeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_tenure_kc.RDS"))

############################
## Average household size ##
############################

plotDF <- avgHHsizeDF %>%
  mutate(hi = hhs + moe,
         lo = hhs - moe)
  
p1 <- plotDF %>%  
  ggplot(aes(x = Year, y = hhs, ymin = lo, ymax = hi, 
             group = type, shape = source)) +
  geom_line(data = plotDF %>% filter(source == "ACS"),aes(color = type)) +
  geom_point(aes(color = type)) +
  geom_ribbon(aes(fill = type), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="", color = "Average household size in King County",
       title = "Average Household size by tenure") +
  guides(fill=FALSE)

ggsave(p1, filename =  paste0(out_dir, "average_hh_size_by_tenure_kc.png"),
       width = 9, height = 6)
#################################
## Household by household size ##
#################################

plotDF <- hhByhhSizeDF %>%
  mutate(hi = estimate + moe,
         lo = estimate - moe) %>%
  filter(
    tenure != "Total" & !is.na(hh_size)
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
  facet_wrap(~tenure) +
  labs(y="", color = "Household by household size in King County",
       title = "Household by household size and tenure") +
  guides(fill=FALSE)

ggsave(p2, filename =  paste0(out_dir, "hh_by_hh_size_and_tenure_kc.png"),
       width = 9, height = 6)

plotDF <- hhByhhSizeDF %>%
  filter(tenure == "Total") %>%
  dplyr::select(Year, source, estimate,moe) %>%
  rename(total = estimate,
         total.moe = moe) %>%
  left_join(hhByhhSizeDF) %>%
  mutate(prop = estimate/total,
         prop.moe = moe_ratio(
           estimate, total,
           moe, total.moe
         )) %>%
  filter(tenure != "Total" & !is.na(hh_size)) %>%
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
  facet_wrap(~tenure) +
  labs(y="", color = "Household by household size in King County",
       title = "Household by household size and tenure") +
  guides(fill=FALSE)

ggsave(p3, filename =  paste0(out_dir, "Prop_hh_by_hh_size_and_tenure_kc.png"),
       width = 9, height = 6)

