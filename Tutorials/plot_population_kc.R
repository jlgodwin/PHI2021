########################################################
###
### Plots of population gains in KC
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

# loading estimates
population_kc <- readRDS(file = paste0(out_dir,"population_over_time_kc.RDS"))

############################
## Average household size ##
############################

plotDF <- population_kc %>%
  data.table() %>%
  melt(id.vars = c('GEOID',"NAME",'Year'),
       measure.vars = c('total_population',
                        'hh_population','gq_population')) %>%
  mutate(
    population = case_when(
      variable == "total_population" ~ "total population",
      variable == "hh_population" ~ "household population",
      variable == "gq_population" ~ "group quarters population"
    ),
    population = factor(population,
                        levels = c("total population","household population",
                                   "group quarters population"))
  ) 

p1<-plotDF %>%  
  ggplot(aes(x = Year, y = value)) +
  geom_line(data = plotDF) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~population, scales = 'free')+
  labs(y="",
       title = "Population of King County") +
  guides(fill=FALSE)

ggsave(p1, filename =  paste0(out_dir, "population_kc_over_time.png"),
       width = 9, height = 6)
