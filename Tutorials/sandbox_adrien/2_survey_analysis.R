########################################################
###
### Analysis of average hh size using ACS and census data
### Updated August 13, 2021

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
out_dir <- paste0(main_dir, "output/")

totDF <- readRDS(file = paste0(out_dir, "kc_survey_data.RDS"))
hh_by_size <- readRDS(file = paste0(out_dir, "kc_hh_by_size.RDS"))

