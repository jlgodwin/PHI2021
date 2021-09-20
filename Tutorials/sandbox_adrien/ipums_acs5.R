########################################################
###
### Analyzing individual-level data from ACS 5-year IPUMS data
### Updated August 18, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(dplyr)
library(survey)
library(tidyverse)


# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")

# loading functions
source(paste0(code_dir,"../Tutorials/ipums_acs_recode.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_proportions.R"))
# loading data
ipums <- readRDS(file = paste0(code_dir, "kc_ACS5_IPUMS.RDS"))

# recoding data
hhDF <- ipums_recode_acs5(ipums) %>%
  filter(household == "household") %>%
  left_join(ipums_recode_acs5(ipums) %>%
              filter(household == "household") %>%
              group_by(CLUSTER) %>%
              summarize(
                hh_size = n() # verify if PERNUM is indeed the number of people per hh
              )) %>%
  mutate(
    hh_size = ifelse(hh_size > 7, 7, hh_size) # censoring hh size to 7
  ) %>%
  dplyr::select(YEAR, STRATA, CLUSTER, HHWT, hh_type, tenure, hh_size, multigen) %>%
  filter(!is.na(hh_type)) %>%
  distinct()

#################################################
# -- set up empty objects for survey results -- #
#################################################

both_merge<-expand.grid(unique(hhDF$hh_type),unique(hhDF$tenure), unique(hhDF$multigen))
names(both_merge)<-c("hh_type","tenure","multigen")

both_merge$hh_type<-as.character(both_merge$hh_type)
both_merge$tenure<-as.character(both_merge$tenure)
both_merge$multigen<-as.character(both_merge$multigen)

hh_type_merge<-data.frame(hh_type=unique(hhDF$hh_type),
                          tenure="ALL",multigen="ALL")

hh_tenure_merge<-data.frame(hh_type="ALL",
                            tenure=unique(hhDF$tenure),
                            multigen="ALL")
multigen_merge<-data.frame(hh_type="ALL",
                            hh_tenure="ALL",
                            multigen=unique(hhDF$multigen))
###############################
# -- start analyzing survey --#
###############################

##########################################
# -- Set up the survey design object -- #
##########################################

my.svydesign <- svydesign(id= ~CLUSTER,
                              strata=~STRATA,
                              weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 2009))

###############################################################
## Compute population proportions for vars  ###################
###############################################################


final_2009 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 2009))
my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 2014))
final_2014 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 2014))
my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 2019))
final_2019 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 2019))
final_2009$year<-2009
final_2014$year<-2014
final_2019$year<-2019

final <- rbind(
  final_2009,
  final_2014,
  final_2019
)

final$survey <- "acs5"

write_csv(as.data.frame(final),
          paste0(out_dir,"Processed_ACS_5_IPUMS.csv"))
