########################################################
###
### Analyzing individual-level data from ACS 1-year IPUMS data
### Updated August 18, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(dplyr)
library(survey)
library(tidyverse)


# constants
main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "Code/PHI2021/household_size/")

# loading functions
source(paste0(code_dir,"../Tutorials/ipums_acs_recode.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_proportions.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_tot.R"))
# loading data
ipums <- readRDS(file = paste0(code_dir, "1970_05_IPUMS.RDS"))

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
  dplyr::select(YEAR, STRATA, CLUSTER, HHWT, hh_type, tenure, multigen,hh_size) %>%
  filter(!is.na(hh_type)) %>%
  distinct()

#################################################
# -- set up empty objects for survey results -- #
#################################################

both_merge<-expand.grid(unique(hhDF$hh_type),unique(hhDF$tenure), unique(hhDF$multigen))#
names(both_merge)<-c("hh_type","tenure","multigen")

both_merge$hh_type<-as.character(both_merge$hh_type)
both_merge$tenure<-as.character(both_merge$tenure)
both_merge$multigen<-as.character(both_merge$multigen)

hh_type_merge<-data.frame(hh_type=unique(hhDF$hh_type),
                          tenure="ALL",multigen="ALL")

hh_tenure_merge<-data.frame(hh_type="ALL",
                            tenure=unique(hhDF$tenure), multigen="ALL")
multigen_merge<-data.frame(hh_type="ALL",
                           hh_tenure="ALL",
                           multigen=unique(hhDF$multigen))

##########################################
# -- Set up the survey design object -- #
##########################################

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 1980))

###############################################################
## Compute population proportions for vars  ###################
###############################################################

final_1980 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 1980))

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 1990 & !(STRATA %in% c(38,42,79,115))))

final_1990 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 1990 & STRATA != 38))

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 2000& !(STRATA %in% c(50,87,95,107,111,119,122,131))))

final_2000 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 2000 & !(STRATA %in% c(50,87,95,107,111,119,122,131))))
final_1980$year<-1980
final_1990$year<-1990
final_2000$year<-2000

final <- rbind(
  final_1980,
  final_1990,
  final_2000
)

final$survey <- "ipums_1980"

write_csv(as.data.frame(final),
          paste0(out_dir,"Processed_1980_2000_IPUMS.csv"))

total_1980 <- compute_survey_proportions(hhDF %>%
                                           filter(YEAR == 1980))


total_1980 <- compute_survey_tot(hhDF %>%
                                           filter(YEAR == 1980))

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 1990 & !(STRATA %in% c(38,42,79,115))))

total_1990 <- compute_survey_tot(hhDF %>%
                                           filter(YEAR == 1990 & STRATA != 38))

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF %>%
                            filter(YEAR == 2000& !(STRATA %in% c(50,87,95,107,111,119,122,131))))

total_2000 <- compute_survey_tot(hhDF %>%
                                           filter(YEAR == 2000 & !(STRATA %in% c(50,87,95,107,111,119,122,131))))

total_1980$year<-1980
total_1990$year<-1990
total_2000$year<-2000

total <- rbind(
  total_1980,
  total_1990,
  total_2000
)

write_csv(as.data.frame(total),
          paste0(out_dir,"Total_1980_2000_IPUMS.csv"))
