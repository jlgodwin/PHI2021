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
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")

# loading functions
source(paste0(code_dir,"../Tutorials/ipums_acs_recode.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_proportions.R"))
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
  dplyr::select(YEAR, STRATA, CLUSTER, HHWT, hh_type, tenure, hh_size) %>%
  filter(!is.na(hh_type)) %>%
  distinct()

#################################################
# -- set up empty objects for survey results -- #
#################################################

both_merge<-expand.grid(unique(hhDF$hh_type),unique(hhDF$tenure))#, unique(hhDF$hh_size))
names(both_merge)<-c("hh_type","tenure")#,"hh_size")

both_merge$hh_type<-as.character(both_merge$hh_type)
both_merge$tenure<-as.character(both_merge$tenure)
# both_merge$hh_size<-as.character(both_merge$hh_size)

hh_type_merge<-data.frame(hh_type=unique(hhDF$hh_type),
                          tenure="ALL")#,hh_size="ALL")

hh_tenure_merge<-data.frame(hh_type="ALL",
                            tenure=unique(hhDF$tenure))#,
# hh_size="ALL")
# hh_size_merge<-data.frame(hh_type="ALL",
#                             hh_tenure="ALL",
#                             hh_size=unique(hhDF$hh_size))
###############################
# -- start analyzing survey --#
###############################

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
