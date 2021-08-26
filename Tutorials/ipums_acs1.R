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
source(paste0(code_dir,"../Tutorials/map_main_theme.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_proportions.R"))
# loading data
ipums <- readRDS(file = paste0(code_dir, "kc_ACS1_IPUMS.RDS"))

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
  dplyr::select(YEAR, STRATA, CLUSTER,HHWT, hh_type, tenure,multigen, hh_size) %>%
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

###############################
# -- start analyzing survey --#
###############################

my.svydesign <- svydesign(id= ~CLUSTER,
                          strata=~STRATA,
                          weights= ~HHWT, data=hhDF)

tbl <- svytable(~ hh_size + tenure + hh_type + multigen + YEAR, my.svydesign)


##########################################
# -- Set up the survey design object -- #
##########################################



###############################################################
## Compute population proportions for vars  ###################
###############################################################
years <- 2005:2019
final <- NULL
for (t in years) {
  my.svydesign <- svydesign(id= ~CLUSTER,
                            strata=~STRATA,
                            weights= ~HHWT, data=hhDF %>%
                              filter(YEAR == t))
  final <- rbind(final,compute_survey_proportions(hhDF %>%
                               filter(YEAR == t)) %>%
    mutate(year = t))
  
  final_total <- rbind(final,compute_survey_tot(hhDF %>%
                                                    filter(YEAR == t)) %>%
                   mutate(year = t))
}

final$survey <- "acs1"
final_total$survey <- "acs1"
write_csv(as.data.frame(final),
          paste0(out_dir,"Processed_ACS_1_IPUMS.csv"))

write_csv(as.data.frame(final_total),
          paste0(out_dir,"Totals_ACS_1_IPUMS.csv"))

plotDF <- final %>%
  dplyr::select(year, hh_type, tenure, multigen, `as.numeric(hh_size == 7)`,
                `as.numeric(hh_size == 6)`,`as.numeric(hh_size == 5)`,
                `as.numeric(hh_size == 4)`,`as.numeric(hh_size == 3)`,
                `as.numeric(hh_size == 2)`,`as.numeric(hh_size == 1)`) %>%
  data.table() %>%
  melt.data.table(id.vars = c('year','hh_type','tenure','multigen'))

## Plot 1: hh_size over time by tenure and household type
library(RColorBrewer)
plotDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  ggplot(aes(x = year, y = value,
             group = interaction(variable,tenure), shape = tenure)) +
  geom_line(data = plotDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL"),aes(color = variable)) +
  geom_point(aes(color = variable)) +
  # geom_ribbon(aes(fill = variable), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2005,2010,2015,2020)) +
  facet_wrap(~hh_type) +
  labs(y="", color = "Household by household size in King County",
       title = "Household by household size and tenure") +
  guides(fill=FALSE) + scale_color_brewer(palette = "Accent")


plotDF <- final_total %>%
  dplyr::select(year, hh_type, tenure, multigen, `as.numeric(hh_size == 7)`,
                `as.numeric(hh_size == 6)`,`as.numeric(hh_size == 5)`,
                `as.numeric(hh_size == 4)`,`as.numeric(hh_size == 3)`,
                `as.numeric(hh_size == 2)`,`as.numeric(hh_size == 1)`) %>%
  data.table() %>%
  melt.data.table(id.vars = c('year','hh_type','tenure','multigen')) %>%
  mutate(
    hh_size  = case_when(
      variable == "as.numeric(hh_size == 7)" ~ 7,
      variable == "as.numeric(hh_size == 6)" ~ 6,
      variable == "as.numeric(hh_size == 5)" ~ 5,
      variable == "as.numeric(hh_size == 4)" ~ 4,
      variable == "as.numeric(hh_size == 3)" ~ 3,
      variable == "as.numeric(hh_size == 2)" ~ 2,
      variable == "as.numeric(hh_size == 1)" ~ 1
    ),
    hh_size = factor(hh_size)
  ) 

## Plot 1: hh_size over time by tenure and household type
library(RColorBrewer)
p1 <- plotDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  ggplot(aes(x = year, y = value,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = plotDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL"),aes(color = hh_size),
            size = 1.2) +
  geom_point(aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2005,2010,2015,2020)) +
  facet_wrap(~hh_type) +
  labs(y="Number of households", x = "Year", color = "Size-of-household", shape = "Tenure",
       title = "Number of households in King County by size-of-household, type, and tenure") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent") +
  map_theme_main

ggsave(p1, filename = paste0(code_dir, "HH_by_size_tenure_type.png"), width = 9, height = 6)
