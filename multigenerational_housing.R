library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)
library(rgdal)
library(spdep)
library(survey)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(classInt)
library(data.table)
setwd("~/Desktop/PHI2021/Data")
ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

View(ddi$var_info)

# Geography
table(data$STATEFIP)

table(data$COUNTYFIP)
ddi$var_info %>%
  filter(var_name == "COUNTYFIP") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% data$COUNTYFIP)

# Time
table(data$YEAR)
table(data$MULTYEAR)

# Which ACS Sample?
table(data$SAMPLE)
ddi$var_info %>%
  filter(var_name == "SAMPLE") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% data$SAMPLE)

# Survey/sampling design variables

## HH number
length(table(data$SERIAL))
nrow(data)

## strata
table(data$STRATA)

## cluster: appears households ARE clusters-
## need to read more about how these relate
## to replicate weighting OR
## if I limit extract to one survey
## and dial up to maximum sample size
## if we would get more households per cluster
length(table(data$CLUSTER))
nrow(data)

### exploring multigenerational housing in KC
data <- data %>% 
      mutate(n=1,
                between_2005_2009 = ifelse(YEAR >= 2005 & YEAR < 2010, 1, 0),
                between_2010_2014 = ifelse(YEAR >= 2010 & YEAR < 2015, 1, 0),
                between_2015_2019 = ifelse(YEAR >= 2015 & YEAR < 2020, 1, 0),
                period = ifelse(between_2005_2009 == 1, "2005-2009", 
                                ifelse(between_2010_2014 == 1, "2010-2014", "2015-2019")))
                
##create KC multigenerational dataset for all years
KC_multgen <- data %>%
  filter((COUNTYFIP == 53 & GQ == 1 ) | (COUNTYFIP == 53 & GQ == 2)) %>% 
  select(YEAR, MULTYEAR, MULTGEN, MULTGEND, SAMPLE, SERIAL, STRATA, CLUSTER,
         HHWT, period)

design <- svydesign(id=~CLUSTER, strata=~STRATA, weight = ~HHWT, 
                    data = KC_multgen)

plot_data <- svyby(~n, ~period+MULTGEN, design, svytotal)

plot_data %>% mutate(test_stat = n/se,
                     CI_lower = n - qnorm(.95)*se, CI_upper = n + qnorm(.95)*se)


# create plot data set for multigenerational households 
multgen_line_plot_data <- KC_multgen %>% 
  select(MULTGEN, YEAR) 

multgen_line_plot <- ggplot(multgen_bar_plot_data) +
  geom_line(mapping = aes(x = YEAR, y = MULTGEN)) +
  labs(
    title = "Multigenerational Housing in King County Over Time",
    x = "Year", # x-axis label
    y = "") +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# multigenerational homes (general) in KC
table(KC$MULTGEN)
ddi$var_info %>%
  filter(var_name == "MULTGEN") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% KC$MULTGEN)

## detailed multigenerational dataset
table(KC$MULTGEND)
ddi$var_info %>%
  filter(var_name == "MULTGEND") %>%
  select(val_labels) %>%
  pluck(1) %>% as.data.frame() %>%
  filter(val %in% KC$MULTGEND)

