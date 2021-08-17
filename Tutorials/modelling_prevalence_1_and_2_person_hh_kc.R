########################################################
###
### Modelling hh by size at the county level and by tenure status
### Updated August 16, 2021
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
library(crsuggest)
library(tigris)
# constants
# main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

# loading estimates
hhByhhSizeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_tenure_kc.RDS"))


hhByhhSizeDF <-  hhByhhSizeDF %>%
  filter(tenure == "Total") %>%
  dplyr::select(GEOID, Year, source, estimate,moe) %>%
  rename(total = estimate,
         total.moe = moe) %>%
  left_join(hhByhhSizeDF) %>%
  mutate(prop = estimate/total,
         prop.moe = moe_ratio(
           estimate, total,
           moe, total.moe
         )) %>%
  dplyr::select(-variable) %>%
  filter(tenure != "Total")
# need to population-weight our estimates
yearsACS <- c(20005:2019)
yearsCensus <- c(2000,2010)

regDF <- hhByhhSizeDF %>%
  filter(hh_size %in% 1:2) %>%
  group_by(Year, source,tenure) %>%
  summarize(
    prop = sum(prop),
    prop.moe = moe_sum(prop.moe)
  )
  
logit_outcome<-function(indicator,thresh){
  
  logit.indicator<-ifelse(!is.na(indicator),logit(indicator),NA)
  logit.indicator<-ifelse(indicator<thresh | indicator>(1-thresh),NA,logit.indicator)
  logit.indicator
}

var_logit_outcome<-function(indicator,se,thresh){
  var_logit<-(se^2)/(indicator^2*(1-indicator)^2)
  var_logit<-ifelse(se<thresh,NA, var_logit)
  var_logit
}

# expit and logit

logit<-function(x){
  log(x/(1-x))
}


expit<-function(x){
  
  exp(x)/(1+exp(x))
}

# Add Columns for SAE #
threshold<-0.00001
dat<-regDF%>%mutate(prop = as.numeric(prop),
                    se.prop=as.numeric(prop.moe/1.96),
                    logit_prop=logit_outcome(prop,thresh=threshold),
                    var_logit_prop= var_logit_outcome(prop,se.prop,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prop),
                                  1/var_logit_prop,
                                  20000))

## Starting with all the data
All <-dat %>%
  filter(tenure == "Owner")
## Modelling trends in average household size 
grid<-expand.grid(Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id<-as.numeric(as.factor(All$Year))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prop ~ 
              f(period.id, model = "rw1", param = prior.iid), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)
