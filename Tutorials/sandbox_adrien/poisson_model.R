########################################################
###
### Poisson model for estimating average hh size over time
### Updated August 6th, 2021

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

main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")
# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)


###########################
## County-level analysis ##
###########################

#########################
## 1. Pulling the data ##
#########################

# 1.1 Get census data
years <- c(2000,2010)
censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "county",
    variables = c("H001001"),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(housing_units = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "county",
        variables = c("H010001"),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(population_total = value) %>%
      dplyr::select(-variable)
  )

# 1.2 Get ACS data
years <- c(2005:2019)
acsDF <- bind_rows(lapply(years, function(x){
  get_acs(
    "county",
    variables = c("B25001_001"),
    state = "WA",
    county = "King",
    survey = "acs1",
    moe_level = 95,
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(housing_units = estimate) %>%
  dplyr::select(-c(variable,moe)) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_acs(
        "county",
        variables = c("B01003_001"),
        state = "WA",
        county = "King",
        survey = "acs1",
        moe_level = 95,
        year = x,
        cache_table = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(population_total = estimate) %>%
      dplyr::select(-c(variable))
  )

# 1.3 Joining the ACS and Census data together
totDF <- censusDF %>%
  mutate(source = "Census",
         lo = population_total,
         hi = population_total) %>%
  rbind(
    acsDF %>%
      mutate(source = "ACS",
             lo = population_total - moe,
             hi = population_total + moe) %>%
      dplyr::select(-moe)
  )

######################
## 2. Poisson model ##
######################
model <- population_total ~ Year
mod0 <- glm(model,
    data = totDF,
    family = poisson,
    offset = log(housing_units))

#define new observation
predDF <- data.frame(Year=(min(totDF$Year)):(max(totDF$Year)+10),
                     housing_units= c(rep(totDF$housing_units[totDF$Year==min(totDF$Year)],
                                          5),rep(totDF$housing_units[totDF$Year==max(totDF$Year)],
                                        length((max(totDF$Year)+1):(max(totDF$Year)+10)))))

#use model to predict value of am
predDF$est_pop <- predict(mod0, predDF, type="response",se.fit=T)$fit
predDF$est_pop_lo <- predict(mod0, predDF, type="response",se.fit=T)$fit -
  2*predict(mod0, predDF, type="response",se.fit=T)$se.fit
predDF$est_pop_hi <- predict(mod0, predDF, type="response",se.fit=T)$fit +
  2*predict(mod0, predDF, type="response",se.fit=T)$se.fit

ggplot(data = predDF, aes(x = Year, y = est_pop/housing_units)) +
  geom_line() +
  geom_line(size = 1, color ="#0d4e93") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_ribbon(aes(ymin=est_pop_lo/housing_units, ymax=est_pop_hi/housing_units),
              fill = "#0d4e93", linetype=2, alpha=0.1) +
  geom_point(data = totDF, aes(x=Year, y=population_total/housing_units,
                               shape = source, color = source))+
  # geom_segment(data = totDF, aes(x=Year, y=lo/housing_units, xend=Year, yend=hi/housing_units, color = source))+
  labs(y = "Average Household Size", title = "Average household size in King County over time") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        legend.position = c(.95, .15),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())



grid<-expand.grid(GEOID=unique(totDF$GEOID),Year=c(2000:2020))

All<-totDF%>%right_join(grid)%>%
  arrange(Year)
dim(All)

All$housing_units[All$Year %in% 2001:2004] <- approx(x=All$Year,
                                                     xout = 2001:2004,
                                                     y = All$housing_units)$y
All$housing_units[All$Year %in% 2020] <- approx(x=All$Year,
                                                     xout = 2020,
                                                    rule = 2,
                                                     y = All$housing_units)$y
prior.iid = c(0.5,0.008)
All$period.id <- as.numeric(as.factor(All$Year))

mod <- inla(population_total ~ 1 + f(period.id, model = "ar1", param = prior.iid) +
              offset(log(housing_units)), 
            family = "poisson",
            data =All,
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)
All$mean<-mod$summary.fitted.values$`mean`/All$housing_units
All$up<-mod$summary.fitted.values$`0.975quant`/All$housing_units
All$low<-mod$summary.fitted.values$`0.025quant`/All$housing_units

ggplot(data = All, aes(x = Year, y = mean)) +
  geom_line() +
  geom_line(size = 1, color ="#0d4e93") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_ribbon(aes(ymin=low, ymax=up),
              fill = "#0d4e93", linetype=2, alpha=0.1) +
  geom_point(data = All, aes(x=Year, y=population_total/housing_units,
                               shape = source, color = source))+
  # geom_segment(data = totDF, aes(x=Year, y=lo/housing_units, xend=Year, yend=hi/housing_units, color = source))+
  labs(y = "Average Household Size", title = "Average household size in King County over time") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        legend.position = c(.85, .1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())



m0 <- inla(formula, family = "poisson",
           E=housing_units,
           control.compute = list(config = T, dic = T, cpo = T),
           data = totDF)
summary(m0)

m0$summary.fixed %>%
  as.data.frame() %>%
  mutate(Covariate = row.names(.)) %>%
  filter(Covariate != "(Intercept)") %>%
  ggplot(aes(x=Covariate, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  coord_flip() +
  geom_hline(yintercept=0, linetype=2) +
  labs(y="Estimate")


formula2 <- update(formula, ~. - Year + f(Year, model = "rw1"))

m01 <- inla(formula2, family = "poisson",
           E=housing_units,
           control.compute = list(config = T, dic = T, cpo = T),
           data = totDF)
summary(m1)



#################################
## Census-tract-level analysis ##
#################################

censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "tract",
    variables = c("H001001"),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(housing_units = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H010001"),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(population_total = value) %>%
      dplyr::select(-variable)
  )