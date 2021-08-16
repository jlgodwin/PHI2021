rm(list=ls())

## folders

out_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/output/"

## Helper functions

## Likelihood for 4 category ordered probit
llk.oprobit4 <- function(param, x, y) {
  # preliminaries
  os <- rep(1, nrow(x))
  x <- cbind(os, x)  
  b <- param[1:ncol(x)]
  t2 <- param[(ncol(x)+1)]
  t3 <- param[(ncol(x)+2)]
  
  # probabilities and penalty function
  xb <- x%*%b
  p1 <- log(pnorm(-xb))
  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))
  if (t3<=t2) p3 <- -((t2-t3)*10000)    # penalty to keep t3>t2
  else p3 <- log(pnorm(t3-xb)-pnorm(t2-xb))     
  p4 <- log(1-pnorm(t3-xb)) 
  
  # -1 * log likelihood (optim is a minimizer)
  -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))
}

## Load libraries
library(MASS)
library(simcf)
library(tile)
library(RColorBrewer)
library(tidyverse)
library(tidycensus)
## Nice colors
brewer <- brewer.pal(9, "Set1")
red <- brewer[1]
blue <- brewer[2]
green <- brewer[3]
purple <- brewer[4]
orange <- brewer[5]
nicegray <- "gray45"

## Load data
years <- c(2009,2014,2019)
hh_by_size_acs <- bind_rows(lapply(years, function(x){
  get_acs(
    "tract",
    table = c("B11016"
    ),
    # summary_var = "B11016_01",
    state = "WA",
    county = "King",
    survey = "acs5",
    moe_level = 95,
    year = x,
    cache_table = TRUE,
    geometry = FALSE) %>%
    mutate(Year = x)})) %>%
  mutate(hh_size = case_when(
    # variable == "B11016_001" ~ 0, # total
    variable == "B11016_003" ~ 2,
    variable == "B11016_004" ~ 3,
    variable == "B11016_005" ~ 4,
    variable == "B11016_006" ~ 5,
    variable == "B11016_007" ~ 6,
    variable == "B11016_008" ~ 7,
    variable == "B11016_010" ~ 1,
    variable == "B11016_011" ~ 2,
    variable == "B11016_012" ~ 3,
    variable == "B11016_013" ~ 4,
    variable == "B11016_014" ~ 5,
    variable == "B11016_015" ~ 6,
    variable == "B11016_016" ~ 7,
  ),
  hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>% 
  group_by(GEOID, Year, hh_size) %>%
  summarize(
    estimate = sum(estimate),
    moe = moe_sum(moe,estimate)
  )

## reducing hh size to 4 groups for now
hh_by_size_4_gp <- hh_by_size_acs %>%
  mutate(hh_size_4_gp = case_when(
    hh_size == 1 ~ 1,
    hh_size == 2 ~ 2,
    hh_size == 3 ~ 3,
    hh_size %in% 4:7 ~ 4,
  ))


## loading location data
loc <- readRDS(file = paste0(out_dir, "loc.RDS"))

## loading census-tract level covariates

covar <- readRDS(file = paste0(out_dir, "covar.RDS")) %>%
  filter(year %in% years) %>%
  dplyr::select(mtract, year, poverty, rent, unemployed, 
                ethn_hisp, race_black, race_white, race_native, race_asian,
                property_value_median, income_median)

## joining all the data together

totDF <- loc %>%
  left_join(covar) %>%
  mutate(GEOID = as.character(tractfips)) %>%
  full_join(hh_by_size_4_gp %>%
              group_by(GEOID, Year, hh_size_4_gp) %>%
              summarize(
                estimate = sum(estimate)
              ))

regDF <- uncount(totDF, estimate)

y <- regDF$hh_size_4_gp   

x <- cbind(totDF$year, totDF$race_black, totDF$race_white,
           totDF$race_native, totDF$race_asian, totDF$ethn_hisp)

model <- hh_size_4_gp ~ year + race_black + race_white + race_native +
  race_asian + ethn_hisp

# Use optim directly to get MLE
ls.result <- lm(model, data=totDF)   # use ls estimates as starting values
stval <- c(coef(ls.result),1,2)          # initial guesses
oprobit.res <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hessian=TRUE)
pe77 <- oprobit.res77$par                # point estimates
vc77 <- solve(oprobit.res77$hessian)     # var-cov matrix
se77 <- sqrt(diag(vc77))                 # standard errors
ll77 <- -oprobit.res77$value             # likelihood at maximum

# Use MASS::polr to do ordered probit
regDF$hh.f <- factor(regDF$hh_size_4_gp, labels=c("1",
                                                   "2",
                                                   "3",
                                                   "4+"))
glm.res <- polr(hh.f ~ year + poverty + rent + income_median, data=regDF,
                  method="probit", na.action=na.omit)

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims, pe77, vc77)   