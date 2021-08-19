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

code_dir <- paste0(main_dir, "PHI2021/household_size/")
# loading estimates
hhByhhSizeDF <- readRDS(file = paste0(code_dir, "hh_by_hh_size_and_tenure_kc.RDS"))


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
  filter(hh_size == 1) %>%
  group_by(Year, source) %>%
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

All <-dat
## Modelling trends in average household size 
grid<-expand.grid(Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod <- inla(logit_prop ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)

# All <- All %>%
#   mutate_at(c('mean','up','low'), function(x) 100*x)
gg <- ggplot(All, aes(x = Year, y = mean)) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(.2, .4)) +
  scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000, 2020)) +
  geom_ribbon(data = All,
              aes(ymin=low, ymax=up), linetype=2, alpha=0.1) +
  geom_point(data = All, aes(x=Year, y=prop,  color = source))+
  geom_segment(data = All,
               aes(x=Year, y=prop-prop.moe,
                   xend=Year, yend=prop+prop.moe,
                   color = source))+
  # geom_rug(data=df) +
  labs(x ='Year', y = 'As a proportion of total households in KC',
       title = "% 1-person household in King County") +
  theme_bw() +
  scale_color_manual(values=c("#2EC4B6", "#E71D36"),
                     limits = c('ACS','Census')) +
  guides(fill=FALSE) +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(
          size=14),
        axis.text.x = element_text(size=14),
        legend.position = c(.95, .75),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
print(gg)

ggsave(gg, filename = paste0(code_dir,"1_person_hh_prev_kc.png"),
       width = 9, height = 6)

######################
## 2-person hh ##
#################

regDF <- hhByhhSizeDF %>%
  filter(hh_size == 2) %>%
  group_by(Year, source) %>%
  summarize(
    prop = sum(prop),
    prop.moe = moe_sum(prop.moe)
  )

dat<-regDF%>%mutate(prop = as.numeric(prop),
                    se.prop=as.numeric(prop.moe/1.96),
                    logit_prop=logit_outcome(prop,thresh=threshold),
                    var_logit_prop= var_logit_outcome(prop,se.prop,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prop),
                                  1/var_logit_prop,
                                  20000))


All <-dat
## Modelling trends in average household size 
grid<-expand.grid(Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod <- inla(logit_prop ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)

# All <- All %>%
#   mutate_at(c('mean','up','low'), function(x) 100*x)
gg <- ggplot(All, aes(x = Year, y = mean)) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(.2, .4)) +
  scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000, 2020)) +
  geom_ribbon(data = All,
              aes(ymin=low, ymax=up), linetype=2, alpha=0.1) +
  geom_point(data = All, aes(x=Year, y=prop,  color = source))+
  geom_segment(data = All,
               aes(x=Year, y=prop-prop.moe,
                   xend=Year, yend=prop+prop.moe,
                   color = source))+
  # geom_rug(data=df) +
  labs(x ='Year', y = 'As a proportion of total households in KC',
       title = "% 2-person household in King County") +
  theme_bw() +
  scale_color_manual(values=c("#2EC4B6", "#E71D36"),
                     limits = c('ACS','Census')) +
  guides(fill=FALSE) +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(
          size=14),
        axis.text.x = element_text(size=14),
        legend.position = c(.95, .15),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
print(gg)
ggsave(gg, filename = paste0(code_dir,"2_person_hh_prev_kc.png"),
       width = 9, height = 6)
######################
## 2+ - person hh ##
#################

regDF <- hhByhhSizeDF %>%
  filter(hh_size > 2) %>%
  group_by(Year, source) %>%
  summarize(
    prop = sum(prop),
    prop.moe = moe_sum(prop.moe)
  )

dat<-regDF%>%mutate(prop = as.numeric(prop),
                    se.prop=as.numeric(prop.moe/1.96),
                    logit_prop=logit_outcome(prop,thresh=threshold),
                    var_logit_prop= var_logit_outcome(prop,se.prop,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prop),
                                  1/var_logit_prop,
                                  20000))


All <-dat
## Modelling trends in average household size 
grid<-expand.grid(Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod <- inla(logit_prop ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)

# All <- All %>%
#   mutate_at(c('mean','up','low'), function(x) 100*x)
gg <- ggplot(All, aes(x = Year, y = mean)) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(.2, .4)) +
  scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000, 2020)) +
  geom_ribbon(data = All,
              aes(ymin=low, ymax=up), linetype=2, alpha=0.1) +
  geom_point(data = All, aes(x=Year, y=prop,  color = source))+
  geom_segment(data = All,
               aes(x=Year, y=prop-prop.moe,
                   xend=Year, yend=prop+prop.moe,
                   color = source))+
  # geom_rug(data=df) +
  labs(x ='Year', y = 'As a proportion of total households in KC',
       title = "% 3+-person household in King County") +
  theme_bw() +
  scale_color_manual(values=c("#2EC4B6", "#E71D36"),
                     limits = c('ACS','Census')) +
  guides(fill=FALSE) +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(
          size=14),
        axis.text.x = element_text(size=14),
        legend.position = c(.95, .15),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
print(gg)
ggsave(gg, filename = paste0(code_dir,"3_person_hh_prev_kc.png"),
       width = 9, height = 6)