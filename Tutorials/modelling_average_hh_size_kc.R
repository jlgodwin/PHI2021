########################################################
###
### Analysis of average hh size using ACS and census data
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

# constants
# main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")
avgHHsizeDF <- readRDS(file = paste0(code_dir, "average_hh_size_by_ownership_kc.RDS"))

## transforming the data to account for uncertainty later
totDF <- avgHHsizeDF  %>%
  mutate(hhs.se = ifelse(!is.na(moe),moe/1.96,0.000001),
         prec = 1/((hhs.se)^2)) %>%
  filter(Year != 2010 | source != "ACS")

## Modelling trends in average household size 
grid<-expand.grid(GEOID=unique(totDF$GEOID),Year=c(2000:2020))

All<-totDF%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))
pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)
prior.iid <- c(0.5,0.008)
mod <- inla(hhs ~  
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1), 
            scale = prec,
            data =All %>%
              filter(type == "Total"),
            control.predictor = list(compute = TRUE, link = 1),
            control.inla = list(strategy = "adaptive", int.strategy = "auto"))

summary(mod)
total <- All %>%
  filter(type == "Total") 

total$mean<-mod$summary.fitted.values$`mean`
total$up<-mod$summary.fitted.values$`0.975quant`
total$low<-mod$summary.fitted.values$`0.025quant`

gg <- ggplot(total, aes(x = Year, y = mean)) +
  geom_line(size = 1) +
  # scale_y_continuous(breaks = c(0,20,40,60,80,100), expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000, 2020)) +
  geom_ribbon(data = total,
              aes(ymin=low, ymax=up), linetype=2, alpha=0.1) +
  geom_point(data = total, aes(x=Year, y=hhs,  color = source))+
  geom_segment(data = total,
               aes(x=Year, y=hhs-moe,
                   xend=Year, yend=hhs+moe,
                   color = source))+
  # geom_rug(data=df) +
  labs(x ='Year', y = 'Average household size',
       title = "King County") +
  theme_bw() +
  scale_color_manual(values=c("#FFA500", "#800080")) +
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

prior.iid <- c(0.5,0.008)
Renter <- All %>%
  filter(type == "Renter") %>%
  right_join(grid) 
mod <- inla(hhs ~  
                     f(period.id, model = "ar1", constr = TRUE,
                       param = prior.iid, hyper = hyperpc1) +
                     f(period.id2, model = "iid", hyper = hyperpc1), 
                   scale = prec,
                   data =Renter,control.compute=list(config = TRUE),
                   control.predictor = list(compute = TRUE, link = 1),
                   control.inla = list(strategy = "adaptive", int.strategy = "auto"))

summary(mod)


Renter$mean<-mod$summary.fitted.values$`mean`
Renter$up<-mod$summary.fitted.values$`0.975quant`
Renter$low<-mod$summary.fitted.values$`0.025quant`

prior.iid <- c(0.5,0.008)
Owner <- All %>%
  filter(type == "Owner") %>%
  right_join(grid)

mod2 <-inla(hhs ~  
                      f(period.id, model = "ar1", constr = TRUE,
                        param = prior.iid, hyper = hyperpc1) +
                      f(period.id2, model = "iid", hyper = hyperpc1), 
                    scale = prec,
                    data =Owner,
                    control.predictor = list(compute = TRUE, link = 1),
            control.compute=list(config = TRUE),
                    control.inla = list(strategy = "adaptive", int.strategy = "auto"))

summary(mod2)


Owner$mean<-mod2$summary.fitted.values$`mean`
Owner$up<-mod2$summary.fitted.values$`0.975quant`
Owner$low<-mod2$summary.fitted.values$`0.025quant`

