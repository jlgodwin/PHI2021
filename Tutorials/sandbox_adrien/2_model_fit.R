########################################################
###
### Analysis of average hh size using ACS and census data
### Updated August 9, 2021

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
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

totDF <- readRDS(file = paste0(out_dir, "kc_survey_data.RDS"))
hh_by_size <- readRDS(file = paste0(out_dir, "kc_hh_by_size.RDS"))

# getting standard error, log-transforming outcome and getting its SE
# using the Delta Method

totDF <- totDF %>%
  mutate(housing_units.se = ifelse(!is.na(housing_units.moe),housing_units.moe/1.96,0),
         log.housing_units = log(housing_units),
         log.housing_units.se = (1/housing_units)*housing_units.se,
         prec = ifelse(log.housing_units.se!=0,1/(log.housing_units.se),1)) %>%
  filter(Year != 2005) %>%
  filter(Year != 2010 | source != "acs")

grid<-expand.grid(GEOID=unique(totDF$GEOID),Year=c(2000:2020))

All<-totDF%>%right_join(grid)
dim(All)
All$period.id<-as.numeric(as.factor(All$Year))

All$household_population[All$Year %in% 2020:2040] <- All$household_population[All$Year == 2019]

prior.iid = c(0.5,0.008)
mod <- inla(housing_units ~ Year, #f(period.id, model = "rw1", param = prior.iid), 
            scale = prec,
            data =All,
            E = household_population,
            family = "poisson",
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)
All$mean<-mod$summary.fitted.values$`mean`
All$up<-mod$summary.fitted.values$`0.975quant`
All$low<-mod$summary.fitted.values$`0.025quant`

gg <- ggplot(All, aes(x = Year, y = 1/mean)) +
  geom_line(size = 1) +
  # scale_y_continuous(breaks = c(0,20,40,60,80,100), expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(2000,2030,5), limits = c(2000, 2020)) +
  geom_ribbon(data = All,
              aes(ymin=1/low, ymax=1/up), linetype=2, alpha=0.1) +
  geom_point(data = All, aes(x=Year, y=household_population/housing_units,  color = source))+
  geom_segment(data = All,
               aes(x=Year, y=(household_population/housing_units)-moe_ratio(household_population,housing_units,household_population.moe,housing_units.moe),
                   xend=Year, yend=(household_population/housing_units)+moe_ratio(household_population,housing_units,household_population.moe,housing_units.moe),
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
        legend.position = c(.95, .75),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
print(gg)

