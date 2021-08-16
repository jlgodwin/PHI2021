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
main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

avgHHsizeDF <- readRDS(file = paste0(out_dir, "average_hh_size_by_ownership_kc.RDS"))

## transforming the data to account for uncertainty later
totDF <- avgHHsizeDF  %>%
  mutate(hhs.se = ifelse(!is.na(moe),moe/1.96,0.00000000001),
         prec = 1/(hhs.se)) %>%
  filter(Year != 2010 | source != "acs")

## Modelling trends in average household size 
grid<-expand.grid(GEOID=unique(totDF$GEOID),Year=c(2000:2020))

All<-totDF%>%right_join(grid)
dim(All)
All$period.id<-as.numeric(as.factor(All$Year))

prior.iid <- c(0.5,0.008)
mod <- inla(hhs ~ Year, #f(period.id, model = "rw1", param = prior.iid), 
            scale = prec,
            data =All %>%
              filter(type == "Total"),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)
All$mean<-mod$summary.fitted.values$`mean`
All$up<-mod$summary.fitted.values$`0.975quant`
All$low<-mod$summary.fitted.values$`0.025quant`

# gg <- ggplot(All, aes(x = Year, y = 1/mean)) +
#   geom_line(size = 1) +
#   # scale_y_continuous(breaks = c(0,20,40,60,80,100), expand = c(0, 0), limits = c(0, 100)) +
#   scale_x_continuous(breaks = seq(2000,2030,5), limits = c(2000, 2020)) +
#   geom_ribbon(data = All,
#               aes(ymin=1/low, ymax=1/up), linetype=2, alpha=0.1) +
#   geom_point(data = All, aes(x=Year, y=household_population/housing_units,  color = source))+
#   geom_segment(data = All,
#                aes(x=Year, y=(household_population/housing_units)-moe_ratio(household_population,housing_units,household_population.moe,housing_units.moe),
#                    xend=Year, yend=(household_population/housing_units)+moe_ratio(household_population,housing_units,household_population.moe,housing_units.moe),
#                    color = source))+
#   # geom_rug(data=df) +
#   labs(x ='Year', y = 'Average household size',
#        title = "King County") +
#   theme_bw() +
#   scale_color_manual(values=c("#FFA500", "#800080")) +
#   guides(fill=FALSE) +
#   theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
#         axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
#         axis.title = element_text(size = 16, face = "bold"),
#         axis.text.y = element_text( 
#           size=14),
#         axis.text.x = element_text(size=14),
#         legend.position = c(.95, .75),
#         legend.justification = c("right", "bottom"),
#         legend.box.just = "right",
#         legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
# print(gg)
