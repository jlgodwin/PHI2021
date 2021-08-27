########################################################
###
### Modelling average hh size at the HRA level and by tenure status
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
source(paste0(main_dir, "PHI2021/Tutorials/map_main_theme.R"))
# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)

# loading estimates
avgHHsizeDF <- readRDS(file = paste0(code_dir, "average_hh_size_by_ownership_ct.RDS"))

############################
## Average household size ##
############################

# need to population-weight our estimates
yearsACS <- c(2009,2014,2019)
yearsCensus <- c(2000,2010)

popDF <- bind_rows(lapply(yearsACS, function(x){
    get_acs("tract",
            table =  "B25008",
            state = "WA",
            county = "King",
            year = x,
            cache_table = TRUE,
            geometry = FALSE) %>%
      mutate(Year = x)})) %>%
      dplyr::select(-NAME) %>%
      rename(
        hhp = estimate,
        hhp.moe = moe
      ) %>%
      mutate(
        tenure = case_when(
          variable == "B25008_001" ~ "Total",
          variable == "B25008_002" ~ "Owner",
          variable == "B25008_003" ~ "Renter"
        ),
        source = "ACS"
      ) %>%
  rbind(
    bind_rows(lapply(yearsCensus, function(x){
        get_decennial("tract",
                table =  "H011",
                state = "WA",
                county = "King",
                year = x,
                cache_table = TRUE,
                geometry = FALSE) %>%
          mutate(Year = x)})) %>%
          dplyr::select(-NAME) %>%
          rename(
            hhp = value
          ) %>%
          mutate(
            tenure = case_when(
              variable == "H011001" ~ "Total",
              variable == "H011002" ~ "Owner",
              variable == "H011003" ~ "Renter",
            ),
            source = "Census",
            hhp.moe = NA
          ))

totDF <- popDF %>%
    left_join(avgHHsizeDF %>%
                rename(tenure = type))

load(paste0(out_dir, "tracts_to_hra.rda"))

# unlisting the tract to hra object (1 per acs5)
cw2000 <- tracts_to_hra$acs5_2009 %>%
  mutate(Year = 2000)
cw2009 <- tracts_to_hra$acs5_2009 %>%
  mutate(Year = 2009)
cw2010 <- tracts_to_hra$acs5_2009 %>%
  mutate(Year = 2010)
cw2014 <- tracts_to_hra$acs5_2014 %>%
  mutate(Year = 2014)
cw2019 <- tracts_to_hra$acs5_2019 %>%
  mutate(Year = 2019)

cw <- rbind(cw2000,cw2009, cw2010, cw2014, cw2019)

# population-weighted average hh size estimate

hraDF <- totDF %>%
  left_join(cw) %>%
  group_by(HRA2010v2_, Year, source, tenure) %>%
  summarize(
    av.hh.size = sum(hhs*hhp*prop.area, na.rm = T)/sum(hhp*prop.area, na.rm = T), # population_weighted,
    av.hh.size.moe = moe_ratio(sum(hhs*hhp*prop.area),
                               sum(hhp*prop.area),
                               moe_sum(moe = moe_product(hhs,
                                                         hhp,
                                                         moe,
                                                         hhp.moe)),
                               moe_sum(moe = hhp.moe))
  )

hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))
nb.r <- poly2nb(hra, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)

hraDF <- hraDF  %>%
  mutate(hhs.se = ifelse(!is.na(av.hh.size.moe),av.hh.size.moe/1.96,NA),
         prec = 1/((hhs.se)^2))

hraDF$prec[is.na(hraDF$prec)] <- max(hraDF$prec, na.rm = T)*20

####################
## All households ##
####################
All <- hraDF %>%
  filter(tenure == "Total" & !is.na(HRA2010v2_))

## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All$HRA2010v2_),Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))
All$dist.id2<-All$dist.id<-as.numeric(as.factor(All$HRA2010v2_))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod <- inla(av.hh.size ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1), 
            control.inla = list(strategy = "adaptive", int.strategy = "auto"), 
            verbose = FALSE)

summary(mod)

All$mean<-mod$summary.fitted.values$`mean`
All$up<-mod$summary.fitted.values$`0.975quant`
All$low<-mod$summary.fitted.values$`0.025quant`

write_csv(as.data.frame(All %>%
                          arrange(Year) %>%
                          mutate(variable = "Average household size") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/average_hh_size_ALL_hra.csv"))

plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year %in% c(2000,2010,2020)))
p1 <- ggplot(plot.df) + 
  geom_sf(aes(fill=mean)) +
  ggtitle("Average household size by HRA") +
  labs(fill = "Average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "YlGnBu") +
  facet_wrap(~ Year, nrow = 2)

ggsave(p1, filename = paste0(code_dir, "Report_plots/average_hh_size_ALL_2000_2020.png"),
       width = 9, height = 6)

## Difference in 4-person hh prevalence over time
plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2000) %>%
              rename(prev2000=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2000) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2000
              ))

p2 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size by HRA 2000-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_average_hh_size_ALL_2000_2020.png"),
       width = 9, height = 6)

plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2010) %>%
              rename(prev2010=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2010) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2010
              ))

p3 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size by HRA 2010-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p3, filename = paste0(code_dir, "Report_plots/Difference_in_average_hh_size_ALL_2010-2019.png"),
       width = 9, height = 6)

################################
## Subsetting to renters only ##
################################

All <- hraDF %>%
  filter(tenure == "Renter" & !is.na(HRA2010v2_))

## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All$HRA2010v2_),Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id2<-All$period.id<-as.numeric(as.factor(All$Year))
All$dist.id2<-All$dist.id<-as.numeric(as.factor(All$HRA2010v2_))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod <- inla(av.hh.size ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1), 
            control.inla = list(strategy = "adaptive", int.strategy = "auto"), 
            verbose = FALSE)

summary(mod)

All$mean<-mod$summary.fitted.values$`mean`
All$up<-mod$summary.fitted.values$`0.975quant`
All$low<-mod$summary.fitted.values$`0.025quant`

write_csv(as.data.frame(All %>%
          arrange(Year) %>%
          mutate(variable = "Average household size among renters") %>%
          rename(GEOID=HRA2010v2_) %>%
          dplyr::select(GEOID, Year, variable, mean, up, low)),
        file = paste0(code_dir,"Report_estimates/average_hh_size_renters_hra.csv"))

plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year %in% c(2000,2010,2020)))
p1 <- ggplot(plot.df) + 
  geom_sf(aes(fill=mean)) +
  ggtitle("Average household size among renters by HRA") +
  labs(fill = "Average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "YlGnBu") +
  facet_wrap(~ Year, nrow = 2)

ggsave(p1, filename = paste0(code_dir, "Report_plots/average_hh_size_renters_2000_2020.png"),
       width = 9, height = 6)

## Difference in 4-person hh prevalence over time
plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2000) %>%
              rename(prev2000=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2000) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2000
              ))

p2 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size among renters by HRA 2000-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_average_hh_size_renters_2000_2020.png"),
       width = 9, height = 6)

plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2010) %>%
              rename(prev2010=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2010) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2010
              ))

p3 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size among renters by HRA 2010-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p3, filename = paste0(code_dir, "Report_plots/Difference_in_average_hh_size_renters_2010-2019.png"),
       width = 9, height = 6)

################################
## Subsetting to Owners only ##
################################

All2 <- hraDF %>%
  filter(tenure == "Owner" & !is.na(HRA2010v2_))
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All2$HRA2010v2_),Year=c(2000:2020))

All2<-All2%>%right_join(grid)
dim(All2)
All2$period.id2<-All2$period.id<-as.numeric(as.factor(All2$Year))
All2$dist.id2<-All2$dist.id<-as.numeric(as.factor(All2$HRA2010v2_))

pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)

mod2 <- inla(av.hh.size ~ 
              f(period.id, model = "ar1", constr = TRUE,
                param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All2,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1), 
            control.inla = list(strategy = "adaptive", int.strategy = "auto"), 
            verbose = FALSE)

summary(mod2)

All2$mean<-mod2$summary.fitted.values$`mean`
All2$up<-mod2$summary.fitted.values$`0.975quant`
All2$low<-mod2$summary.fitted.values$`0.025quant`


write_csv(as.data.frame(All2 %>%
                          arrange(Year) %>%
                          mutate(variable = "Average household size among owners") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/average_hh_size_owners_hra.csv"))


plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year %in% c(2000,2010,2020)))
p1 <- ggplot(plot.df) + 
  geom_sf(aes(fill=mean)) +
  ggtitle("Average household size among owners by HRA") +
  labs(fill = "Average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "YlGnBu") +
  facet_wrap(~ Year, nrow = 2)

ggsave(p1, filename = paste0(code_dir, "Report_plots/average_hh_size_owners_2000_2020.png"),
       width = 9, height = 6)

## Difference in 4-person hh prevalence over time
plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2000) %>%
              rename(prev2000=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2000) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2000
              ))

p2 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size among owners by HRA 2000-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_average_hh_size_owners_2000_2020.png"),
       width = 9, height = 6)

plot.df <- st_as_sf(hra) %>%
  full_join(All %>%
              filter(Year == 2010) %>%
              rename(prev2010=mean) %>%
              dplyr::select(`HRA2010v2_`, prev2010) %>%
              merge(All %>%
                      filter(Year == 2020)%>%
                      dplyr::select(-Year), by = "HRA2010v2_") %>%
              mutate(
                diff_prev = mean - prev2010
              ))

p3 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_prev)) +
  ggtitle("Difference in average household size among owners by HRA 2010-2020") +
  labs(fill = "Difference average household size", x = "", y = "") +
  # scale_x_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p3, filename = paste0(code_dir, "Report_plots/Difference_in_average_hh_size_owners_2010-2019.png"),
       width = 9, height = 6)

