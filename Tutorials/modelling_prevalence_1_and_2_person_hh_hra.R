########################################################
###
### Modelling hh by size at the HRA level and by tenure status
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
hhByhhSizeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_tenure_ct.RDS"))

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
      )) %>%
  dplyr::select(-variable) %>%
  filter(tenure != "Total")

totDF <- popDF %>%
  left_join(hhByhhSizeDF) %>%
  filter(!is.na(hh_size))

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
  filter(hh_size %in% 1:2) %>%
  group_by(HRA2010v2_, Year, source, tenure) %>%
  summarize(
    prev_1_2 = sum(prop*hhp, na.rm = T)/sum(hhp, na.rm = T), # population_weighted,
    # prev_1_2.moe = moe_ratio(sum(prop*hhp),
    #                            sum(prop),
    #                            moe_sum(moe = moe_product(prop,
    #                                                      hhp,
    #                                                      prop.moe,
    #                                                      hhp.moe)),
    #                            moe_sum(moe = hhp.moe))
    prev_1_2.moe = moe_sum(prop.moe)
  )

hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))
hraDF <- hraDF  %>%
  mutate(prev_1_2.se = ifelse(!is.na(prev_1_2.moe),prev_1_2.moe/1.96,0.000001))


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
dat<-hraDF%>%mutate(prev_1_2 = as.numeric(prev_1_2),
                  se.prev_1_2=as.numeric(prev_1_2.se),
                  logit_prev_1_2=logit_outcome(prev_1_2,thresh=threshold),
                  var_logit_prev_1_2= var_logit_outcome(prev_1_2,se.prev_1_2,thresh=threshold),
                  prec = ifelse(!is.na(var_logit_prev_1_2),
                                1/var_logit_prev_1_2,
                                20))


##################
## Renters only ##
##################

All <-dat %>%
  filter(tenure == "Renter" & !is.na(HRA2010v2_))
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All$HRA2010v2_),Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id<-as.numeric(as.factor(All$Year))
All$dist.id<-as.numeric(as.factor(All$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_1_2 ~ 
              f(period.id, model = "rw1", param = prior.iid) +
              f(dist.id, model = "iid"), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)

####################################################
## Maps of prevalence 1 and 2 person hh over time ##
####################################################

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Renter_Map_2019.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2019]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Renter_Map_2010.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2010]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Renter_Map_2000.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2000]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()


##################
## Owners only ##
##################

All <-dat %>%
  filter(tenure == "Owner" & !is.na(HRA2010v2_))
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All$HRA2010v2_),Year=c(2000:2020))

All<-All%>%right_join(grid)
dim(All)
All$period.id<-as.numeric(as.factor(All$Year))
All$dist.id<-as.numeric(as.factor(All$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_1_2 ~ 
              f(period.id, model = "rw1", param = prior.iid) +
              f(dist.id, model = "iid"), 
            scale = prec,
            data =All,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All$mean<-expit(mod$summary.fitted.values$`mean`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)

####################################################
## Maps of prevalence 1 and 2 person hh over time ##
####################################################

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Owner_Map_2019.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2019]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Owner_Map_2010.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2010]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()

png(paste0(out_dir,"Prevalence_1_2_pers_hh_Owner_Map_2000.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2000]

brks=seq(.1,.3,length=5)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(.1,.3,length=5),""),
             align="rb",cex=.5)

dev.off()

