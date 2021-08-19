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
          )) %>%
  filter(tenure != "Total")

totDF <- popDF %>%
    left_join(avgHHsizeDF)

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

# test <- cw %>% 
#   subset(HRA2010v2_ == "Auburn-North" & Year == 2009) %>%
#   dplyr::select(prop.area, GEOID)
# 
# test %>%
#   left_join(totDF) %>%
#   group_by(Year, source, tenure) %>%
#   summarize(
#     av.hh.size = sum(hhs*hhp*prop.area, na.rm = T)/sum(hhp*prop.area, na.rm = T)
#   )

hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))
nb.r <- poly2nb(hra, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)

hraDF <- hraDF  %>%
  mutate(hhs.se = ifelse(!is.na(av.hh.size.moe),av.hh.size.moe/1.96,0.000001),
         prec = 1/((hhs.se)^2))


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

#######################################
## Maps of average hh size over time ##
#######################################

png(paste0(out_dir,"Average_hh_size_renter_Map_2019.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2019]

brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

png(paste0(out_dir,"Average_hh_size_renter_Map_2000.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2000]

brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

png(paste0(out_dir,"Average_hh_size_renter_Map_2010.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All$mean[All$Year==2010]

brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

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

#######################################
## Maps of average hh size over time ##
#######################################

png(paste0(out_dir,"Average_hh_size_Owner_Map_2019.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All2$mean[All2$Year==2019]

brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

png(paste0(out_dir,"Average_hh_size_Owner_Map_2000.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All2$mean[All2$Year==2000]
brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

png(paste0(out_dir,"Average_hh_size_Owner_Map_2010.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = All2$mean[All2$Year==2010]

brks=seq(1.5,3.5,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(1.5,3.5,1),""),
             align="rb",cex=.7)

dev.off()

###################################################
## Maps of differences in average size by tenure ##
###################################################

post <- inla.posterior.sample(100,mod)
post2 <- inla.posterior.sample(100,mod2)

samps <- matrix(nrow=length(unique(All$HRA2010v2_)),ncol=100)
sampsDiff<-matrix(nrow=length(unique(All$HRA2010v2_)),ncol=100)
for(jj in 1:100){
  # jj<-1
  samps[,jj] <- post2[[jj]]$latent[1:nrow(All)][All$Year==2020]
  sampsDiff[,jj]<-post[[jj]]$latent[1:nrow(All)][All$Year==2020]-post2[[jj]]$latent[1:nrow(All2)][All2$Year==2020]
  
}

apply(sampsDiff, 1, median)

png(paste0(out_dir,"Median_diff_hh_size_Owner_Renter_Map_2020.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = -apply(sampsDiff, 1, median)

brks=seq(0,.5,length=6)
nclr<-length(brks)-1

plotclr<-rev(brewer.pal(nclr,"RdYlBu"))
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(hra,border="black",lwd=0.5,col=colcode)

color.legend(1500000,120000,1550000,150000, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,.5,length=6),""),
             align="rb",cex=.7)

dev.off()

