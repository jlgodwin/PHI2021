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
library(dplyr)
# constants
# main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")
code_dir <- paste0(main_dir, "PHI2021/household_size/")
# loading estimates
hhByhhSizeDF <- readRDS(file = paste0(out_dir, "hh_by_hh_size_and_tenure_ct.RDS"))

source(paste0(main_dir, "PHI2021/Tutorials/map_main_theme.R"))

hhByhhSizeDF <-  hhByhhSizeDF %>%
  filter(hh_size == 0) %>%
  dplyr::select(GEOID, Year, tenure, estimate, moe) %>%
  rename(total = estimate,
         total.moe = moe) %>%
  right_join(hhByhhSizeDF)%>%
  dplyr::select(-variable) %>%
  filter(hh_size != 0)


load(paste0(code_dir, "../Data/tracts_to_hra.rda"))

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

#########################
## 1-person households ##
#########################

hraDF <- cw %>%
  left_join(hhByhhSizeDF) %>%
  filter(hh_size == 1) %>%
  group_by(HRA2010v2_, Year, tenure) %>%
  summarize(
      prev_1 = sum(estimate*prop.area, na.rm = T)/sum(total*prop.area, na.rm = T),
      prev_1.moe = moe_sum(moe))

hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))
nb.r <- poly2nb(hra, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)

hraDF <- hraDF  %>%
  mutate(prev_1.se = ifelse(!is.na(prev_1.moe),prev_1.moe/1.96,0.000001))


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
pc.u = 1; pc.alpha = 0.01; pc.u.phi = 0.5; pc.alpha.phi = 2/3
hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)))
hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)), 
                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi)))
prior.iid <- c(0.5,0.008)
# Add Columns for SAE #
threshold<-0.00001
dat<-hraDF%>%mutate(prev_1 = as.numeric(prev_1),
                  se.prev_1=as.numeric(prev_1.se),
                  logit_prev_1=logit_outcome(prev_1,thresh=threshold),
                  var_logit_prev_1= var_logit_outcome(prev_1,se.prev_1,thresh=threshold),
                  prec = ifelse(!is.na(var_logit_prev_1),
                                1/var_logit_prev_1,
                                20))



All1 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Renter")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All1$HRA2010v2_),Year=c(2000:2020))

All1<-All1%>%right_join(grid)
dim(All1)
All1$period.id2<-All1$period.id<-as.numeric(as.factor(All1$Year))
All1$dist.id2<-All1$dist.id<-as.numeric(as.factor(All1$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_1 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All1,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All1$mean<-expit(mod$summary.fitted.values$`mean`)
All1$up<-expit(mod$summary.fitted.values$`0.975quant`)
All1$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All1 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 1-person household among renters") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_1_person_hh_renters_hra.csv"))

# All1 <- read_csv(paste0(code_dir,"Report_estimates/prevalence_1_person_hh_renters_hra.csv")) %>%
  # rename(HRA2010v2_=GEOID)

####################################################
## Maps of prevalence 1 - person hh over time ##
#####
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2020])-expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All1$HRA2010v2_[All1$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.box.margin=margin(-10,-10,-10,-10))

legend2 <- get_legend(pchange)  

assign("p_change_renters", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All1 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%
  # mutate_at(c("mean","low","up"), function(x) x*100)
  
  colors <- c("#fde725ff","#73d055ff",
              "#3cbb75ff","#4eb3d3",
              "#1f968bff","#2d708eff","#084081",
              "#404788ff","#440154ff")

    
  bins <- c(0, .05, .1,
            .15, .2, .25,
            .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
  paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
  paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
  paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
  paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  # paste0(sprintf(y,bkpt[10]),'-', '<', sprintf(y,bkpt[11])))
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  
  assign(paste0("p_renters_",t), pbest )
  
}

ggsave(grid.arrange(p_renters_2000 + theme(legend.position = 'none'), p_renters_2010, p_renters_2020  + theme(legend.position = 'none'), p_change_renters,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/1-person_hh_prev_renters_2000-2020.png"),
       width = 9, height = 6)


All2 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Owner")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All2$HRA2010v2_),Year=c(2000:2020))

All2<-All2%>%right_join(grid)
dim(All2)
All2$period.id2<-All2$period.id<-as.numeric(as.factor(All2$Year))
All2$dist.id2<-All2$dist.id<-as.numeric(as.factor(All2$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_1 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All2,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All2$mean<-expit(mod$summary.fitted.values$`mean`)
All2$up<-expit(mod$summary.fitted.values$`0.975quant`)
All2$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All2 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 1-person household among Owners") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_1_person_hh_Owners_hra.csv"))

# All2 <- read_csv(paste0(code_dir,"Report_estimates/prevalence_1_person_hh_Owners_hra.csv")) %>%
# rename(HRA2010v2_=GEOID)

####################################################
## Maps of prevalence 1 - person hh over time ##
#####

post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2020])-expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All2$HRA2010v2_[All2$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_owners", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All2 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%
  # mutate_at(c("mean","low","up"), function(x) x*100)
  
  colors <- c("#fde725ff","#73d055ff",
              "#3cbb75ff","#4eb3d3",
              "#1f968bff","#2d708eff","#084081",
              "#404788ff","#440154ff")
  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  assign(paste0("p_owners_",t), pbest )
  
}

ggsave(grid.arrange(p_owners_2000 + theme(legend.position = 'none'), p_owners_2010, p_owners_2020 + theme(legend.position = 'none'), p_change_owners,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/1-person_hh_prev_Owners_2000-2020.png"),
       width = 9, height = 6)


ggsave(grid.arrange(p_renters_2020+ ggtitle("A. Prevalence among renters 2020") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_owners_2020+ ggtitle("C. Prevalence among owners 2020") + theme(plot.title = element_text(size = 12, face = "bold")),
                    p_change_renters+ggtitle("B. Change renters (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_change_owners+ ggtitle("D. Change owners (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")),
                    ncol=2, nrow = 2),#, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/1-person_hh_prev_owners_renters_2020-2020.png"),
       width = 9, height = 6)

#########################
## 2-person households ##
#########################


hraDF <- cw %>%
  left_join(hhByhhSizeDF) %>%
  filter(hh_size == 2) %>%
  group_by(HRA2010v2_, Year, tenure) %>%
  summarize(
    prev_2 = sum(estimate*prop.area, na.rm = T)/sum(total*prop.area, na.rm = T), # population_weighted,
    prev_2.moe = sum(moe)/sum(total.moe))

hraDF <- hraDF  %>%
  mutate(prev_2.se = ifelse(!is.na(prev_2.moe),prev_2.moe/1.96,0.000001))

dat<-hraDF%>%mutate(prev_2 = as.numeric(prev_2),
                    se.prev_2=as.numeric(prev_2.se),
                    logit_prev_2=logit_outcome(prev_2,thresh=threshold),
                    var_logit_prev_2= var_logit_outcome(prev_2,se.prev_2,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prev_2),
                                  1/var_logit_prev_2,
                                  20))

All1 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Renter")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All1$HRA2010v2_),Year=c(2000:2020))

All1<-All1%>%right_join(grid)
dim(All1)
All1$period.id2<-All1$period.id<-as.numeric(as.factor(All1$Year))
All1$dist.id2<-All1$dist.id<-as.numeric(as.factor(All1$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_2 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All1,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All1$mean<-expit(mod$summary.fitted.values$`mean`)
All1$up<-expit(mod$summary.fitted.values$`0.975quant`)
All1$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All1 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 2-person household among renters") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_2-person_hh_renters_hra.csv"))


post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2020])-expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All1$HRA2010v2_[All1$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_renters", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All1 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%
  # mutate_at(c("mean","low","up"), function(x) x*100)
  
  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  
  assign(paste0("p_renters_",t), pbest )
  
}

ggsave(grid.arrange(p_renters_2000, p_renters_2010, p_renters_2020, p_change_renters,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/2-person_hh_prev_renters_2000-2020.png"),
       width = 9, height = 6)


All2 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Owner")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All2$HRA2010v2_),Year=c(2000:2020))

All2<-All2%>%right_join(grid)
dim(All2)
All2$period.id2<-All2$period.id<-as.numeric(as.factor(All2$Year))
All2$dist.id2<-All2$dist.id<-as.numeric(as.factor(All2$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_2 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All2,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All2$mean<-expit(mod$summary.fitted.values$`mean`)
All2$up<-expit(mod$summary.fitted.values$`0.975quant`)
All2$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All2 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 2-person household among Owners") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_2-person_hh_Owners_hra.csv"))



post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2020])-expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All2$HRA2010v2_[All2$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  
assign("p_change_owners", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All2 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%

  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  assign(paste0("p_owners_",t), pbest )
  
}

ggsave(grid.arrange(p_owners_2000 + theme(legend.position = 'none'), p_owners_2010, p_owners_2020 + theme(legend.position = 'none'), p_change_owners,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/2-person_hh_prev_Owners_2000-2020.png"),
       width = 9, height = 6)


ggsave(grid.arrange(p_renters_2020+ ggtitle("A. Prevalence among renters 2020") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_owners_2020+ ggtitle("C. Prevalence among owners 2020") + theme(plot.title = element_text(size = 12, face = "bold")),
                    p_change_renters+ggtitle("B. Change renters (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_change_owners+ ggtitle("D. Change owners (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")),
                    ncol=2, nrow = 2),#, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/2-person_hh_prev_owners_renters_2020-2020.png"),
       width = 9, height = 6)
#########################
## 3-person households ##
#########################

hraDF <- cw %>%
  left_join(hhByhhSizeDF) %>%
  filter(hh_size == 3) %>%
  group_by(HRA2010v2_, Year, tenure) %>%
  summarize(
    prev_3 = sum(estimate*prop.area, na.rm = T)/sum(total*prop.area, na.rm = T), # population_weighted,
    prev_3.moe = sum(moe)/sum(total.moe))

hraDF <- hraDF  %>%
  mutate(prev_3.se = ifelse(!is.na(prev_3.moe),prev_3.moe/1.96,0.000001))

dat<-hraDF%>%mutate(prev_3 = as.numeric(prev_3),
                    se.prev_3=as.numeric(prev_3.se),
                    logit_prev_3=logit_outcome(prev_3,thresh=threshold),
                    var_logit_prev_3= var_logit_outcome(prev_3,se.prev_3,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prev_3),
                                  1/var_logit_prev_3,
                                  20))

All1 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Renter")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All1$HRA2010v2_),Year=c(2000:2020))

All1<-All1%>%right_join(grid)
dim(All1)
All1$period.id2<-All1$period.id<-as.numeric(as.factor(All1$Year))
All1$dist.id2<-All1$dist.id<-as.numeric(as.factor(All1$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_3 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All1,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All1$mean<-expit(mod$summary.fitted.values$`mean`)
All1$up<-expit(mod$summary.fitted.values$`0.975quant`)
All1$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All1 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 3-person household among renters") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_3-person_hh_renters_hra.csv"))


post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2020])-expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All1$HRA2010v2_[All1$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_renters", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All1 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%

  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  assign(paste0("p_renters_",t), pbest )

}

ggsave(grid.arrange(p_renters_2000 + theme(legend.position = 'none'), p_renters_2010, p_renters_2020 + theme(legend.position = 'none'), p_change_renters,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/3-person_hh_prev_renters_2000-2020.png"),
       width = 9, height = 6)

All2 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Owner")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All2$HRA2010v2_),Year=c(2000:2020))

All2<-All2%>%right_join(grid)
dim(All2)
All2$period.id2<-All2$period.id<-as.numeric(as.factor(All2$Year))
All2$dist.id2<-All2$dist.id<-as.numeric(as.factor(All2$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_3 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All2,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All2$mean<-expit(mod$summary.fitted.values$`mean`)
All2$up<-expit(mod$summary.fitted.values$`0.975quant`)
All2$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All2 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 3-person household among Owners") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_3-person_hh_Owners_hra.csv"))


post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2020])-expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All2$HRA2010v2_[All2$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_owners", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All2 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%

  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  assign(paste0("p_owners_",t), pbest )
  
}

ggsave(grid.arrange(p_owners_2000 + theme(legend.position = 'none'), p_owners_2010, p_owners_2020 + theme(legend.position = 'none'), p_change_owners,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/3-person_hh_prev_Owners_2000-2020.png"),
       width = 9, height = 6)


ggsave(grid.arrange(p_renters_2020+ ggtitle("A. Prevalence among renters 2020") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_owners_2020+ ggtitle("C. Prevalence among owners 2020") + theme(plot.title = element_text(size = 12, face = "bold")),
                    p_change_renters+ggtitle("B. Change renters (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_change_owners+ ggtitle("D. Change owners (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")),
                    ncol=2, nrow = 2),#, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/3-person_hh_prev_owners_renters_2020-2020.png"),
       width = 9, height = 6)
#########################
## 4+-person households ##
#########################

hraDF <- cw %>%
  left_join(hhByhhSizeDF) %>%
  filter(hh_size > 3) %>%
  group_by(HRA2010v2_, Year, tenure) %>%
  summarize(
    prev_4 = sum(estimate*prop.area, na.rm = T)/sum(total*prop.area, na.rm = T), # population_weighted,
    prev_4.moe = sum(moe)/sum(total.moe))

hraDF <- hraDF  %>%
  mutate(prev_4.se = ifelse(!is.na(prev_4.moe),prev_4.moe/1.96,0.000001))

dat<-hraDF%>%mutate(prev_4 = as.numeric(prev_4),
                    se.prev_4=as.numeric(prev_4.se),
                    logit_prev_4=logit_outcome(prev_4,thresh=threshold),
                    var_logit_prev_4= var_logit_outcome(prev_4,se.prev_4,thresh=threshold),
                    prec = ifelse(!is.na(var_logit_prev_4),
                                  1/var_logit_prev_4,
                                  20))

All1 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Renter")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All1$HRA2010v2_),Year=c(2000:2020))

All1<-All1%>%right_join(grid)
dim(All1)
All1$period.id2<-All1$period.id<-as.numeric(as.factor(All1$Year))
All1$dist.id2<-All1$dist.id<-as.numeric(as.factor(All1$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_4 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All1,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All1$mean<-expit(mod$summary.fitted.values$`mean`)
All1$up<-expit(mod$summary.fitted.values$`0.975quant`)
All1$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All1 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 4+-person household among renters") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_4+-person_hh_renters_hra.csv"))


post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2020])-expit(post[[jj]]$latent[1:nrow(All1)][All1$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All1$HRA2010v2_[All1$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_renters", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All1 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%

  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  
  assign(paste0("p_renters_",t), pbest )
  
}

ggsave(grid.arrange(p_2000, p_2010, p_2020, p_change,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/4+-person_hh_prev_renters_2000-2020.png"),
       width = 9, height = 6)

All2 <-dat %>%
  filter(!is.na(HRA2010v2_) & tenure == "Owner")
## Modelling trends in average household size 
grid<-expand.grid(HRA2010v2_=unique(All2$HRA2010v2_),Year=c(2000:2020))

All2<-All2%>%right_join(grid)
dim(All2)
All2$period.id2<-All2$period.id<-as.numeric(as.factor(All2$Year))
All2$dist.id2<-All2$dist.id<-as.numeric(as.factor(All2$HRA2010v2_))

prior.iid <- c(0.5,0.008)
mod <- inla(logit_prev_4 ~ f(period.id, model = "ar1", constr = TRUE,
                             param = prior.iid, hyper = hyperpc1) +
              f(period.id2, model = "iid", hyper = hyperpc1) +
              f(dist.id, model = "bym2", graph = mat, hyper = hyperpc2,
                scale.model = TRUE, adjust.for.con.comp = TRUE) +
              f(dist.id2, model = "iid", param = prior.iid), 
            scale = prec,
            data =All2,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE, link = 1))

summary(mod)


All2$mean<-expit(mod$summary.fitted.values$`mean`)
All2$up<-expit(mod$summary.fitted.values$`0.975quant`)
All2$low<-expit(mod$summary.fitted.values$`0.025quant`)

write_csv(as.data.frame(All2 %>%
                          arrange(Year) %>%
                          mutate(variable = "Prevalence of 4+-person household among Owners") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/prevalence_4+-person_hh_Owners_hra.csv"))

# All2 <- read_csv(paste0(code_dir,"Report_estimates/prevalence_4+-person_hh_Owners_hra.csv")) %>%
# rename(HRA2010v2_=GEOID)

####################################################
## Maps of prevalence 1 - person hh over time ##
#####

post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2020])-expit(post[[jj]]$latent[1:nrow(All2)][All2$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All2$HRA2010v2_[All2$Year==2020],
                       mdn = apply(sampsDiff, 1, median), 
                       lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                       hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
pchange <- st_as_sf(hra) %>%
  full_join(changeDF) %>%
  ggplot() +
  geom_sf(aes(fill=mdn)) +
  geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
  ggtitle("D. Change (2000-2020)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill= "") +
  map_theme_main+
  scale_fill_gradient2()+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.height= unit(.4, 'cm'),
        legend.key.width= unit(.4, 'cm'),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5))

legend2 <- get_legend(pchange)  

assign("p_change_owners", pchange + theme(legend.position = 'right'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All2 %>%
    filter(Year == t) %>%
    mutate_at(c("mean","low","up"), function(x) x*100)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%

  
  bins <- c(0, .05, .1,             .15, .2, .25,             .4, .5, .6, .75)*100
  y <- '%.1f'
  val <- as.numeric(gsub("\\$|,", "", plot.df$mean))
  lo <- as.numeric(gsub("\\$|,", "", plot.df$low))
  hi <- as.numeric(gsub("\\$|,", "", plot.df$up))
  bkpt <- as.numeric(bins)
  labels <- gsub("(?<!^)(\\d{3})$", ",\\1", bkpt, perl=T)
  lab_break1 <- c(#paste0(paste0('<',sprintf(y,bkpt[2]))),
    paste0(sprintf(y,bkpt[1]),'-', '<', sprintf(y,bkpt[2])),
    paste0(sprintf(y,bkpt[2]),'-', '<', sprintf(y,bkpt[3])),
    paste0(sprintf(y,bkpt[3]),'-', '<', sprintf(y,bkpt[4])),
    paste0(sprintf(y,bkpt[4]),'-', '<', sprintf(y,bkpt[5])),
    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])),
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])),
    paste0(sprintf(y,bkpt[7]),'-', '<', sprintf(y,bkpt[8])),
    paste0(sprintf(y,bkpt[8]),'-', '<', sprintf(y,bkpt[9])),
    paste0(sprintf(y,bkpt[9]),'-', '<', sprintf(y,bkpt[10])))#,
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Prevalence")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.key.height= unit(.4, 'cm'),
          legend.key.width= unit(.4, 'cm'),
          legend.text = element_text(size=5),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5)) +
    guides(fill = guide_legend(override.aes = list(size = .5)))
  
  
  assign(paste0("p_owners_",t), pbest )
}

ggsave(grid.arrange(p_owners_2000 + theme(legend.position = 'none'), p_owners_2010, p_owners_2020 + theme(legend.position = 'none'), p_change_owners,
                    ncol=2, nrow = 2, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/4+-person_hh_prev_Owners_2000-2020.png"),
       width = 9, height = 6)


ggsave(grid.arrange(p_renters_2020+ ggtitle("A. Prevalence among renters 2020") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_owners_2020+ ggtitle("C. Prevalence among owners 2020") + theme(plot.title = element_text(size = 12, face = "bold")),
                    p_change_renters+ggtitle("B. Change renters (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")), 
                    p_change_owners+ ggtitle("D. Change owners (2000-2020)") + theme(plot.title = element_text(size = 12, face = "bold")),
                    ncol=2, nrow = 2),#, widths = c(2/5,3/5)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/4+-person_hh_prev_owners_renters_2020-2020.png"),
       width = 9, height = 6)
