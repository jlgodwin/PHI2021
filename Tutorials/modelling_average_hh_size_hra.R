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
library(viridis)
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

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
expit <- function(x) (exp(x)/(1+exp(x)))

  ##########################
  # -- Plot the results -- #
  ##########################
  
  post<-inla.posterior.sample(1000,mod)
  names(post)
  
  sampsDiff<-matrix(nrow=48,ncol=1000)
  samps<-matrix(nrow=48,ncol=1000)
  for(jj in 1:1000){
    # jj<-1
    sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All)][All$Year==2020])-expit(post[[jj]]$latent[1:nrow(All)][All$Year==2010]))
  }
  
  
  changeDF <- data.frame(HRA2010v2_ = All$HRA2010v2_[All$Year==2020],
                         mdn = apply(sampsDiff, 1, median), 
                         lo = apply(sampsDiff, 1, quantile, probs = c(.025)),
                         hi = apply(sampsDiff, 1, quantile, probs = c(.975)))
  pchange <- st_as_sf(hra) %>%
    full_join(changeDF) %>%
    ggplot() +
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle("D. Change (2010-2020)") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill= "Change in average household size") +
    map_theme_main+
    scale_fill_gradient2()+
    theme(legend.title = element_text(size = 12, face = "bold"))
  
  legend2 <- get_legend(pchange)  
  
  assign("p_change", pchange + theme(legend.position = 'none'))
  
  for (t in c(2010,2015,2020)) {
    
    tmp <- All %>%
      filter(Year == t)
    plot.df <- st_as_sf(hra) %>%
      full_join(tmp) #%>%
      # mutate_at(c("mean","low","up"), function(x) x*100)
    
    colors <- c("#fde725ff",#"#73d055ff",
                "#3cbb75ff",#"#4eb3d3",
                "#1f968bff","#2d708eff",#"#084081",
                "#404788ff","#440154ff")
    
    bins <- seq(1.5,4,.5)
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
                    paste0(sprintf(y,bkpt[5]),'-', '<', sprintf(y,bkpt[6])))#,
                    # paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])))
    plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
    plot.df$lo <- cut(lo, bkpt, lab_break1)
    plot.df$hi <- cut(hi, bkpt, lab_break1)
    pbest <- ggplot(plot.df) + 
      geom_sf(aes(fill=mdn)) +
      geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
      ggtitle(paste0(c("A","B","C")[which(t==c(2010,2015,2020))],". ",
                     c("Average size")," in ", t)) +
      scale_fill_manual("",values = colors, drop =FALSE) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(fill = "Average household size") +
      # color = "black") +
      map_theme_main + theme(legend.position = 'right') +
      theme(legend.title = element_text(size = 12, face = "bold"))
    if(t == 2020) {legend1 <- get_legend(pbest)}
    
    assign(paste0("p_",t), pbest + theme(legend.position = 'none'))
    
  }
  
  ggsave(grid.arrange(p_2010, p_2015, legend1, p_2020, p_change, legend2, 
                      ncol=3, nrow = 2, widths = c(3/7,3/7,1/7)), 
         # layout_matrix = rbind(c(1,2), c(3,3)),
         # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
         filename= paste0(code_dir, "Report_plots/average_hh_size_ALL_2010_2020.png"),
         width = 9, height = 6)

write_csv(as.data.frame(All %>%
                          arrange(Year) %>%
                          mutate(variable = "Average household size") %>%
                          rename(GEOID=HRA2010v2_) %>%
                          dplyr::select(GEOID, Year, variable, mean, up, low)),
          file = paste0(code_dir,"Report_estimates/average_hh_size_ALL_hra.csv"))

# plot.df <- st_as_sf(hra) %>%
#   full_join(All %>%
#               filter(Year %in% c(2000,2010,2020)))
# p1 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=mean)) +
#   ggtitle("Average household size by HRA") +
#   labs(fill = "Average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "YlGnBu") +
#   facet_wrap(~ Year, nrow = 2)
# 
# ggsave(p1, filename = paste0(code_dir, "Report_plots/average_hh_size_ALL_2000_2020.png"),
#        width = 9, height = 6)


# p2 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=diff_prev)) +
#   ggtitle("Difference in average household size by HRA 2000-2020") +
#   labs(fill = "Difference average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "RdBu")
# 
# ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_average_hh_size_ALL_2000_2020.png"),
#        width = 9, height = 6)

# plot.df <- st_as_sf(hra) %>%
#   full_join(All %>%
#               filter(Year == 2010) %>%
#               rename(prev2010=mean) %>%
#               dplyr::select(`HRA2010v2_`, prev2010) %>%
#               merge(All %>%
#                       filter(Year == 2020)%>%
#                       dplyr::select(-Year), by = "HRA2010v2_") %>%
#               mutate(
#                 diff_prev = mean - prev2010
#               ))
# 
# p3 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=diff_prev)) +
#   ggtitle("Difference in average household size by HRA 2010-2020") +
#   labs(fill = "Difference average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "RdBu")
# 
# ggsave(p3, filename = paste0(code_dir, "Report_plots/Difference_in_average_hh_size_ALL_2010-2019.png"),
#        width = 9, height = 6)

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

##########################
# -- Plot the results -- #
##########################

post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=48,ncol=1000)
samps<-matrix(nrow=48,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-100*(expit(post[[jj]]$latent[1:nrow(All)][All$Year==2020])-expit(post[[jj]]$latent[1:nrow(All)][All$Year==2000]))
}


changeDF <- data.frame(HRA2010v2_ = All$HRA2010v2_[All$Year==2020],
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
  theme(legend.title = element_text(size = 12, face = "bold"))

legend2 <- get_legend(pchange)  

assign("p_change", pchange + theme(legend.position = 'none'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All %>%
    filter(Year == t)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%
  # mutate_at(c("mean","low","up"), function(x) x*100)
  
  colors <- c("#fde725ff","#73d055ff",
              "#3cbb75ff",#"#4eb3d3",
              "#1f968bff","#2d708eff",#"#084081",
              "#404788ff","#440154ff")
  
  bins <- seq(1,4,.5)
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
  paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])))
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Average size")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"))
  if(t == 2020) {legend1 <- get_legend(pbest)}
  
  assign(paste0("p_",t), pbest + theme(legend.position = 'none'))
  
}

ggsave(grid.arrange(p_2000, p_2010, legend1, p_2020, p_change, legend2, 
                    ncol=3, nrow = 2, widths = c(2/5,3/7,1/7)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/average_hh_size_renters_2000_2020.png"),
       width = 9, height = 6)

# plot.df <- st_as_sf(hra) %>%
#   full_join(All %>%
#               filter(Year %in% c(2000,2010,2020)))
# p1 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=mean)) +
#   ggtitle("Average household size among renters by HRA") +
#   labs(fill = "Average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "YlGnBu") +
#   facet_wrap(~ Year, nrow = 2)
# 
# ggsave(p1, filename = paste0(code_dir, "Report_plots/average_hh_size_renters_2000_2020.png"),
#        width = 9, height = 6)
# 
# plot.df <- st_as_sf(hra) %>%
#   full_join(All %>%
#               filter(Year == 2000) %>%
#               rename(prev2000=mean) %>%
#               dplyr::select(`HRA2010v2_`, prev2000) %>%
#               merge(All %>%
#                       filter(Year == 2020)%>%
#                       dplyr::select(-Year), by = "HRA2010v2_") %>%
#               mutate(
#                 diff_prev = mean - prev2000
#               ))
# 
# p2 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=diff_prev)) +
#   ggtitle("Difference in average household size among renters by HRA 2000-2020") +
#   labs(fill = "Difference average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "RdBu")
# 
# ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_average_hh_size_renters_2000_2020.png"),
#        width = 9, height = 6)
# 
# plot.df <- st_as_sf(hra) %>%
#   full_join(All %>%
#               filter(Year == 2010) %>%
#               rename(prev2010=mean) %>%
#               dplyr::select(`HRA2010v2_`, prev2010) %>%
#               merge(All %>%
#                       filter(Year == 2020)%>%
#                       dplyr::select(-Year), by = "HRA2010v2_") %>%
#               mutate(
#                 diff_prev = mean - prev2010
#               ))
# 
# p3 <- ggplot(plot.df) + 
#   geom_sf(aes(fill=diff_prev)) +
#   ggtitle("Difference in average household size among renters by HRA 2010-2020") +
#   labs(fill = "Difference average household size", x = "", y = "") +
#   # scale_x_discrete(expand = c(0, 0)) +
#   # scale_y_discrete(expand = c(0, 0)) +
#   map_theme_main + 
#   scale_fill_distiller(palette = "RdBu")
# 
# ggsave(p3, filename = paste0(code_dir, "Report_plots/Difference_in_average_hh_size_renters_2010-2019.png"),
#        width = 9, height = 6)

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


post<-inla.posterior.sample(1000,mod2)
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
  theme(legend.title = element_text(size = 12, face = "bold"))

legend2 <- get_legend(pchange)  

assign("p_change", pchange + theme(legend.position = 'none'))

for (t in c(2000,2010,2020)) {
  
  tmp <- All2 %>%
    filter(Year == t)
  plot.df <- st_as_sf(hra) %>%
    full_join(tmp) #%>%
  # mutate_at(c("mean","low","up"), function(x) x*100)
  
  colors <- c("#fde725ff","#73d055ff",
              "#3cbb75ff",#"#4eb3d3",
              "#1f968bff","#2d708eff",#"#084081",
              "#404788ff","#440154ff")
  
  bins <- seq(1,4,.5)
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
    paste0(sprintf(y,bkpt[6]),'-', '<', sprintf(y,bkpt[7])))
  plot.df$mdn <- cut(x = val, breaks = bkpt, labels = lab_break1)
  plot.df$lo <- cut(lo, bkpt, lab_break1)
  plot.df$hi <- cut(hi, bkpt, lab_break1)
  pbest <- ggplot(plot.df) + 
    geom_sf(aes(fill=mdn)) +
    geom_sf(data = st_as_sf(hra), color = "black", fill = "transparent", size = 1) +
    ggtitle(paste0(c("A","B","C")[which(t==c(2000,2010,2020))],". ",
                   c("Average size")," in ", t)) +
    scale_fill_manual("",values = colors, drop =FALSE) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "") +
    # color = "black") +
    map_theme_main + theme(legend.position = 'right') +
    theme(legend.title = element_text(size = 12, face = "bold"))
  if(t == 2020) {legend1 <- get_legend(pbest)}
  
  assign(paste0("p_",t), pbest + theme(legend.position = 'none'))
  
}

ggsave(grid.arrange(p_2000, p_2010, legend1, p_2020, p_change, legend2, 
                    ncol=3, nrow = 2, widths = c(2/5,3/7,1/7)), 
       # layout_matrix = rbind(c(1,2), c(3,3)),
       # widths = c(2.7, 2.7), heights = c(2.5, 0.5)),
       filename= paste0(code_dir, "Report_plots/average_hh_size_owners_2000_2020.png"),
       width = 9, height = 6)

