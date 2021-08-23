
rm(list=ls())
library(ggplot2)
library(INLA)
# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")

# loading functions
source(paste0(code_dir,"../Tutorials/ipums_acs_recode.R"))
source(paste0(code_dir,"../Tutorials/compute_survey_proportions.R"))
source(paste0(main_dir,"PHI2021/Tutorials/expit_logit.R"))

dat<-read_csv(paste0(out_dir,"All_KC_hh_size.csv")) %>%
  filter(survey != "acs5") %>%
  mutate(sampling = as.numeric(as.factor(survey)))

################################################################################
################################################################################
############ Loop through the subsets of data and select a model ###############
################################################################################
################################################################################

indicators<-c("hh_size","logit_prevalence.1_person_hh","logit_prevalence.2_person_hh",
              "logit_prevalence.3_person_hh","logit_prevalence.4_person_hh",
              "logit_prevalence.5_person_hh","logit_prevalence.6_person_hh",
              "logit_prevalence.7_person_hh")

hh_type<-c("ALL","family","non-family")
tenure<-c("ALL","owner","renter")

groups<-expand.grid(outcome=indicators,hh_type=hh_type, tenure = tenure)
groups_all<-data.frame(outcome=indicators,hh_type="ALL",tenure="ALL")#),mga="ALL"

results<-rbind(groups_all,groups)
results$var<-paste0("var_",results$outcome)

#######################
# -- cycle through -- #
out.pred <- NULL
for(j in 1:nrow(results)){
  print(paste0(j," of ",nrow(results),
               " outcomes and hh_type groups. ",Sys.time()))
  All<-filter(dat, hh_type==results$hh_type[j] & tenure==results$tenure[j])
  
  grid<-expand.grid(year=c(1980:2020))
  dim(grid)
  dim(All)
  
  All<-All%>%right_join(grid)
  dim(All)
  
  # creating some variables for the random effects #
  All$period.id3<-All$period.id2<-All$period.id<-as.numeric(as.factor(All$year))
  
  All<-arrange(All,year)
  
  pred.all<-unique(All[,c("sampling","period.id","period.id2","period.id3","year")])#
  pred.all<-pred.all%>%mutate(outcome=NA, prec=NA,preds=1,sampling = NA)
  
  
  All$outcome<-All[,as.character(results$outcome[j])]
  All$prec<-1/All[,as.character(results$var[j])]
  All$preds<-0
  
  # a dataset for predictions #
  row.names(All)<-NULL
  row.names(pred.all)<-NULL
  
  tmp<-All[,names(pred.all)]
  tmp$outcome<-as.numeric(unlist(tmp$outcome))
  tmp$prec<-as.numeric(unlist(tmp$prec))
  mod_dat<-rbind(tmp,pred.all)
  mod_dat<-mod_dat%>%mutate(outcome=ifelse(is.na(prec),NA,outcome),
                            prec=ifelse(is.na(outcome),NA,prec))
  
  
  ##################################
  #### -- Set some priors --- ######
  ##################################
  
  prior.iid = c(0.5,0.008)
  prior.besag = c(0.5,0.008)
  
  
  ############################
  # -- set up some models -- #

  model <- outcome ~ f(period.id, model="rw1",  param=prior.iid) +  # random walk 
      f(period.id2, model="iid",  param=prior.iid) +  # iid time
      f(sampling, model="iid", param=prior.iid)
  #######################################
  # -- fit the model for the outcomes-- #
  #######################################

  mod <- inla(model, 
              family = "gaussian", 
              data =mod_dat, 
              control.predictor=list(compute=TRUE),
              control.compute=list(cpo=TRUE,waic=T,config=T),
              control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
              scale=prec)
  
  ###########################################
  # --- add predictions to the data set --- #
  ###########################################
  
  if (j == 1) {
    mod_dat$mean<-mod$summary.fitted.values$`mean`
    mod_dat$up<-mod$summary.fitted.values$`0.975quant`
    mod_dat$low<-mod$summary.fitted.values$`0.025quant`
  } else{
    mod_dat$mean<-expit(mod$summary.fitted.values$`mean`)
    mod_dat$up<-expit(mod$summary.fitted.values$`0.975quant`)
    mod_dat$low<-expit(mod$summary.fitted.values$`0.025quant`)
  }

  
  loc<-paste0(substr(results$outcome[j],7,30),"_type_",results$hh_type[j],
              "_tenure_",results$tenure[j])#"_mga_",results$mga[j],

  ###############################
  # -- save the model output -- #
  ###############################
  pred<-filter(mod_dat,preds==1) %>%
    dplyr::select(year,up,low,mean) %>%
    mutate(indicator = results$outcome[j],
           hh_type = results$hh_type[j],
           tenure = results$tenure[j])
  
  #
  out.pred <- rbind(out.pred, pred)
  save(mod,mod_dat,All,file=paste0(out_dir,"ModelFits/",loc,"_model.RDATA"))
}
saveRDS(out.pred,file=paste0(out_dir,"Preds/_preds.rds"))
