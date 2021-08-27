#######################################################
### Merging the ACS IPUMS Surveys

### this file will read in the most recent 
### versions of these surveys
### and save with the date.

rm(list=ls())

#####################################
# -- load packages and functions -- #
#####################################

# function
source(paste0(main_dir,"PHI2021/Tutorials/expit_logit.R"))
# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")


processed<-list.files(out_dir)[grep("Processed",list.files(out_dir))]
processed_full<-processed

# combine the DHS
dat<-read_csv(paste0(out_dir,processed[1]))

for(i in 2:length(processed)){
  # i<-8
  tmp<-read_csv(paste0(out_dir,processed[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}

colnames(dat)[c(3,5,7,9,11,13,15)] <- c("prevalence.7_person_hh",
                                     "prevalence.6_person_hh",
                                     "prevalence.5_person_hh",
                                     "prevalence.4_person_hh",
                                     "prevalence.3_person_hh",
                                     "prevalence.2_person_hh",
                                     "prevalence.1_person_hh")
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


# Add Columns for SAE #
threshold<-0.00001
dat<-dat%>%mutate(hh_size = as.numeric(hh_size),
                  se.average_hh_size=as.numeric(se.average_hh_size),
                  var_hh_size= se.average_hh_size^2,
                  prevalence.7_person_hh = as.numeric(prevalence.7_person_hh),
                  se.prevalence.7_person_hh=as.numeric(se.prevalence.7_person_hh),
                  logit_prevalence.7_person_hh=logit_outcome(prevalence.7_person_hh,thresh=threshold),
                  var_logit_prevalence.7_person_hh= var_logit_outcome(prevalence.7_person_hh,se.prevalence.7_person_hh,thresh=threshold),
                  prevalence.6_person_hh = as.numeric(prevalence.6_person_hh),
                  se.prevalence.6_person_hh=as.numeric(se.prevalence.6_person_hh),
                  logit_prevalence.6_person_hh=logit_outcome(prevalence.6_person_hh,thresh=threshold),
                  var_logit_prevalence.6_person_hh= var_logit_outcome(prevalence.6_person_hh,se.prevalence.6_person_hh,thresh=threshold),
                  prevalence.5_person_hh = as.numeric(prevalence.5_person_hh),
                  se.prevalence.5_person_hh=as.numeric(se.prevalence.5_person_hh),
                  logit_prevalence.5_person_hh=logit_outcome(prevalence.5_person_hh,thresh=threshold),
                  var_logit_prevalence.5_person_hh= var_logit_outcome(prevalence.5_person_hh,se.prevalence.5_person_hh,thresh=threshold),
                  prevalence.4_person_hh = as.numeric(prevalence.4_person_hh),
                  se.prevalence.4_person_hh=as.numeric(se.prevalence.4_person_hh),
                  logit_prevalence.4_person_hh=logit_outcome(prevalence.4_person_hh,thresh=threshold),
                  var_logit_prevalence.4_person_hh= var_logit_outcome(prevalence.4_person_hh,se.prevalence.4_person_hh,thresh=threshold),
                  prevalence.3_person_hh = as.numeric(prevalence.3_person_hh),
                  se.prevalence.3_person_hh=as.numeric(se.prevalence.3_person_hh),
                  logit_prevalence.3_person_hh=logit_outcome(prevalence.3_person_hh,thresh=threshold),
                  var_logit_prevalence.3_person_hh= var_logit_outcome(prevalence.3_person_hh,se.prevalence.3_person_hh,thresh=threshold),
                  prevalence.2_person_hh = as.numeric(prevalence.2_person_hh),
                  se.prevalence.2_person_hh=as.numeric(se.prevalence.2_person_hh),
                  logit_prevalence.2_person_hh=logit_outcome(prevalence.2_person_hh,thresh=threshold),
                  var_logit_prevalence.2_person_hh= var_logit_outcome(prevalence.2_person_hh,se.prevalence.2_person_hh,thresh=threshold),
                  prevalence.1_person_hh = as.numeric(prevalence.1_person_hh),
                  se.prevalence.1_person_hh=as.numeric(se.prevalence.1_person_hh),
                  logit_prevalence.1_person_hh=logit_outcome(prevalence.1_person_hh,thresh=threshold),
                  var_logit_prevalence.1_person_hh= var_logit_outcome(prevalence.1_person_hh,se.prevalence.1_person_hh,thresh=threshold)
                  )

###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(out_dir,"All_KC_hh_size.csv"))

total<-list.files(out_dir)[grep("Total",list.files(out_dir))]
total_full<-total

# combine the DHS
dat<-read_csv(paste0(out_dir,total[1]))

for(i in 2:length(total)){
  # i<-8
  tmp<-read_csv(paste0(out_dir,total[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}

colnames(dat)[c(3,5,7,9,11,13,15)] <- c("prevalence.7_person_hh",
                                        "prevalence.6_person_hh",
                                        "prevalence.5_person_hh",
                                        "prevalence.4_person_hh",
                                        "prevalence.3_person_hh",
                                        "prevalence.2_person_hh",
                                        "prevalence.1_person_hh")

write_csv(dat,paste0(out_dir,"All_KC_hh_size_total.csv"))
